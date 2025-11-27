import {
  ActionPanel,
  Action,
  Icon,
  List,
  getPreferenceValues,
  Color,
} from "@raycast/api";
import { useCachedPromise } from "@raycast/utils";
import { useState, useMemo } from "react";
import { LunchMoneyApi, LMTransaction } from "lunchmoney-tools";

type Transaction = LMTransaction;

interface Preferences {
  apiKey: string;
}

interface CategoryTotal {
  name: string;
  total: number;
  count: number;
  percentage: number;
  transactions: Transaction[];
}

function generateMonthOptions() {
  const now = new Date();
  const currentYear = now.getFullYear();
  const currentMonth = now.getMonth(); // 0-11

  const months = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
  ];

  const options: { value: string; title: string }[] = [];

  // Start from current month and work backwards through the year
  for (let i = currentMonth; i >= 0; i--) {
    const monthName = months[i];
    const value = `${currentYear}-${String(i + 1).padStart(2, "0")}`;
    options.push({
      value,
      title: `${monthName} ${currentYear}`,
    });
  }

  return options;
}

function getDateRange(monthValue: string) {
  const [year, month] = monthValue.split("-").map(Number);

  // First day of the month
  const start = new Date(year, month - 1, 1);

  // Last day of the month
  const end = new Date(year, month, 0);

  return {
    start: start.toISOString().split("T")[0],
    end: end.toISOString().split("T")[0],
  };
}

function calculateCategoryTotals(transactions: Transaction[]): CategoryTotal[] {
  const categoryMap = new Map<
    string,
    { total: number; count: number; transactions: Transaction[] }
  >();
  let grandTotal = 0;

  // Only include expenses (positive amounts in Lunch Money)
  const expenses = transactions.filter((t) => {
    const amount =
      typeof t.amount === "string" ? parseFloat(t.amount) : t.amount;
    return amount > 0;
  });

  expenses.forEach((transaction) => {
    const amount =
      typeof transaction.amount === "string"
        ? parseFloat(transaction.amount)
        : transaction.amount;
    const categoryName = transaction.category_name || "Uncategorized";

    grandTotal += amount;

    const existing = categoryMap.get(categoryName) || {
      total: 0,
      count: 0,
      transactions: [],
    };
    categoryMap.set(categoryName, {
      total: existing.total + amount,
      count: existing.count + 1,
      transactions: [...existing.transactions, transaction],
    });
  });

  const totals: CategoryTotal[] = Array.from(categoryMap.entries())
    .map(([name, data]) => ({
      name,
      total: data.total,
      count: data.count,
      percentage: grandTotal > 0 ? (data.total / grandTotal) * 100 : 0,
      transactions: data.transactions.sort(
        (a, b) => new Date(b.date).getTime() - new Date(a.date).getTime()
      ),
    }))
    .sort((a, b) => b.total - a.total);

  return totals;
}

function CategoryTransactionsList({
  category,
  onBack,
}: {
  category: CategoryTotal;
  onBack: () => void;
}) {
  const formattedTotal = new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
  }).format(category.total);

  return (
    <List
      navigationTitle={`${category.name} - ${formattedTotal}`}
      searchBarPlaceholder="Search transactions..."
    >
      {category.transactions.map((transaction) => {
        const amount =
          typeof transaction.amount === "string"
            ? parseFloat(transaction.amount)
            : transaction.amount;
        const formattedAmount = new Intl.NumberFormat("en-US", {
          style: "currency",
          currency: "USD",
        }).format(amount);

        return (
          <List.Item
            key={transaction.id}
            icon={Icon.ArrowDown}
            title={transaction.payee || "Unknown"}
            subtitle={transaction.date}
            accessories={[{ text: formattedAmount }]}
            actions={
              <ActionPanel>
                <Action
                  title="Back to Categories"
                  icon={Icon.ArrowLeft}
                  onAction={onBack}
                />
                <Action.OpenInBrowser
                  title="Open in Lunch Money"
                  url={`https://my.lunchmoney.app/transactions/${transaction.id}`}
                  shortcut={{ modifiers: ["cmd"], key: "o" }}
                />
                <Action.CopyToClipboard
                  content={`${transaction.payee} - ${formattedAmount}`}
                  title="Copy Transaction"
                />
              </ActionPanel>
            }
          />
        );
      })}
    </List>
  );
}

export default function Command() {
  const { apiKey } = getPreferenceValues<Preferences>();
  const monthOptions = generateMonthOptions();
  const [selectedMonth, setSelectedMonth] = useState<string>(
    monthOptions[0].value
  );
  const [selectedCategory, setSelectedCategory] =
    useState<CategoryTotal | null>(null);
  const { start, end } = getDateRange(selectedMonth);

  const api = useMemo(() => new LunchMoneyApi(apiKey), [apiKey]);

  const { isLoading, data } = useCachedPromise(
    async (startDate: string, endDate: string) =>
      api.getTransactions({
        start_date: startDate,
        end_date: endDate,
      }),
    [start, end]
  );

  const transactions = data?.transactions ?? [];
  const categoryTotals = calculateCategoryTotals(transactions);
  const grandTotal = categoryTotals.reduce((sum, cat) => sum + cat.total, 0);
  const formattedGrandTotal = new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
  }).format(grandTotal);

  if (selectedCategory) {
    return (
      <CategoryTransactionsList
        category={selectedCategory}
        onBack={() => setSelectedCategory(null)}
      />
    );
  }

  return (
    <List
      isLoading={isLoading}
      searchBarPlaceholder="Search categories..."
      searchBarAccessory={
        <List.Dropdown
          tooltip="Select Month"
          value={selectedMonth}
          onChange={setSelectedMonth}
        >
          {monthOptions.map((option) => (
            <List.Dropdown.Item
              key={option.value}
              value={option.value}
              title={option.title}
            />
          ))}
        </List.Dropdown>
      }
    >
      <List.Section title={`Total Spending: ${formattedGrandTotal}`}>
        {categoryTotals.map((category) => {
          const formattedTotal = new Intl.NumberFormat("en-US", {
            style: "currency",
            currency: "USD",
          }).format(category.total);

          return (
            <List.Item
              key={category.name}
              icon={{ source: Icon.Tag, tintColor: Color.Red }}
              title={category.name}
              subtitle={`${category.count} transaction${category.count !== 1 ? "s" : ""}`}
              accessories={[
                {
                  text: `${category.percentage.toFixed(1)}%`,
                },
                {
                  text: formattedTotal,
                },
              ]}
              actions={
                <ActionPanel>
                  <Action
                    title="View Transactions"
                    icon={Icon.List}
                    onAction={() => setSelectedCategory(category)}
                  />
                  <Action.CopyToClipboard
                    content={`${category.name}: ${formattedTotal} (${category.percentage.toFixed(1)}%)`}
                    title="Copy Category Total"
                  />
                </ActionPanel>
              }
            />
          );
        })}
      </List.Section>
    </List>
  );
}
