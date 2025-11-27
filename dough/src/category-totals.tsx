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
import { LunchMoneyService, type Transaction } from "./api";
import { formatAmount } from "./mockData";
import {
  TransactionListItem,
  generateMonthOptions,
  getDateRangeForFilter,
  DateRangeDropdown,
} from "./components";

interface Preferences {
  apiKey: string;
}

interface CategoryTotal {
  name: string;
  total: number;
  count: number;
  percentage: number;
  transactions: Transaction[];
  isIncome: boolean;
}

function calculateCategoryTotals(transactions: Transaction[]): CategoryTotal[] {
  const categoryMap = new Map<
    string,
    {
      total: number;
      count: number;
      transactions: Transaction[];
      isIncome: boolean;
    }
  >();
  let grandTotal = 0;

  // Group all transactions by category
  transactions.forEach((transaction) => {
    const amount = parseFloat(
      formatAmount(transaction.amount, transaction.is_income)
    );
    const categoryName = transaction.category_name || "Uncategorized";
    const isIncome = transaction.is_income || false;

    // Only count non-income towards grand total
    if (!isIncome) {
      grandTotal += Math.abs(amount);
    }

    const existing = categoryMap.get(categoryName) || {
      total: 0,
      count: 0,
      transactions: [],
      isIncome: isIncome,
    };
    categoryMap.set(categoryName, {
      total: existing.total + Math.abs(amount),
      count: existing.count + 1,
      transactions: [...existing.transactions, transaction],
      isIncome: isIncome,
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
      isIncome: data.isIncome,
    }))
    .sort((a, b) => b.total - a.total);

  return totals;
}

function CategoryTransactionsList({ category }: { category: CategoryTotal }) {
  const formattedTotal = new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
  }).format(category.total);

  return (
    <List
      navigationTitle={`${category.name} - ${formattedTotal}`}
      searchBarPlaceholder="Search transactions..."
    >
      {category.transactions.map((transaction) => (
        <TransactionListItem
          key={transaction.id}
          transaction={transaction}
          lunchMoneyUrl={`https://my.lunchmoney.app/transactions/${transaction.id}`}
        />
      ))}
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
  const { start, end } = useMemo(
    () => getDateRangeForFilter(selectedMonth),
    [selectedMonth]
  );

  const api = useMemo(() => new LunchMoneyService(apiKey), [apiKey]);

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

  // Separate income and expenses
  const incomeTotals = categoryTotals.filter((cat) => cat.isIncome);
  const expenseTotals = categoryTotals.filter((cat) => !cat.isIncome);

  const totalIncome = incomeTotals.reduce((sum, cat) => sum + cat.total, 0);
  const totalExpenses = expenseTotals.reduce((sum, cat) => sum + cat.total, 0);

  const formattedTotalIncome = new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
  }).format(totalIncome);

  const formattedTotalExpenses = new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
  }).format(totalExpenses);

  if (selectedCategory) {
    return <CategoryTransactionsList category={selectedCategory} />;
  }

  return (
    <List
      isLoading={isLoading}
      searchBarPlaceholder="Search categories..."
      searchBarAccessory={
        <DateRangeDropdown value={selectedMonth} onChange={setSelectedMonth} />
      }
    >
      {incomeTotals.length > 0 && (
        <List.Section title={`Income: ${formattedTotalIncome}`}>
          {incomeTotals.map((category) => {
            const formattedTotal = new Intl.NumberFormat("en-US", {
              style: "currency",
              currency: "USD",
            }).format(category.total);

            return (
              <List.Item
                key={category.name}
                icon={{ source: Icon.Tag, tintColor: Color.Green }}
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
      )}

      {expenseTotals.length > 0 && (
        <List.Section title={`Expenses: ${formattedTotalExpenses}`}>
          {expenseTotals.map((category) => {
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
      )}
    </List>
  );
}
