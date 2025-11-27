import { List, getPreferenceValues, showToast, Toast } from "@raycast/api";
import { useCachedPromise } from "@raycast/utils";
import { useState, useMemo } from "react";
import { LunchMoneyService, type Transaction, type Tag } from "./api";
import {
  EditTransactionForm,
  TransactionDetail,
  TransactionListItem,
} from "./components";

interface Preferences {
  apiKey: string;
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

function getDateRangeForFilter(filter: string): { start: string; end: string } {
  const now = new Date();
  const end = now.toISOString().split("T")[0];
  let start: Date;

  switch (filter) {
    case "7days":
      start = new Date();
      start.setDate(start.getDate() - 7);
      break;
    case "30days":
      start = new Date();
      start.setDate(start.getDate() - 30);
      break;
    case "90days":
      start = new Date();
      start.setDate(start.getDate() - 90);
      break;
    case "thisMonth":
      start = new Date(now.getFullYear(), now.getMonth(), 1);
      break;
    case "lastMonth": {
      const lastMonth = new Date(now.getFullYear(), now.getMonth() - 1, 1);
      const lastMonthEnd = new Date(now.getFullYear(), now.getMonth(), 0);
      return {
        start: lastMonth.toISOString().split("T")[0],
        end: lastMonthEnd.toISOString().split("T")[0],
      };
    }
    case "thisYear":
      start = new Date(now.getFullYear(), 0, 1);
      break;
    case "lastYear": {
      const lastYearStart = new Date(now.getFullYear() - 1, 0, 1);
      const lastYearEnd = new Date(now.getFullYear() - 1, 11, 31);
      return {
        start: lastYearStart.toISOString().split("T")[0],
        end: lastYearEnd.toISOString().split("T")[0],
      };
    }
    case "allTime":
      start = new Date();
      start.setFullYear(start.getFullYear() - 2);
      break;
    default:
      return getDateRange(filter);
  }

  return {
    start: start.toISOString().split("T")[0],
    end,
  };
}

export default function Command() {
  const { apiKey } = getPreferenceValues<Preferences>();
  const monthOptions = generateMonthOptions();
  const [selectedMonth, setSelectedMonth] = useState<string>(
    monthOptions[0].value
  );
  const [searchText, setSearchText] = useState("");
  const { start, end } = useMemo(
    () => getDateRangeForFilter(selectedMonth),
    [selectedMonth]
  );
  const [selectedTransaction, setSelectedTransaction] =
    useState<Transaction | null>(null);
  const [editingTransaction, setEditingTransaction] =
    useState<Transaction | null>(null);

  const api = useMemo(() => new LunchMoneyService(apiKey), [apiKey]);

  const { isLoading, data, revalidate } = useCachedPromise(
    async (startDate: string, endDate: string) =>
      api.getTransactions({
        start_date: startDate,
        end_date: endDate,
      }),
    [start, end]
  );

  const { data: categoriesData } = useCachedPromise(async () =>
    api.getCategories()
  );

  const { data: tagsData } = useCachedPromise(async () => api.getTags());

  const transactions = (data?.transactions ?? []).sort(
    (a: Transaction, b: Transaction) => {
      return new Date(b.date).getTime() - new Date(a.date).getTime();
    }
  );

  // Filter transactions based on search text
  const filteredTransactions = useMemo(() => {
    if (!searchText.trim()) {
      return transactions;
    }

    const query = searchText.toLowerCase();
    return transactions.filter((transaction: Transaction) => {
      const payee = transaction.payee?.toLowerCase() || "";
      const category = transaction.category_name?.toLowerCase() || "";
      const amount = transaction.amount.toString();
      const notes = transaction.notes?.toLowerCase() || "";
      const tagNames = transaction.tags
        .map((t) => t.name.toLowerCase())
        .join(" ");

      return (
        payee.includes(query) ||
        category.includes(query) ||
        amount.includes(query) ||
        notes.includes(query) ||
        tagNames.includes(query)
      );
    });
  }, [transactions, searchText]);

  const categories = categoriesData?.categories ?? [];
  const tags = tagsData ?? [];

  async function handleUpdateTransaction(
    transactionId: number,
    categoryId: string,
    tagNames: string[]
  ) {
    const tagIds = tagNames
      .map((name) => tags.find((t: Tag) => t.name === name)?.id)
      .filter((id): id is number => id !== undefined);

    await api.updateTransaction(transactionId, {
      category_id: parseInt(categoryId),
      tags: tagIds,
    });
    revalidate();
    setEditingTransaction(null);
  }

  async function handleToggleReviewStatus(transaction: Transaction) {
    const newStatus =
      transaction.status === "cleared" ? "uncleared" : "cleared";
    await api.updateTransaction(transaction.id, {
      status: newStatus,
    });
    await showToast({
      style: Toast.Style.Success,
      title:
        newStatus === "cleared" ? "Marked as reviewed" : "Marked as unreviewed",
    });
    revalidate();
  }

  if (editingTransaction) {
    return (
      <EditTransactionForm
        transaction={editingTransaction}
        categories={categories}
        tags={tags}
        onSubmit={(categoryId, tagIds) =>
          handleUpdateTransaction(editingTransaction.id, categoryId, tagIds)
        }
      />
    );
  }

  if (selectedTransaction) {
    const { start, end } = getDateRange(selectedMonth);
    const [year, month] = selectedMonth.split("-");
    const category = selectedTransaction.category_id || "";
    const lunchMoneyUrl = `https://my.lunchmoney.app/transactions/${year}/${month}?${category ? `category=${category}&` : ""}end_date=${end}&match=all&start_date=${start}&time=custom`;

    return (
      <TransactionDetail
        transaction={selectedTransaction}
        onBack={() => setSelectedTransaction(null)}
        onEdit={() => {
          setEditingTransaction(selectedTransaction);
          setSelectedTransaction(null);
        }}
        lunchMoneyUrl={lunchMoneyUrl}
      />
    );
  }

  return (
    <List
      isLoading={isLoading}
      searchBarPlaceholder="Search transactions by payee, category, amount, notes, or tags..."
      onSearchTextChange={setSearchText}
      searchBarAccessory={
        <List.Dropdown
          tooltip="Select Time Range"
          value={selectedMonth}
          onChange={setSelectedMonth}
        >
          <List.Dropdown.Section title="Quick Ranges">
            <List.Dropdown.Item value="7days" title="Last 7 Days" />
            <List.Dropdown.Item value="30days" title="Last 30 Days" />
            <List.Dropdown.Item value="90days" title="Last 90 Days" />
            <List.Dropdown.Item value="thisMonth" title="This Month" />
            <List.Dropdown.Item value="lastMonth" title="Last Month" />
            <List.Dropdown.Item value="thisYear" title="This Year" />
            <List.Dropdown.Item value="lastYear" title="Last Year" />
            <List.Dropdown.Item value="allTime" title="All Time" />
          </List.Dropdown.Section>
          <List.Dropdown.Section title="By Month">
            {monthOptions.map((option) => (
              <List.Dropdown.Item
                key={option.value}
                value={option.value}
                title={option.title}
              />
            ))}
          </List.Dropdown.Section>
        </List.Dropdown>
      }
      throttle
    >
      {filteredTransactions.map((transaction: Transaction) => {
        const [year, month] = selectedMonth.split("-");
        const category = transaction.category_id || "";
        const lunchMoneyUrl = `https://my.lunchmoney.app/transactions/${year}/${month}?${category ? `category=${category}&` : ""}end_date=${end}&match=all&start_date=${start}&time=custom`;

        return (
          <TransactionListItem
            key={transaction.id}
            transaction={transaction}
            onToggleReviewStatus={handleToggleReviewStatus}
            onEdit={setEditingTransaction}
            lunchMoneyUrl={lunchMoneyUrl}
          />
        );
      })}
    </List>
  );
}
