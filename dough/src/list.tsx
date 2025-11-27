import { List, getPreferenceValues, showToast, Toast } from "@raycast/api";
import { useCachedPromise } from "@raycast/utils";
import { useState, useMemo } from "react";
import { LunchMoneyApi, LMTransaction, LMTag } from "lunchmoney-tools";
import {
  EditTransactionForm,
  TransactionDetail,
  TransactionListItem,
} from "./components";

type Transaction = LMTransaction;
type Tag = LMTag;

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

export default function Command() {
  const { apiKey } = getPreferenceValues<Preferences>();
  const monthOptions = generateMonthOptions();
  const [selectedMonth, setSelectedMonth] = useState<string>(
    monthOptions[0].value
  );
  const { start, end } = getDateRange(selectedMonth);
  const [selectedTransaction, setSelectedTransaction] =
    useState<Transaction | null>(null);
  const [editingTransaction, setEditingTransaction] =
    useState<Transaction | null>(null);

  const api = useMemo(() => new LunchMoneyApi(apiKey), [apiKey]);

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
      searchBarPlaceholder="Search transactions..."
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
      {transactions.map((transaction: Transaction) => {
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
