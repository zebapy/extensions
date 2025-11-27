import {
  Icon,
  List,
  getPreferenceValues,
  showToast,
  Toast,
} from "@raycast/api";
import { useCachedPromise } from "@raycast/utils";
import { useState, useMemo } from "react";
import { LunchMoneyApi, LMTransaction, LMTag } from "lunchmoney-tools";
import { EditTransactionForm, TransactionListItem } from "./components";

type Transaction = LMTransaction;
type Tag = LMTag;

interface Preferences {
  apiKey: string;
}

export default function Command() {
  const { apiKey } = getPreferenceValues<Preferences>();
  const [searchText, setSearchText] = useState("");
  const [editingTransaction, setEditingTransaction] =
    useState<Transaction | null>(null);

  const api = useMemo(() => new LunchMoneyApi(apiKey), [apiKey]);

  // Get last 90 days of transactions for search
  const endDate = new Date();
  const startDate = new Date();
  startDate.setDate(startDate.getDate() - 90);

  const { isLoading, data, revalidate } = useCachedPromise(
    async (startDate: string, endDate: string) =>
      api.getTransactions({
        start_date: startDate,
        end_date: endDate,
      }),
    [startDate.toISOString().split("T")[0], endDate.toISOString().split("T")[0]]
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

  return (
    <List
      isLoading={isLoading}
      searchBarPlaceholder="Search transactions by payee, category, amount, notes, or tags..."
      onSearchTextChange={setSearchText}
      throttle
    >
      {filteredTransactions.length === 0 && !isLoading && searchText && (
        <List.EmptyView
          title="No transactions found"
          description="Try searching with different keywords"
          icon={Icon.MagnifyingGlass}
        />
      )}
      {filteredTransactions.map((transaction: Transaction) => (
        <TransactionListItem
          key={transaction.id}
          transaction={transaction}
          onToggleReviewStatus={handleToggleReviewStatus}
          onEdit={setEditingTransaction}
          lunchMoneyUrl="https://my.lunchmoney.app/transactions"
        />
      ))}
    </List>
  );
}
