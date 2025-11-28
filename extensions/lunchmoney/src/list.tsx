import { List, showToast, Toast } from "@raycast/api";
import { useCachedPromise } from "@raycast/utils";
import { useState, useMemo } from "react";
import { type Transaction, type Tag, useLunchMoney } from "./api";
import {
  EditTransactionForm,
  TransactionDetail,
  TransactionListItem,
  generateMonthOptions,
  getDateRange,
  getDateRangeForFilter,
  DateRangeDropdown,
} from "./components";

export default function Command() {
  const client = useLunchMoney();
  const monthOptions = generateMonthOptions();
  const [selectedMonth, setSelectedMonth] = useState<string>(monthOptions[0].value);
  const [searchText, setSearchText] = useState("");
  const { start, end } = useMemo(() => getDateRangeForFilter(selectedMonth), [selectedMonth]);
  const [selectedTransaction, setSelectedTransaction] = useState<Transaction | null>(null);
  const [editingTransaction, setEditingTransaction] = useState<Transaction | null>(null);

  const { isLoading, data, revalidate } = useCachedPromise(
    async (startDate: string, endDate: string) => {
      const { data, error } = await client.GET("/transactions", {
        params: { query: { start_date: startDate, end_date: endDate } },
      });
      if (error) {
        console.error("Transactions fetch error:", error);
        throw new Error(JSON.stringify(error));
      }
      return data?.transactions || [];
    },
    [start, end],
  );

  const { data: categoriesData } = useCachedPromise(async () => {
    const { data, error } = await client.GET("/categories");
    if (error) {
      console.error("Categories fetch error:", error);
      throw new Error(JSON.stringify(error));
    }
    return data?.categories || [];
  });

  const { data: tagsData } = useCachedPromise(async () => {
    const { data, error } = await client.GET("/tags");
    if (error) {
      console.error("Tags fetch error:", error);
      throw new Error(JSON.stringify(error));
    }
    return data?.tags || [];
  });

  const transactions = (data ?? []).sort((a: Transaction, b: Transaction) => {
    return new Date(b.date).getTime() - new Date(a.date).getTime();
  });

  const categories = categoriesData ?? [];
  const tags = tagsData ?? [];

  // Filter transactions based on search text
  const filteredTransactions = useMemo(() => {
    if (!searchText.trim()) {
      return transactions;
    }

    const query = searchText.toLowerCase();
    return transactions.filter((transaction: Transaction) => {
      const payee = transaction.payee?.toLowerCase() || "";
      const amount = transaction.amount.toString();
      const notes = transaction.notes?.toLowerCase() || "";
      // Get tag names from tag_ids
      const transactionTags = transaction.tag_ids
        .map((tagId) => tags.find((t) => t.id === tagId))
        .filter((t): t is Tag => t !== undefined);
      const tagNames = transactionTags.map((t) => t.name.toLowerCase()).join(" ");
      // Get category name from category_id
      const category = categories.find((c) => c.id === transaction.category_id);
      const categoryName = category?.name.toLowerCase() || "";

      return (
        payee.includes(query) ||
        categoryName.includes(query) ||
        amount.includes(query) ||
        notes.includes(query) ||
        tagNames.includes(query)
      );
    });
  }, [transactions, searchText, categories, tags]);

  async function handleUpdateTransaction(transactionId: number, categoryId: string, tagNames: string[]) {
    const tagIds = tagNames
      .map((name) => tags.find((t: Tag) => t.name === name)?.id)
      .filter((id): id is number => id !== undefined);

    const { error } = await client.PUT("/transactions/{id}", {
      params: { path: { id: transactionId } },
      body: {
        category_id: parseInt(categoryId),
        tags: tagIds,
      },
    });
    if (error) {
      console.error("Update transaction error:", error);
      throw new Error(JSON.stringify(error));
    }
    revalidate();
    setEditingTransaction(null);
  }

  async function handleToggleReviewStatus(transaction: Transaction) {
    const newStatus = transaction.status === "reviewed" ? "unreviewed" : "reviewed";
    const { error } = await client.PUT("/transactions/{id}", {
      params: { path: { id: transaction.id } },
      body: {
        status: newStatus,
      },
    });
    if (error) {
      console.error("Toggle review status error:", error);
      throw new Error(JSON.stringify(error));
    }
    await showToast({
      style: Toast.Style.Success,
      title: newStatus === "reviewed" ? "Marked as reviewed" : "Marked as unreviewed",
    });
    revalidate();
  }

  if (editingTransaction) {
    return (
      <EditTransactionForm
        transaction={editingTransaction}
        categories={categories}
        tags={tags}
        onSubmit={(categoryId, tagIds) => handleUpdateTransaction(editingTransaction.id, categoryId, tagIds)}
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
        categories={categories}
        tags={tags}
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
      searchBarAccessory={<DateRangeDropdown value={selectedMonth} onChange={setSelectedMonth} />}
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
            categories={categories}
            tags={tags}
            onToggleReviewStatus={handleToggleReviewStatus}
            onEdit={setEditingTransaction}
            lunchMoneyUrl={lunchMoneyUrl}
          />
        );
      })}
    </List>
  );
}
