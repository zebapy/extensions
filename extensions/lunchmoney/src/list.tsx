import { List } from "@raycast/api";
import { useCachedPromise } from "@raycast/utils";
import { useState, useMemo } from "react";
import { type Transaction, type Tag, useLunchMoney } from "./api";
import { TransactionListItem, getDateRangeForFilter, DateRangeDropdown } from "./components";

function buildLunchMoneyUrl({
  transaction,
  year,
  month,
  start,
  end,
}: {
  transaction: Transaction;
  year: string;
  month: string;
  start: string;
  end: string;
}): string {
  const url = new URL(`https://my.lunchmoney.app/transactions/${year}/${month}`);
  if (transaction.category_id) {
    url.searchParams.set("category", transaction.category_id.toString());
  }
  url.searchParams.set("end_date", end);
  url.searchParams.set("match", "all");
  url.searchParams.set("start_date", start);
  url.searchParams.set("time", "custom");
  return url.toString();
}

export default function Command() {
  const client = useLunchMoney();
  const [selectedMonth, setSelectedMonth] = useState<string>("thisMonth");
  const [searchText, setSearchText] = useState<string>("");
  const { start, end } = useMemo(() => getDateRangeForFilter(selectedMonth), [selectedMonth]);

  const { isLoading, data, revalidate } = useCachedPromise(
    async (startDate: string, endDate: string) => {
      const allTransactions: Transaction[] = [];
      let offset = 0;
      let hasMore = true;

      while (hasMore) {
        const { data, error } = await client.GET("/transactions", {
          params: { query: { start_date: startDate, end_date: endDate, offset } },
        });
        if (error) {
          console.error("Transactions fetch error:", error);
          throw new Error(JSON.stringify(error));
        }
        allTransactions.push(...(data?.transactions || []));
        hasMore = data?.has_more ?? false;
        offset += data?.transactions?.length ?? 0;
      }

      return allTransactions;
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
    if (!searchText) return transactions;

    const lowerSearch = searchText.toLowerCase();
    return transactions.filter((t: Transaction) => {
      const category = categories.find((c) => c.id === t.category_id);
      const transactionTags = (t.tag_ids || [])
        .map((tagId) => tags.find((tag) => tag.id === tagId))
        .filter((tag): tag is Tag => tag !== undefined);

      return (
        t.payee?.toLowerCase().includes(lowerSearch) ||
        t.notes?.toLowerCase().includes(lowerSearch) ||
        t.status?.toLowerCase().includes(lowerSearch) ||
        category?.name?.toLowerCase().includes(lowerSearch) ||
        transactionTags.some((tag) => tag.name.toLowerCase().includes(lowerSearch))
      );
    });
  }, [transactions, searchText, categories, tags]);

  const pendingTransactions = filteredTransactions.filter((t: Transaction) => t.is_pending);
  const nonPendingTransactions = filteredTransactions.filter((t: Transaction) => !t.is_pending);

  // Group non-pending transactions by date
  const transactionsByDate = nonPendingTransactions.reduce(
    (acc, transaction) => {
      const date = transaction.date;
      if (!acc[date]) {
        acc[date] = [];
      }
      acc[date].push(transaction);
      return acc;
    },
    {} as Record<string, Transaction[]>,
  );

  // Sort dates in descending order
  const sortedDates = Object.keys(transactionsByDate).sort((a, b) => new Date(b).getTime() - new Date(a).getTime());

  // Extract year and month for URL building - handle both month format (YYYY-MM) and quick ranges
  const getYearMonth = () => {
    if (selectedMonth.includes("-")) {
      const [year, month] = selectedMonth.split("-");
      return { year, month };
    }
    // For quick ranges, use the start date from the date range
    const startDate = new Date(start);
    return {
      year: startDate.getFullYear().toString(),
      month: (startDate.getMonth() + 1).toString().padStart(2, "0"),
    };
  };

  const { year, month } = getYearMonth();

  return (
    <List
      isLoading={isLoading}
      searchBarPlaceholder="Search transactions..."
      searchBarAccessory={<DateRangeDropdown value={selectedMonth} onChange={setSelectedMonth} />}
      filtering={false}
      onSearchTextChange={setSearchText}
    >
      {pendingTransactions.length > 0 && (
        <List.Section
          title="Pending"
          subtitle={`${pendingTransactions.length} transaction${pendingTransactions.length === 1 ? "" : "s"}`}
        >
          {pendingTransactions.map((transaction: Transaction) => {
            const lunchMoneyUrl = buildLunchMoneyUrl({ transaction, year, month, start, end });

            return (
              <TransactionListItem
                key={transaction.id}
                transaction={transaction}
                categories={categories}
                tags={tags}
                onRevalidate={revalidate}
                lunchMoneyUrl={lunchMoneyUrl}
              />
            );
          })}
        </List.Section>
      )}
      {sortedDates.map((date) => {
        const dateTransactions = transactionsByDate[date];
        const formattedDate = new Date(date + "T00:00:00").toLocaleDateString("en-US", {
          weekday: "long",
          year: "numeric",
          month: "long",
          day: "numeric",
        });

        return (
          <List.Section
            key={date}
            title={formattedDate}
            subtitle={`${dateTransactions.length} transaction${dateTransactions.length === 1 ? "" : "s"}`}
          >
            {dateTransactions.map((transaction: Transaction) => {
              const lunchMoneyUrl = buildLunchMoneyUrl({ transaction, year, month, start, end });

              return (
                <TransactionListItem
                  key={transaction.id}
                  transaction={transaction}
                  categories={categories}
                  tags={tags}
                  onRevalidate={revalidate}
                  lunchMoneyUrl={lunchMoneyUrl}
                />
              );
            })}
          </List.Section>
        );
      })}
    </List>
  );
}
