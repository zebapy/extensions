import { ActionPanel, Action, Icon, List, getPreferenceValues, Detail, Form, showToast, Toast } from "@raycast/api";
import { useCachedPromise } from "@raycast/utils";
import { useState, useMemo } from "react";
import { LunchMoneyApi, LMTransaction, LMCategory, LMTag } from "lunchmoney-tools";

type Transaction = LMTransaction;
type Category = LMCategory;
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

function EditTransactionForm({
  transaction,
  categories,
  tags,
  onSubmit,
}: {
  transaction: Transaction;
  categories: Category[];
  tags: Tag[];
  onSubmit: (categoryId: string, tagIds: string[]) => Promise<void>;
}) {
  const [isLoading, setIsLoading] = useState(false);

  async function handleSubmit(values: { category: string; tags: string[] }) {
    setIsLoading(true);
    try {
      await onSubmit(values.category, values.tags);
      await showToast({
        style: Toast.Style.Success,
        title: "Transaction updated",
      });
    } catch (error) {
      await showToast({
        style: Toast.Style.Failure,
        title: "Failed to update transaction",
        message: String(error),
      });
    } finally {
      setIsLoading(false);
    }
  }

  const currentCategory = categories.find((c) => c.name === transaction.category_name);

  return (
    <Form
      isLoading={isLoading}
      actions={
        <ActionPanel>
          <Action.SubmitForm title="Save Changes" onSubmit={handleSubmit} />
        </ActionPanel>
      }
    >
      <Form.Description text={`Editing: ${transaction.payee}`} />
      <Form.Dropdown id="category" title="Category" defaultValue={currentCategory?.id.toString()}>
        {categories.map((category) => (
          <Form.Dropdown.Item key={category.id} value={category.id.toString()} title={category.name} />
        ))}
      </Form.Dropdown>
      <Form.TagPicker id="tags" title="Tags" defaultValue={transaction.tags.map((t) => t.name)}>
        {tags.map((tag) => (
          <Form.TagPicker.Item key={tag.id} value={tag.name} title={tag.name} />
        ))}
      </Form.TagPicker>
    </Form>
  );
}

function TransactionDetail({
  transaction,
  onBack,
  onEdit,
}: {
  transaction: Transaction;
  onBack: () => void;
  onEdit: () => void;
}) {
  const amount = typeof transaction.amount === "string" ? parseFloat(transaction.amount) : transaction.amount;
  const isExpense = amount > 0;
  const formattedAmount = `${isExpense ? "-" : "+"}$${Math.abs(amount).toFixed(2)}`;

  return (
    <Detail
      markdown={`# ${transaction.payee}\n\n${formattedAmount}`}
      metadata={
        <Detail.Metadata>
          <Detail.Metadata.Label title="Amount" text={formattedAmount} />
          <Detail.Metadata.Label
            title="Type"
            text={isExpense ? "Expense" : "Income"}
            icon={isExpense ? Icon.ArrowDown : Icon.ArrowUp}
          />
          <Detail.Metadata.Separator />
          <Detail.Metadata.Label title="Date" text={transaction.date} />
          <Detail.Metadata.Label title="Category" text={transaction.category_name || "Uncategorized"} icon={Icon.Tag} />
          {transaction.tags.length > 0 && (
            <Detail.Metadata.TagList title="Tags">
              {transaction.tags.map((tag) => (
                <Detail.Metadata.TagList.Item key={tag.id} text={tag.name} />
              ))}
            </Detail.Metadata.TagList>
          )}
          <Detail.Metadata.Separator />
          <Detail.Metadata.Label title="Currency" text={transaction.currency.toUpperCase()} />
          <Detail.Metadata.Label title="Transaction ID" text={transaction.id.toString()} />
        </Detail.Metadata>
      }
      actions={
        <ActionPanel>
          <Action title="Back to List" icon={Icon.ArrowLeft} onAction={onBack} />
          <Action
            title="Edit Transaction"
            icon={Icon.Pencil}
            shortcut={{ modifiers: ["cmd"], key: "e" }}
            onAction={onEdit}
          />
          <Action.OpenInBrowser
            title="Open in Lunch Money"
            url={`https://my.lunchmoney.app/transactions/${transaction.id}`}
            shortcut={{ modifiers: ["cmd"], key: "o" }}
          />
          <Action.CopyToClipboard content={`${transaction.payee} - ${formattedAmount}`} title="Copy Transaction" />
        </ActionPanel>
      }
    />
  );
}

export default function Command() {
  const { apiKey } = getPreferenceValues<Preferences>();
  const monthOptions = generateMonthOptions();
  const [selectedMonth, setSelectedMonth] = useState<string>(monthOptions[0].value);
  const { start, end } = getDateRange(selectedMonth);
  const [selectedTransaction, setSelectedTransaction] = useState<Transaction | null>(null);
  const [editingTransaction, setEditingTransaction] = useState<Transaction | null>(null);

  const api = useMemo(() => new LunchMoneyApi(apiKey), [apiKey]);

  const { isLoading, data, revalidate } = useCachedPromise(
    async (startDate: string, endDate: string) =>
      api.getTransactions({
        start_date: startDate,
        end_date: endDate,
      }),
    [start, end],
  );

  const { data: categoriesData } = useCachedPromise(async () => api.getCategories());

  const { data: tagsData } = useCachedPromise(async () => api.getTags());

  const transactions = (data?.transactions ?? []).sort((a: Transaction, b: Transaction) => {
    return new Date(b.date).getTime() - new Date(a.date).getTime();
  });

  const categories = categoriesData?.categories ?? [];
  const tags = tagsData ?? [];

  async function handleUpdateTransaction(transactionId: number, categoryId: string, tagNames: string[]) {
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
    const newStatus = transaction.status === "cleared" ? "uncleared" : "cleared";
    await api.updateTransaction(transaction.id, {
      status: newStatus,
    });
    await showToast({
      style: Toast.Style.Success,
      title: newStatus === "cleared" ? "Marked as reviewed" : "Marked as unreviewed",
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
    return (
      <TransactionDetail
        transaction={selectedTransaction}
        onBack={() => setSelectedTransaction(null)}
        onEdit={() => {
          setEditingTransaction(selectedTransaction);
          setSelectedTransaction(null);
        }}
      />
    );
  }

  return (
    <List
      isLoading={isLoading}
      searchBarPlaceholder="Search transactions..."
      searchBarAccessory={
        <List.Dropdown tooltip="Select Month" value={selectedMonth} onChange={setSelectedMonth}>
          {monthOptions.map((option) => (
            <List.Dropdown.Item key={option.value} value={option.value} title={option.title} />
          ))}
        </List.Dropdown>
      }
    >
      {transactions.map((transaction: Transaction) => {
        const amount = typeof transaction.amount === "string" ? parseFloat(transaction.amount) : transaction.amount;
        const isExpense = amount > 0;
        const formattedAmount = `${isExpense ? "-" : "+"}$${Math.abs(amount).toFixed(2)}`;
        const isReviewed = transaction.status === "cleared";

        return (
          <List.Item
            key={transaction.id}
            icon={isExpense ? Icon.ArrowDown : Icon.ArrowUp}
            title={transaction.payee || "Unknown"}
            subtitle={transaction.category_name || "Uncategorized"}
            accessories={[
              { text: formattedAmount },
              { text: transaction.date },
              {
                icon: isReviewed ? Icon.CheckCircle : Icon.Circle,
                tooltip: isReviewed ? "Reviewed" : "Unreviewed",
              },
            ]}
            actions={
              <ActionPanel>
                <Action title="View Details" icon={Icon.Eye} onAction={() => setSelectedTransaction(transaction)} />
                <Action
                  title={isReviewed ? "Mark as Unreviewed" : "Mark as Reviewed"}
                  icon={isReviewed ? Icon.Circle : Icon.CheckCircle}
                  shortcut={{ modifiers: ["cmd"], key: "r" }}
                  onAction={() => handleToggleReviewStatus(transaction)}
                />
                <Action
                  title="Edit Transaction"
                  icon={Icon.Pencil}
                  shortcut={{ modifiers: ["cmd"], key: "e" }}
                  onAction={() => setEditingTransaction(transaction)}
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
