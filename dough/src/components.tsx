import {
  ActionPanel,
  Action,
  Icon,
  Detail,
  Form,
  showToast,
  Toast,
  List,
} from "@raycast/api";
import { useState } from "react";
import { formatAmount } from "./mockData";
import type { Transaction, Category, Tag } from "./api";

export function generateMonthOptions() {
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

export function getDateRange(monthValue: string) {
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

export function getDateRangeForFilter(filter: string): {
  start: string;
  end: string;
} {
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

export function DateRangeDropdown({
  value,
  onChange,
}: {
  value: string;
  onChange: (value: string) => void;
}) {
  const monthOptions = generateMonthOptions();

  return (
    <List.Dropdown
      tooltip="Select Time Range"
      value={value}
      onChange={onChange}
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
  );
}

export interface TransactionListItemProps {
  transaction: Transaction;
  categories?: Category[];
  tags?: Tag[];
  onToggleReviewStatus?: (transaction: Transaction) => void;
  onEdit?: (transaction: Transaction) => void;
  lunchMoneyUrl?: string;
  customDetailTarget?: React.ReactNode;
}

export function TransactionListItem({
  transaction,
  categories = [],
  tags = [],
  onToggleReviewStatus,
  onEdit,
  lunchMoneyUrl = "https://my.lunchmoney.app/transactions",
  customDetailTarget,
}: TransactionListItemProps) {
  const originalAmount =
    typeof transaction.amount === "string"
      ? parseFloat(transaction.amount)
      : transaction.amount;
  const isExpense = originalAmount > 0;

  // Get category to check if it's income
  const category = categories.find((c) => c.id === transaction.category_id);
  const isIncome = category?.is_income ?? false;

  const displayAmount = parseFloat(formatAmount(transaction.amount, isIncome));
  const formattedAmount = `${isExpense ? "-" : "+"}$${Math.abs(displayAmount).toLocaleString("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
  const isReviewed = transaction.status === "reviewed";

  // Get tag objects from tag_ids
  const transactionTags = transaction.tag_ids
    .map((tagId) => tags.find((t) => t.id === tagId))
    .filter((t): t is Tag => t !== undefined);

  const accessories = [
    ...transactionTags.map((tag) => ({ tag: { value: tag.name } })),
    { text: formattedAmount },
    { text: transaction.date },
    {
      icon: isReviewed ? Icon.CheckCircle : Icon.Circle,
      tooltip: isReviewed ? "Reviewed" : "Unreviewed",
    },
  ];

  const defaultDetailTarget = customDetailTarget || (
    <TransactionDetail
      transaction={transaction}
      categories={categories}
      tags={tags}
      lunchMoneyUrl={lunchMoneyUrl}
    />
  );

  return (
    <List.Item
      key={transaction.id}
      icon={{
        source: isExpense ? Icon.ArrowDown : Icon.ArrowUp,
        tintColor: isExpense ? "#DC143C" : "#228B22",
      }}
      title={transaction.payee || "Unknown"}
      subtitle={category?.name || "Uncategorized"}
      accessories={accessories}
      actions={
        <ActionPanel>
          <Action.Push
            title="View Details"
            icon={Icon.Eye}
            target={defaultDetailTarget}
          />
          {onToggleReviewStatus && (
            <Action
              title={isReviewed ? "Mark as Unreviewed" : "Mark as Reviewed"}
              icon={isReviewed ? Icon.Circle : Icon.CheckCircle}
              shortcut={{ modifiers: ["cmd"], key: "r" }}
              onAction={() => onToggleReviewStatus(transaction)}
            />
          )}
          {onEdit && (
            <Action
              title="Edit Transaction"
              icon={Icon.Pencil}
              shortcut={{ modifiers: ["cmd"], key: "e" }}
              onAction={() => onEdit(transaction)}
            />
          )}
          <Action.OpenInBrowser
            title="Open in Lunch Money"
            url={lunchMoneyUrl}
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
}

export function EditTransactionForm({
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

  const currentCategory = categories.find(
    (c) => c.id === transaction.category_id
  );

  // Get tag names from tag_ids
  const transactionTags = transaction.tag_ids
    .map((tagId) => tags.find((t) => t.id === tagId))
    .filter((t): t is Tag => t !== undefined);

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
      <Form.Dropdown
        id="category"
        title="Category"
        defaultValue={currentCategory?.id.toString()}
      >
        {categories.map((category) => (
          <Form.Dropdown.Item
            key={category.id}
            value={category.id.toString()}
            title={category.name}
          />
        ))}
      </Form.Dropdown>
      <Form.TagPicker
        id="tags"
        title="Tags"
        defaultValue={transactionTags.map((t) => t.name)}
      >
        {tags.map((tag) => (
          <Form.TagPicker.Item key={tag.id} value={tag.name} title={tag.name} />
        ))}
      </Form.TagPicker>
    </Form>
  );
}

export function TransactionDetail({
  transaction,
  categories = [],
  tags = [],
  onBack,
  onEdit,
  lunchMoneyUrl,
}: {
  transaction: Transaction;
  categories?: Category[];
  tags?: Tag[];
  onBack?: () => void;
  onEdit?: () => void;
  lunchMoneyUrl?: string;
}) {
  const originalAmount =
    typeof transaction.amount === "string"
      ? parseFloat(transaction.amount)
      : transaction.amount;
  const isExpense = originalAmount > 0;

  // Get category to check if it's income
  const category = categories.find((c) => c.id === transaction.category_id);
  const isIncome = category?.is_income ?? false;

  const displayAmount = parseFloat(formatAmount(transaction.amount, isIncome));
  const formattedAmount = `${isExpense ? "-" : "+"}$${Math.abs(displayAmount).toLocaleString("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
  const isReviewed = transaction.status === "reviewed";

  // Get tag objects from tag_ids
  const transactionTags = transaction.tag_ids
    .map((tagId) => tags.find((t) => t.id === tagId))
    .filter((t): t is Tag => t !== undefined);

  return (
    <Detail
      markdown={`# ${transaction.payee}\n\n${formattedAmount}`}
      metadata={
        <Detail.Metadata>
          <Detail.Metadata.Label
            title="Amount"
            text={formattedAmount}
            icon={{
              source: isExpense ? Icon.ArrowDown : Icon.ArrowUp,
              tintColor: isExpense ? "#FF0000" : "#00FF00",
            }}
          />
          <Detail.Metadata.Label
            title="Date"
            text={transaction.date}
            icon={Icon.Calendar}
          />

          <Detail.Metadata.Separator />

          <Detail.Metadata.TagList title="Status">
            <Detail.Metadata.TagList.Item
              text={isReviewed ? "Reviewed" : "Unreviewed"}
              color={isReviewed ? "#00FF00" : "#FFA500"}
              icon={isReviewed ? Icon.CheckCircle : Icon.Circle}
            />
          </Detail.Metadata.TagList>

          <Detail.Metadata.Label
            title="Category"
            text={category?.name || "Uncategorized"}
            icon={Icon.Tag}
          />

          {transactionTags.length > 0 && (
            <Detail.Metadata.TagList title="Tags">
              {transactionTags.map((tag) => (
                <Detail.Metadata.TagList.Item key={tag.id} text={tag.name} />
              ))}
            </Detail.Metadata.TagList>
          )}

          {transaction.notes && (
            <>
              <Detail.Metadata.Separator />
              <Detail.Metadata.Label title="Notes" text={transaction.notes} />
            </>
          )}

          <Detail.Metadata.Separator />

          <Detail.Metadata.Label
            title="Currency"
            text={transaction.currency.toUpperCase()}
            icon={Icon.BankNote}
          />
          <Detail.Metadata.Label
            title="Transaction ID"
            text={transaction.id.toString()}
          />

          {lunchMoneyUrl && (
            <>
              <Detail.Metadata.Separator />
              <Detail.Metadata.Link
                title="View in Lunch Money"
                target={lunchMoneyUrl}
                text="Open Transaction"
              />
            </>
          )}
        </Detail.Metadata>
      }
      actions={
        <ActionPanel>
          {onBack && (
            <Action
              title="Back to List"
              icon={Icon.ArrowLeft}
              onAction={onBack}
            />
          )}
          {onEdit && (
            <Action
              title="Edit Transaction"
              icon={Icon.Pencil}
              shortcut={{ modifiers: ["cmd"], key: "e" }}
              onAction={onEdit}
            />
          )}
          {lunchMoneyUrl && (
            <Action.OpenInBrowser
              title="Open in Lunch Money"
              url={lunchMoneyUrl}
              shortcut={{ modifiers: ["cmd"], key: "o" }}
            />
          )}
          <Action.CopyToClipboard
            content={`${transaction.payee} - ${formattedAmount}`}
            title="Copy Transaction"
          />
        </ActionPanel>
      }
    />
  );
}
