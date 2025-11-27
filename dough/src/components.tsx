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
import { LMTransaction, LMCategory, LMTag } from "lunchmoney-tools";
import { formatAmount } from "./mockData";

type Transaction = LMTransaction;
type Category = LMCategory;
type Tag = LMTag;

export interface TransactionListItemProps {
  transaction: Transaction;
  onToggleReviewStatus?: (transaction: Transaction) => void;
  onEdit?: (transaction: Transaction) => void;
  lunchMoneyUrl?: string;
  customDetailTarget?: React.ReactNode;
}

export function TransactionListItem({
  transaction,
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
  const displayAmount = parseFloat(
    formatAmount(transaction.amount, transaction.is_income)
  );
  const formattedAmount = `${isExpense ? "-" : "+"}$${Math.abs(displayAmount).toLocaleString("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
  const isReviewed = transaction.status === "cleared";

  const accessories = [
    ...transaction.tags.map((tag) => ({ tag: { value: tag.name } })),
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
      subtitle={transaction.category_name || "Uncategorized"}
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
    (c) => c.name === transaction.category_name
  );

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
        defaultValue={transaction.tags.map((t) => t.name)}
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
  onBack,
  onEdit,
  lunchMoneyUrl,
}: {
  transaction: Transaction;
  onBack?: () => void;
  onEdit?: () => void;
  lunchMoneyUrl?: string;
}) {
  const originalAmount =
    typeof transaction.amount === "string"
      ? parseFloat(transaction.amount)
      : transaction.amount;
  const isExpense = originalAmount > 0;
  const displayAmount = parseFloat(
    formatAmount(transaction.amount, transaction.is_income)
  );
  const formattedAmount = `${isExpense ? "-" : "+"}$${Math.abs(displayAmount).toLocaleString("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
  const isReviewed = transaction.status === "cleared";

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
            text={transaction.category_name || "Uncategorized"}
            icon={Icon.Tag}
          />

          {transaction.tags.length > 0 && (
            <Detail.Metadata.TagList title="Tags">
              {transaction.tags.map((tag) => (
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
