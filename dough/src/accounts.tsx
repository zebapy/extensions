import { ActionPanel, Action, Icon, List, getPreferenceValues, Color, showToast, Toast } from "@raycast/api";
import { useCachedPromise } from "@raycast/utils";
import { useMemo } from "react";
import { LunchMoneyApi } from "lunchmoney-tools";

interface Preferences {
  apiKey: string;
}

interface Account {
  id: number;
  type_name: string;
  subtype_name: string | null;
  name: string;
  display_name: string | null;
  balance: string;
  balance_as_of: string;
  currency: string;
  institution_name: string | null;
  closed_on: string | null;
  exclude_transactions: boolean;
}

function getAccountIcon(typeName: string): Icon {
  const type = typeName.toLowerCase();
  if (type.includes("credit")) return Icon.CreditCard;
  if (type.includes("cash")) return Icon.BankNote;
  if (type.includes("checking") || type.includes("saving")) return Icon.Building;
  if (type.includes("investment") || type.includes("brokerage")) return Icon.LineChart;
  if (type.includes("loan") || type.includes("mortgage")) return Icon.House;
  return Icon.Wallet;
}

function getAccountColor(balance: number, typeName: string): Color {
  const type = typeName.toLowerCase();

  // Credit cards and loans are liabilities - positive balance means you owe money
  if (type.includes("credit") || type.includes("loan") || type.includes("mortgage")) {
    return balance > 0 ? Color.Red : Color.Green;
  }

  // Assets - negative balance is bad
  return balance >= 0 ? Color.Green : Color.Red;
}

export default function Command() {
  const { apiKey } = getPreferenceValues<Preferences>();
  const api = useMemo(() => new LunchMoneyApi(apiKey), [apiKey]);

  const { isLoading, data, revalidate } = useCachedPromise(async () => {
    const [assetsResponse, plaidResponse] = await Promise.all([api.getAssets(), api.getPlaidAccounts()]);

    // Combine both types of accounts
    const allAccounts: Account[] = [
      ...(assetsResponse.assets || []).map((asset) => ({
        id: asset.id,
        type_name: asset.type_name,
        subtype_name: asset.subtype_name ?? null,
        name: asset.name,
        display_name: asset.display_name ?? null,
        balance: asset.balance,
        balance_as_of: asset.balance_as_of ?? new Date().toISOString(),
        currency: asset.currency,
        institution_name: asset.institution_name ?? null,
        closed_on: asset.closed_on ?? null,
        exclude_transactions: asset.exclude_transactions,
      })),
      ...(plaidResponse.plaid_accounts || []).map((plaid) => ({
        id: plaid.id,
        type_name: plaid.type || "Bank Account",
        subtype_name: plaid.subtype ?? null,
        name: plaid.name,
        display_name: plaid.display_name ?? null,
        balance: plaid.balance,
        balance_as_of: plaid.last_import ?? new Date().toISOString(),
        currency: plaid.currency || "usd",
        institution_name: plaid.institution_name ?? null,
        closed_on: null,
        exclude_transactions: false,
      })),
    ];

    return allAccounts;
  });

  const accounts = data ?? [];

  async function handleSync() {
    try {
      await showToast({
        style: Toast.Style.Animated,
        title: "Syncing accounts...",
      });

      // Trigger fetch from Plaid
      await fetch("https://dev.lunchmoney.app/v1/plaid_accounts/fetch", {
        method: "POST",
        headers: {
          Authorization: `Bearer ${apiKey}`,
          "Content-Type": "application/json",
        },
      });

      await showToast({
        style: Toast.Style.Success,
        title: "Sync initiated",
        message: "Accounts are syncing in the background",
      });

      // Revalidate after a short delay to show updated data
      setTimeout(() => revalidate(), 3000);
    } catch (error) {
      await showToast({
        style: Toast.Style.Failure,
        title: "Sync failed",
        message: String(error),
      });
    }
  }

  // Separate active and closed accounts
  const activeAccounts = accounts.filter((acc) => !acc.closed_on);
  const closedAccounts = accounts.filter((acc) => acc.closed_on);

  // Calculate net worth (assets minus liabilities)
  const netWorth = activeAccounts.reduce((sum, acc) => {
    const balance = parseFloat(acc.balance);
    const isLiability =
      acc.type_name.toLowerCase().includes("credit") ||
      acc.type_name.toLowerCase().includes("loan") ||
      acc.type_name.toLowerCase().includes("mortgage");
    return sum + (isLiability ? -balance : balance);
  }, 0);

  return (
    <List isLoading={isLoading} searchBarPlaceholder="Search accounts...">
      <List.Section title={`Net Worth: $${netWorth.toFixed(2)}`}>
        {activeAccounts.map((account) => {
          const balance = parseFloat(account.balance);
          const formattedBalance = `$${Math.abs(balance).toFixed(2)}`;
          const displayName = account.display_name || account.name;
          const institution = account.institution_name || account.type_name;

          return (
            <List.Item
              key={account.id}
              icon={{
                source: getAccountIcon(account.type_name),
                tintColor: getAccountColor(balance, account.type_name),
              }}
              title={displayName}
              subtitle={institution}
              accessories={[
                { tag: { value: account.type_name, color: Color.Blue } },
                { text: formattedBalance },
                { text: `as of ${account.balance_as_of}`, icon: Icon.Calendar },
              ]}
              actions={
                <ActionPanel>
                  <Action title="Refresh Balance" icon={Icon.ArrowClockwise} onAction={revalidate} />
                  <Action
                    title="Sync All Accounts"
                    icon={Icon.Download}
                    shortcut={{ modifiers: ["cmd"], key: "s" }}
                    onAction={handleSync}
                  />
                  <Action.OpenInBrowser
                    title="Open in Lunch Money"
                    url={`https://my.lunchmoney.app/assets/${account.id}`}
                    shortcut={{ modifiers: ["cmd"], key: "o" }}
                  />
                  <Action.CopyToClipboard content={`${displayName}: ${formattedBalance}`} title="Copy Balance" />
                </ActionPanel>
              }
            />
          );
        })}
      </List.Section>

      {closedAccounts.length > 0 && (
        <List.Section title="Closed Accounts">
          {closedAccounts.map((account) => {
            const balance = parseFloat(account.balance);
            const formattedBalance = `$${Math.abs(balance).toFixed(2)}`;
            const displayName = account.display_name || account.name;

            return (
              <List.Item
                key={account.id}
                icon={{ source: Icon.XMarkCircle, tintColor: Color.SecondaryText }}
                title={displayName}
                subtitle={`Closed on ${account.closed_on}`}
                accessories={[{ text: formattedBalance }]}
                actions={
                  <ActionPanel>
                    <Action.CopyToClipboard content={`${displayName}: ${formattedBalance}`} title="Copy Balance" />
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
