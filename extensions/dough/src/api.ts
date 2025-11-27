import { getPreferenceValues } from "@raycast/api";
import createClient from "openapi-fetch";
import { useMemo } from "react";
import type { components, paths } from "./lunchmoney-api";

// Type exports from generated schema
export type Transaction = components["schemas"]["transactionObject"];
export type Category = components["schemas"]["categoryObject"];
export type Tag = components["schemas"]["tagObject"];
export type ManualAccount = components["schemas"]["manualAccountObject"];
export type PlaidAccount = components["schemas"]["plaidAccountObject"];

interface Preferences {
  apiKey: string;
}

export function useLunchMoney() {
  const { apiKey } = getPreferenceValues<Preferences>();

  const client = useMemo(
    () =>
      createClient<paths>({
        baseUrl: "https://api.lunchmoney.dev/v2",
        headers: {
          Authorization: `Bearer ${apiKey}`,
        },
      }),
    [apiKey],
  );

  return client;
}
