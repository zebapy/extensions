import { Fetcher } from "openapi-typescript-fetch";
import type { paths, components } from "./lunchmoney-api";

// Type exports from generated schema
export type Transaction = components["schemas"]["transactionObject"];
export type Category = components["schemas"]["categoryObject"];
export type Tag = components["schemas"]["tagObject"];
export type ManualAccount = components["schemas"]["manualAccountObject"];
export type PlaidAccount = components["schemas"]["plaidAccountObject"];
export type RecurringItem = components["schemas"]["recurringObject"];
export type User = components["schemas"]["userObject"];

export interface TransactionFilters {
  start_date?: string;
  end_date?: string;
  tag_id?: number;
  recurring_id?: number;
  plaid_account_id?: number;
  category_id?: number;
  manual_account_id?: number;
  is_group?: boolean;
  status?: "reviewed" | "unreviewed" | "delete_pending";
  limit?: number;
  offset?: number;
}

export interface TransactionUpdateData {
  category_id?: number;
  tags?: number[];
  status?: "reviewed" | "unreviewed";
  notes?: string;
  payee?: string;
  amount?: string | number;
  date?: string;
}

// Create a typed fetcher instance
const createFetcher = (apiKey: string) => {
  const fetcher = Fetcher.for<paths>();

  fetcher.configure({
    baseUrl: "https://api.lunchmoney.dev/v2",
    init: {
      headers: {
        Authorization: `Bearer ${apiKey}`,
      },
    },
  });

  return fetcher;
};

export class LunchMoneyService {
  private fetcher: ReturnType<typeof createFetcher>;

  constructor(apiKey: string) {
    this.fetcher = createFetcher(apiKey);
  }

  async getTransactions(filters: TransactionFilters) {
    const getTransactions = this.fetcher
      .path("/transactions")
      .method("get")
      .create();
    const response = await getTransactions(filters);
    return response.data.transactions || [];
  }

  async getCategories() {
    const getCategories = this.fetcher
      .path("/categories")
      .method("get")
      .create();
    const response = await getCategories({});
    return response.data.categories || [];
  }

  async getTags() {
    const getTags = this.fetcher.path("/tags").method("get").create();
    const response = await getTags({});
    return response.data.tags || [];
  }

  async updateTransaction(transactionId: number, data: TransactionUpdateData) {
    const updateTransaction = this.fetcher
      .path("/transactions/{id}")
      .method("put")
      .create();
    const response = await updateTransaction({ id: transactionId, ...data });
    return response.data;
  }

  async getManualAccounts() {
    const getManualAccounts = this.fetcher
      .path("/manual_accounts")
      .method("get")
      .create();
    const response = await getManualAccounts({});
    return response.data.manual_accounts || [];
  }

  async getPlaidAccounts() {
    const getPlaidAccounts = this.fetcher
      .path("/plaid_accounts")
      .method("get")
      .create();
    const response = await getPlaidAccounts({});
    return response.data.plaid_accounts || [];
  }

  async getMe() {
    const getMe = this.fetcher.path("/me").method("get").create();
    const { data } = await getMe({});
    return data;
  }

  async getSummary(params: { start_date: string; end_date: string }) {
    const getSummary = this.fetcher.path("/summary").method("get").create();
    const { data } = await getSummary(params);
    return data;
  }

  async triggerPlaidSync() {
    const triggerFetch = this.fetcher
      .path("/plaid_accounts/fetch")
      .method("post")
      .create();
    const response = await triggerFetch({});
    return response;
  }
}
