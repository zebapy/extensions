import {
  LunchMoneyApi,
  LMTransaction,
  LMCategory,
  LMTag,
} from "lunchmoney-tools";

export type Transaction = LMTransaction;
export type Category = LMCategory;
export type Tag = LMTag;

export interface Asset {
  id: number;
  type_name: string;
  subtype_name?: string | null;
  name: string;
  display_name?: string | null;
  balance: string;
  balance_as_of?: string;
  currency: string;
  institution_name?: string | null;
  closed_on?: string | null;
  exclude_transactions: boolean;
}

export interface PlaidAccount {
  id: number;
  type?: string;
  subtype?: string | null;
  name: string;
  display_name?: string | null;
  balance: string;
  last_import?: string;
  currency?: string;
  institution_name?: string | null;
}

export interface AssetsResponse {
  assets: Asset[];
}

export interface PlaidAccountsResponse {
  plaid_accounts: PlaidAccount[];
}

export interface TransactionFilters {
  start_date: string;
  end_date: string;
}

export interface TransactionUpdateData {
  category_id?: number;
  tags?: number[];
  status?: "cleared" | "uncleared";
}

export class LunchMoneyService {
  private api: LunchMoneyApi;

  constructor(apiKey: string) {
    this.api = new LunchMoneyApi(apiKey);
  }

  async getTransactions(filters: TransactionFilters) {
    return this.api.getTransactions(filters);
  }

  async getCategories() {
    return this.api.getCategories();
  }

  async getTags() {
    return this.api.getTags();
  }

  async updateTransaction(transactionId: number, data: TransactionUpdateData) {
    return this.api.updateTransaction(transactionId, data);
  }

  async getAssets(): Promise<AssetsResponse> {
    return this.api.getAssets();
  }

  async getPlaidAccounts(): Promise<PlaidAccountsResponse> {
    return this.api.getPlaidAccounts();
  }
}
