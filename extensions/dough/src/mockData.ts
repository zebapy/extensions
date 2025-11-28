// Mock mode flag for randomizing amounts in screenshots
// Set to true when taking screenshots to randomize amounts under $200

export const isMockMode = false; // Set to false for real data

const mockValues = [
  12.34, 23.45, 34.56, 45.67, 56.78, 67.89, 78.9, 89.01, 98.76, 87.65, 76.54, 65.43, 54.32, 43.21, 123.45, 111.11,
  99.99, 88.88, 77.77, 66.66, 55.55, 44.44, 33.33, 22.22, 11.11,
];

function getMockAmount(): number {
  return mockValues[Math.floor(Math.random() * mockValues.length)];
}

export function formatAmount(amount: string | number, isIncome?: boolean): string {
  if (!isMockMode) {
    const num = typeof amount === "string" ? parseFloat(amount) : amount;
    return num.toFixed(2);
  }

  const mockAmount = getMockAmount();
  if (isIncome) {
    return (-mockAmount).toFixed(2);
  }
  return mockAmount.toFixed(2);
}

export function formatBalance(balance: string | number): string {
  if (!isMockMode) {
    const num = typeof balance === "string" ? parseFloat(balance) : balance;
    return num.toFixed(2);
  }

  return getMockAmount().toFixed(2);
}
