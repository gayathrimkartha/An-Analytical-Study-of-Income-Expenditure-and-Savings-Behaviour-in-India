library(readxl)

options(scipen = 999)

dataset <- read_excel("INDIAN PERSONAL FINANCE AND SPENDING HABITS.xlsx")

expense_cols <- c(
  "Rent", "Loan_Repayment", "Insurance", "Groceries",
  "Transport", "Eating_Out", "Entertainment",
  "Utilities", "Healthcare", "Education", "Miscellaneous"
)

dataset$Total_Spending <- rowSums(
  dataset[, expense_cols],
  na.rm = TRUE
)

dataset$Spending_Ratio <- dataset$Total_Spending / dataset$Income

dataset$Age_Group <- cut(
  dataset$Age,
  breaks = c(15, 25, 35, 45, 55, 65),
  labels = c("15-24", "25-34", "35-44", "45-54", "55-64"),
  right = FALSE
)

dataset$Category <- ifelse(
  dataset$Spending_Ratio > 0.8,
  "High Spender",
  ifelse(dataset$Spending_Ratio > 0.5, "Moderate", "High Saver")
)

cat("Minimum Age:", min(dataset$Age), "\n")
cat("Maximum Age:", max(dataset$Age), "\n\n")

summary(dataset[, c("Income", "Total_Spending", "Desired_Savings")])

colMeans(
  dataset[, c("Rent", "Groceries", "Transport", "Entertainment")],
  na.rm = TRUE
)

table(dataset$Category)

hist(
  dataset$Spending_Ratio,
  main = "Spending Ratio Distribution",
  xlab = "Spending Ratio",
  col = "orange",
  border = "black"
)