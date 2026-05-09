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

aggregate(Spending_Ratio ~ City_Tier, dataset, mean)

aggregate(Spending_Ratio ~ Age_Group, dataset, mean)

aggregate(
  cbind(Rent, Groceries, Transport, Utilities) ~ City_Tier,
  dataset,
  mean
)

aggregate(Desired_Savings ~ City_Tier, dataset, mean)

table(dataset$City_Tier, dataset$Category)

anova_model <- aov(Total_Spending ~ City_Tier, data = dataset)

summary(anova_model)

boxplot(
  Total_Spending ~ City_Tier,
  data = dataset,
  col = c("lightblue", "lightgreen", "lightpink"),
  main = "Total Spending by City Tier",
  xlab = "City Tier",
  ylab = "Total Spending (₹)"
)

grid()

boxplot(
  Desired_Savings ~ City_Tier,
  data = dataset,
  col = c("lightblue", "lightgreen", "lightpink"),
  main = "Desired Savings by City Tier",
  xlab = "City Tier",
  ylab = "Desired Savings (₹)"
)

grid()