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

dataset$Age_Group <- cut(
  dataset$Age,
  breaks = c(15, 25, 35, 45, 55, 65),
  labels = c("15-24", "25-34", "35-44", "45-54", "55-64"),
  right = FALSE
)

dataset$Spending_Ratio <- dataset$Total_Spending / dataset$Income

spending_by_age <- aggregate(
  Total_Spending ~ Age_Group,
  dataset,
  mean
)

spending_by_age

aggregate(
  Desired_Savings ~ Age_Group,
  dataset,
  mean
)

aggregate(
  Spending_Ratio ~ Age_Group,
  dataset,
  mean
)

aggregate(
  cbind(Rent, Groceries, Transport, Entertainment) ~ Age_Group,
  dataset,
  mean
)

anova_age <- aov(
  Total_Spending ~ Age_Group,
  data = dataset
)

summary(anova_age)

highest_spender <- spending_by_age$Age_Group[
  which.max(spending_by_age$Total_Spending)
]

lowest_spender <- spending_by_age$Age_Group[
  which.min(spending_by_age$Total_Spending)
]

cat(
  "Highest Average Spending Age Group:",
  as.character(highest_spender),
  "\n"
)

cat(
  "Lowest Average Spending Age Group:",
  as.character(lowest_spender),
  "\n"
)

boxplot(
  Total_Spending ~ Age_Group,
  data = dataset,
  col = c("skyblue", "lightgreen", "orange", "pink", "purple"),
  main = "Total Spending by Age Group",
  xlab = "Age Group",
  ylab = "Total Spending (₹)"
)

grid()

boxplot(
  Desired_Savings ~ Age_Group,
  data = dataset,
  col = c("red", "blue", "green", "yellow", "violet"),
  main = "Desired Savings by Age Group",
  xlab = "Age Group",
  ylab = "Desired Savings (₹)"
)

grid()