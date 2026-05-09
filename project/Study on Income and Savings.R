library(readxl)
library(ggplot2)

dataset <- read_excel("INDIAN PERSONAL FINANCE AND SPENDING HABITS.xlsx")

hist(
  dataset$Income,
  main = "Income Distribution",
  xlab = "Income (₹)",
  col = "skyblue",
  border = "black"
)

hist(
  dataset$Desired_Savings,
  main = "Desired Savings Distribution",
  xlab = "Desired Savings (₹)",
  col = "lightgreen",
  border = "black"
)

correlation <- cor(
  dataset$Income,
  dataset$Desired_Savings,
  use = "complete.obs"
)

cat("Correlation between Income and Desired Savings:", correlation, "\n")

model <- lm(Desired_Savings ~ Income, data = dataset)

summary(model)

ggplot(dataset, aes(x = Income, y = Desired_Savings)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Desired Savings vs Income",
    x = "Income (₹)",
    y = "Desired Savings (₹)"
  ) +
  theme_minimal()

new_incomes <- data.frame(
  Income = c(15000, 50000)
)

predictions <- predict(model, new_incomes)

cat(
  "Predicted Desired Savings for ₹15,000 Income:",
  round(predictions[1], 2),
  "\n"
)

cat(
  "Predicted Desired Savings for ₹50,000 Income:",
  round(predictions[2], 2),
  "\n"
)