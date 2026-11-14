

source("C:/Users/anjal/OneDrive/Documents/Desktop/R project/helpers.R")

library(ggplot2)

dir.create("results", showWarnings = FALSE)

# Load the data
df <- load_data()

cat("\n== DATA ==\n")
cat("Rows:", nrow(df), "| Languages:", length(unique(df$language)),
    "| Years:", min(df$year), "-", max(df$year), "\n")

# Check agreement between TIOBE and PYPL
cat("\n== SOURCE AGREEMENT (Spearman rank correlation per year) ==\n")

agree <- agreement_by_year(df)
print(as.data.frame(agree))

cat("Overall Spearman (all years pooled):",
    round(cor(df$tiobe_rank, df$pypl_rank, method = "spearman"), 3), "\n")

# Find languages with different rankings
cat("\n== LANGUAGES WHERE THE TWO SOURCES DISAGREE MOST ==\n")

gaps <- rank_gaps(df)
print(as.data.frame(head(gaps, 8)))

write.csv(gaps, "results/rank_gaps.csv", row.names = FALSE)
write.csv(agree, "results/agreement_by_year.csv", row.names = FALSE)

# Check salary relationships
cat("\n== SALARY RELATIONSHIPS (Spearman, all rows) ==\n")

cat("Salary vs learning difficulty:",
    round(cor(df$average_salary_usd, df$learning_difficulty, method = "spearman"), 3), "\n")

cat("Salary vs demand score      :",
    round(cor(df$average_salary_usd, df$demand_score, method = "spearman"), 3), "\n")

cat("(Each language appears 7 times, so rows are not independent: report the\n",
    " correlation values, do not lean on p-values.)\n")

# Test forecasting models
cat("\n== FORECAST ACCURACY ON 2026 HOLD-OUT ==\n")

res <- holdout_eval(df, test_year = 2026)
acc <- accuracy_table(res)

print(as.data.frame(acc), digits = 4)
write.csv(acc, "results/forecast_accuracy.csv", row.names = FALSE)

# Select the best forecasting method
best <- best_methods(acc)

cat("\nBest method per metric (lowest MAPE):\n")
print(best)

# Generate future forecasts
fc <- forecast_all(df, best, h = 2)

write.csv(fc, "results/forecasts_2027_2028.csv", row.names = FALSE)

# Show top languages for each career goal
cat("\n== ADVISOR: TOP 5 PER GOAL (weights 0.4 / 0.3 / 0.3) ==\n")

for (goal in names(GOAL_LABELS)) {
  top <- head(advisor_scores(df, fc, goal), 5)
  
  cat("\nGoal:", GOAL_LABELS[[goal]], "\n")
  
  print(top[, c("language", "score", "goal_score", "growth_pct", "salary", "difficulty")],
        row.names = FALSE, digits = 4)
}

# Create the first figure
p1 <- ggplot(agree, aes(year, spearman)) +
  geom_line() +
  geom_point() +
  ylim(0.9, 1) +
  labs(title = "TIOBE vs PYPL rank agreement",
       y = "Spearman correlation", x = "Year") +
  theme_minimal()

ggsave("results/fig1_agreement.png", p1, width = 6, height = 4, dpi = 200)

# Create the second figure
d26 <- df[df$year == 2026, ]

p2 <- ggplot(d26, aes(tiobe_rank, pypl_rank, label = language)) +
  geom_abline(linetype = "dashed") +
  geom_point() +
  geom_text(vjust = -0.7, size = 2.5) +
  labs(title = "TIOBE vs PYPL rank (2026)",
       x = "TIOBE rank", y = "PYPL rank") +
  theme_minimal()

ggsave("results/fig2_scatter_2026.png", p2, width = 6, height = 5, dpi = 200)

# Create the third figure
p3 <- ggplot(acc, aes(method, MAPE, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Forecast error on 2026 hold-out (lower is better)",
       y = "MAPE (%)", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("results/fig3_forecast_accuracy.png", p3, width = 8, height = 4, dpi = 200)

# Create the fourth figure
hist_py <- df[df$language == "Python", c("year", "github_repositories")]

fc_py <- fc[fc$language == "Python" & fc$metric == "github_repositories", ]

p4 <- ggplot() +
  geom_line(data = hist_py, aes(year, github_repositories)) +
  geom_line(data = rbind(setNames(tail(hist_py, 1), c("year", "value")), fc_py[, c("year", "value")]),
            aes(year, value), linetype = "dashed", colour = "red") +
  labs(title = "Python: GitHub repositories, history and forecast",
       y = "Repositories", x = "Year") +
  theme_minimal()

ggsave("results/fig4_python_forecast.png", p4, width = 6, height = 4, dpi = 200)

# Print completion message
cat("\nDone. Tables and figures saved in the 'results' folder.\n")
