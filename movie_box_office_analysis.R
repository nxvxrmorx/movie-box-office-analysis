# ------------------------------------------------------------------------------
# Movie Box Office & Sequel Performance Analysis
# Question: do sequels earn more than original films, and does the gap survive
# once budget, era, genre and release season are accounted for?
#
# Data: TMDb 5000 Movie Dataset (public), cleaned to 2,515 English-language films,
#       1990-2016, budget >= $1M and revenue >= $1M. `sequel` = 1 if the film is a
#       later installment in a series (see write-up for the labeling rules).
# Input : movie_analysis_data.csv 
# NOTE  : packages used - ggplot2, sandwich, lmtest 
# ------------------------------------------------------------------------------
library(ggplot2)
library(sandwich)
library(lmtest)

movies <- read.csv("movie_analysis_data.csv", stringsAsFactors = FALSE)

# ---- 1. Prepare ---------------------------------------------------------------
movies$log_rev <- log(movies$revenue)
movies$log_bud <- log(movies$budget)
movies$yr_c    <- movies$year - 2003                    # centered year
movies$roi     <- movies$revenue / movies$budget
movies$type    <- factor(ifelse(movies$sequel == 1, "Sequel", "Original"),
                         levels = c("Original", "Sequel"))
movies$genre   <- relevel(factor(movies$genre),  ref = "Drama")
movies$season  <- relevel(factor(movies$season), ref = "Other")

# ---- 2. Descriptive statistics -------------------------------------------------
cat("\nFilms by type:\n"); print(table(movies$type))
desc <- aggregate(cbind(revenue, budget, roi) ~ type, data = movies, FUN = median)
cat("\nMedian revenue, budget and return on budget by type:\n"); print(desc)

# Distribution tests (revenue is heavily right-skewed, so use logs / ranks)
print(t.test(log_rev ~ type, data = movies))            # Welch t-test on log revenue
print(wilcox.test(revenue ~ type, data = movies))       # Mann-Whitney U

# ---- 3. Visualizations ---------------------------------------------------------
cols <- c(Original = "#1F3A5F", Sequel = "#D1722E")

p1 <- ggplot(movies, aes(type, revenue / 1e6, fill = type)) +
  geom_jitter(aes(color = type), width = 0.2, alpha = 0.15, size = 0.7) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.9) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
  labs(x = NULL, y = "Worldwide revenue ($M, log scale)",
       title = "Sequels earn more") +
  theme_minimal() + theme(legend.position = "none")

p2 <- ggplot(movies, aes(budget / 1e6, revenue / 1e6, color = type)) +
  geom_point(alpha = 0.4, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() + scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = cols) +
  labs(x = "Production budget ($M, log scale)",
       y = "Worldwide revenue ($M, log scale)",
       title = "...even at the same budget", color = NULL) +
  theme_minimal()

ggsave("figure_revenue_by_type.png", p1, width = 5, height = 4, dpi = 200)
ggsave("figure_budget_vs_revenue.png", p2, width = 6, height = 4, dpi = 200)

# ---- 4. Regression models ------------------------------------------------------
# Outcome is log(revenue), so a coefficient b means revenue is (exp(b) - 1) higher.
m1 <- lm(log_rev ~ sequel, data = movies)                                   # raw gap
m2 <- lm(log_rev ~ sequel + log_bud + yr_c, data = movies)                  # + budget, era
m3 <- lm(log_rev ~ sequel + log_bud + yr_c + genre + season, data = movies) # + genre, season

for (m in list(m1, m2, m3)) {
  s <- summary(m)
  b <- coef(m)["sequel"]; ci <- confint(m)["sequel", ]
  cat(sprintf("\nR2 = %.3f | adj R2 = %.3f | sequel effect = %+.0f%% (95%% CI %+.0f%% to %+.0f%%)\n",
              s$r.squared, s$adj.r.squared, 100 * (exp(b) - 1),
              100 * (exp(ci[1]) - 1), 100 * (exp(ci[2]) - 1)))
}
print(summary(m3))

# ---- 5. Diagnostics -------------------------------------------------------------
par(mfrow = c(2, 2)); plot(m3); par(mfrow = c(1, 1))     # residuals, Q-Q, leverage

# Multicollinearity among the main predictors (VIFs should be well under 5)
vif_of <- function(var, others) {
  1 / (1 - summary(lm(reformulate(others, var), data = movies))$r.squared)
}
cat("\nVIF sequel:",  round(vif_of("sequel",  c("log_bud", "yr_c")), 2),
    "| log_bud:",     round(vif_of("log_bud", c("sequel", "yr_c")), 2), "\n")

# Heteroskedasticity check -> use robust (HC3) standard errors if it is present
if (requireNamespace("lmtest", quietly = TRUE) && requireNamespace("sandwich", quietly = TRUE)) {
  print(lmtest::bptest(m3))
  print(lmtest::coeftest(m3, vcov = sandwich::vcovHC(m3, type = "HC3")))
}

# ---- 6. Overfitting check: 80/20 train/test split -------------------------------
set.seed(42)
idx   <- sample(nrow(movies), size = round(0.8 * nrow(movies)))
train <- movies[idx, ]; test <- movies[-idx, ]
mt    <- lm(formula(m3), data = train)
pred  <- predict(mt, newdata = test)
rmse  <- function(e) sqrt(mean(e^2))
cat(sprintf("\nTrain RMSE = %.3f | Test RMSE = %.3f | Test R2 = %.3f\n",
            sigma(mt), rmse(test$log_rev - pred),
            1 - sum((test$log_rev - pred)^2) / sum((test$log_rev - mean(test$log_rev))^2)))
