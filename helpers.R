# =====================================================================
# helpers.R  --  shared functions for analysis.R and app.R
# Project: Programming Language Trend Forecaster & Learning Advisor
# =====================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(forecast)       # holt() for the trend model
  library(randomForest)   # the ML model
})

# ---- STEP 1: constants -----------------------------------------------
DATA_PATH <- "C:/Users/anjal/OneDrive/Documents/Desktop/R project/data/global_programming_languages_dataset_2020_2026.csv"

METRIC_LABELS <- c(
  tiobe_rank              = "TIOBE rank (1 = best)",
  pypl_rank               = "PYPL rank (1 = best)",
  github_repositories     = "GitHub repositories",
  stackoverflow_questions = "Stack Overflow questions",
  average_salary_usd      = "Average salary (USD)",
  demand_score            = "Demand score"
)

# Continuous metrics we forecast (ranks barely move, so we do not forecast them)
FORECAST_METRICS <- c("github_repositories", "stackoverflow_questions", "average_salary_usd")

GOAL_LABELS <- c(
  ai_popularity_score   = "AI / Machine Learning",
  web_development_score = "Web Development",
  data_science_score    = "Data Science",
  mobile_score          = "Mobile Apps",
  game_dev_score        = "Game Development",
  embedded_score        = "Embedded / Systems"
)

# Columns that describe a language's "profile" (used as ML features)
STATIC_COLS <- c("ai_popularity_score", "web_development_score", "data_science_score",
                 "mobile_score", "game_dev_score", "embedded_score",
                 "demand_score", "learning_difficulty", "performance_score")

# ---- STEP 2: load + check the data ------------------------------------
load_data <- function(path = DATA_PATH) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  stopifnot(!anyNA(df))                                   # no missing values
  stopifnot(!any(duplicated(df[, c("language", "year")]))) # one row per language-year
  df[order(df$language, df$year), ]
}

# ---- STEP 3: source agreement (TIOBE vs PYPL) -------------------------
# Spearman is used (not Pearson) because ranks are ordinal.
agreement_by_year <- function(df) {
  df %>% group_by(year) %>%
    summarise(spearman = cor(tiobe_rank, pypl_rank, method = "spearman"),
              kendall  = cor(tiobe_rank, pypl_rank, method = "kendall"),
              .groups = "drop")
}

# gap = TIOBE rank - PYPL rank.
# gap > 0  -> PYPL ranks the language HIGHER than TIOBE does.
# gap < 0  -> TIOBE ranks it higher than PYPL does.
rank_gaps <- function(df) {
  df %>% mutate(gap = tiobe_rank - pypl_rank) %>%
    group_by(language) %>%
    summarise(mean_gap = mean(gap), mean_abs_gap = mean(abs(gap)), .groups = "drop") %>%
    arrange(desc(mean_abs_gap))
}

# ---- STEP 4: simple forecasting models --------------------------------
# y = values in year order, years = matching years, h = years ahead
forecast_series <- function(y, years, method, h = 1) {
  if (method == "naive") {                       # baseline: next year = last year
    return(rep(tail(y, 1), h))
  }
  if (method == "linear") {                      # straight-line trend
    fit <- lm(y ~ years)
    return(as.numeric(predict(fit, newdata = data.frame(years = max(years) + seq_len(h)))))
  }
  if (method == "holt") {                        # Holt's exponential trend
    fit <- forecast::holt(ts(y, start = min(years), frequency = 1), h = h)
    return(as.numeric(fit$mean))
  }
  stop("unknown method")
}

# ---- STEP 5: ML model (Random Forest on all languages together) -------
# Idea: learn how a language's yearly GROWTH depends on its size, its last
# growth and its profile (domain scores, demand, difficulty...).
# It is trained on all 29 languages at once (more data than one language alone).
rf_predict <- function(df, metric, origin_year, h = 1, seed = 42) {
  panel <- df %>% arrange(language, year) %>% group_by(language) %>%
    mutate(log_y      = log(.data[[metric]]),
           lag_growth = log_y - lag(log_y),          # growth in the previous year
           target     = lead(log_y) - log_y) %>%     # growth in the NEXT year
    ungroup() %>% as.data.frame()
  
  feats <- c("log_y", "lag_growth", STATIC_COLS)
  
  # train only on rows whose "next year" is already known at origin_year
  train <- panel[panel$year + 1 <= origin_year & !is.na(panel$lag_growth), ]
  set.seed(seed)
  rf <- randomForest::randomForest(x = train[, feats], y = train$target, ntree = 500)
  
  cur <- panel[panel$year == origin_year, ]
  out <- list()
  for (step in seq_len(h)) {                         # recursive multi-year forecast
    g <- as.numeric(predict(rf, cur[, feats]))
    cur$log_y      <- cur$log_y + g
    cur$lag_growth <- g
    out[[step]] <- data.frame(language = cur$language,
                              year = origin_year + step, value = exp(cur$log_y))
  }
  do.call(rbind, out)
}

# ---- STEP 6: hold-out test: train up to 2025, predict 2026 -------------
holdout_eval <- function(df, test_year = 2026) {
  rows <- list()
  for (metric in FORECAST_METRICS) {
    for (lang in unique(df$language)) {
      d      <- df[df$language == lang, ]
      train  <- d[d$year < test_year, ]
      actual <- d[d$year == test_year, metric]
      for (meth in c("naive", "linear", "holt")) {
        p <- forecast_series(train[[metric]], train$year, meth, h = 1)[1]
        rows[[length(rows) + 1]] <- data.frame(language = lang, metric = metric,
                                               method = meth, actual = actual, predicted = p)
      }
    }
    rf <- rf_predict(df, metric, origin_year = test_year - 1, h = 1)
    act <- df[df$year == test_year, c("language", metric)]
    names(act)[2] <- "actual"
    rows[[length(rows) + 1]] <- data.frame(language = rf$language, metric = metric, method = "random_forest",
                                           actual = act$actual[match(rf$language, act$language)],
                                           predicted = rf$value)
  }
  do.call(rbind, rows)
}

accuracy_table <- function(res) {
  res %>% mutate(err = predicted - actual) %>%
    group_by(metric, method) %>%
    summarise(MAE  = mean(abs(err)),
              RMSE = sqrt(mean(err^2)),
              MAPE = mean(abs(err) / actual) * 100,   # % error
              .groups = "drop") %>%
    arrange(metric, MAPE)
}

# best method per metric = lowest MAPE on the hold-out year
best_methods <- function(acc) {
  b <- acc %>% group_by(metric) %>% slice_min(MAPE, n = 1, with_ties = FALSE) %>% ungroup()
  setNames(b$method, b$metric)
}

# ---- STEP 7: final forecasts (2027, 2028) using the best method --------
forecast_all <- function(df, methods_by_metric, h = 2) {
  last_year <- max(df$year)
  rows <- list()
  for (metric in FORECAST_METRICS) {
    meth <- methods_by_metric[[metric]]
    if (meth == "random_forest") {
      f <- rf_predict(df, metric, origin_year = last_year, h = h)
      f$metric <- metric; f$method <- meth
      rows[[length(rows) + 1]] <- f
    } else {
      for (lang in unique(df$language)) {
        d <- df[df$language == lang, ]
        p <- forecast_series(d[[metric]], d$year, meth, h = h)
        rows[[length(rows) + 1]] <- data.frame(language = lang, year = last_year + seq_len(h),
                                               value = p, metric = metric, method = meth)
      }
    }
  }
  do.call(rbind, rows)
}

# ---- STEP 8: the Advisor ------------------------------------------------
rescale01 <- function(x) if (diff(range(x)) == 0) rep(0.5, length(x)) else (x - min(x)) / diff(range(x))

# score = w_goal * goal score + w_growth * forecast growth + w_salary * salary
# every part is scaled 0-1 first so the weights are comparable
advisor_scores <- function(df, fc, goal, w_goal = 0.4, w_growth = 0.3, w_salary = 0.3,
                           max_difficulty = 10,min_goal=30) {
  w <- c(w_goal, w_growth, w_salary); w <- w / sum(w)     # weights add up to 1
  latest <- df[df$year == max(df$year), ]
  last_repos <- latest$github_repositories[match(unique(fc$language), latest$language)]
  f <- fc[fc$metric == "github_repositories" & fc$year == max(fc$year), ]
  growth_pct <- (f$value / latest$github_repositories[match(f$language, latest$language)] - 1) * 100
  
  out <- data.frame(language = latest$language,
                    goal_score = latest[[goal]],
                    growth_pct = growth_pct[match(latest$language, f$language)],
                    salary = latest$average_salary_usd,
                    difficulty = latest$learning_difficulty)
  out$score <- 100 * (w[1] * rescale01(out$goal_score) +
                        w[2] * rescale01(out$growth_pct) +
                        w[3] * rescale01(out$salary))
  out$reason <- sprintf("%s score %.0f/100 | repos forecast %+.0f%% by %d | salary $%s",
                        GOAL_LABELS[[goal]], out$goal_score, out$growth_pct, max(fc$year),
                        format(round(out$salary, -2), big.mark = ","))
  out <- out[out$difficulty <= max_difficulty & out$goal_score>=min_goal, ]
  out[order(-out$score), ]
}