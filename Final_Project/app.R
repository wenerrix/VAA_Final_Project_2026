# app.R
# Fintech Customer Intelligence Dashboard - Skeleton Version
# ----------------------------------------------------------
# Purpose:
# 1) Build the full Shiny app structure first
# 2) Keep placeholders for later integration
# 3) Provide a clean dashboard shell for EDA, Clustering, Decision Tree, and Insights

# app.R
library(shiny)
library(shinydashboard)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(lubridate)
library(scales)
library(readr)
library(forcats)
library(rpart)
library(rpart.plot)
library(stringr)
library(tibble)

# =========================================================
# Configuration
# =========================================================
RDS_FILE <- "app_data.rds"

cat("Working dir =", getwd(), "\n")
cat("RDS_FILE =", RDS_FILE, "\n")
cat("File exists? ", file.exists(RDS_FILE), "\n")

if (!file.exists(RDS_FILE)) {
  stop("Cannot find app_data.rds in the app directory.")
}
# =========================================================
# Helper functions
# =========================================================
fmt_num <- function(x, digits = 0) {
  if (length(x) == 0 || is.na(x) || is.infinite(x)) return("NA")
  scales::comma(round(x, digits))
}

fmt_num_short <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x) || is.infinite(x)) return("NA")
  scales::label_number(
    accuracy = 10^(-digits),
    scale_cut = scales::cut_short_scale()
  )(x)
}

fmt_pct <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x) || is.infinite(x)) return("NA")
  scales::percent(x, accuracy = 10^(-digits) * 100)
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

metric_card <- function(title, value, subtitle = NULL) {
  tags$div(
    class = "metric-card",
    tags$div(class = "metric-title", title),
    tags$div(class = "metric-value", value),
    if (!is.null(subtitle)) tags$div(class = "metric-subtitle", subtitle)
  )
}

prepare_customer_data <- function(df) {
  date_cols <- intersect(
    c("first_tx", "last_tx", "last_survey_date", "last_transaction_date", "first_transaction_date"),
    names(df)
  )
  
  for (col in date_cols) {
    df[[col]] <- suppressWarnings(as.Date(df[[col]]))
  }
  
  logical_cols <- names(df)[vapply(df, is.logical, logical(1))]
  if (length(logical_cols) > 0) {
    df[logical_cols] <- lapply(df[logical_cols], function(x) {
      factor(
        ifelse(is.na(x), "Unknown", ifelse(x, "Yes", "No")),
        levels = c("No", "Yes", "Unknown")
      )
    })
  }
  
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  if (length(char_cols) > 0) {
    df[char_cols] <- lapply(df[char_cols], function(x) {
      x <- trimws(x)
      x[is.na(x) | x == ""] <- "Unknown"
      factor(x)
    })
  }
  
  if ("clv_segment" %in% names(df)) {
    df$clv_segment <- factor(
      as.character(df$clv_segment),
      levels = c("Bronze", "Silver", "Gold", "Platinum")
    )
  }
  
  df
}

# --- 新增：统一的全局稳健缺失值插补逻辑 ---
robust_impute <- function(df) {
  exclude_cols <- c("customer_id", "first_tx", "last_tx", "last_survey_date",
                    "last_transaction_date", "first_transaction_date",
                    "first_tx_date_tx", "last_tx_date_tx", "date", "month")
  
  for (v in names(df)) {
    if (v %in% exclude_cols) next
    x <- df[[v]]
    if (inherits(x, "Date") || inherits(x, "POSIXt")) next
    
    if (is.logical(x)) {
      x <- factor(ifelse(is.na(x), "Unknown", ifelse(x, "Yes", "No")))
    } else if (is.character(x) || is.factor(x)) {
      x <- as.character(x)
      x[is.na(x) | trimws(x) == ""] <- "Unknown"
      x <- factor(x)
    } else if (is.numeric(x) || is.integer(x)) {
      med <- suppressWarnings(stats::median(x, na.rm = TRUE))
      if (!is.finite(med)) med <- 0
      x[is.na(x)] <- med
    }
    df[[v]] <- x
  }
  df
}
# ------------------------------------------

prepare_transactions <- function(df) {
  df$date <- suppressWarnings(as.Date(df$date))
  df$month <- as.Date(format(df$date, "%Y-%m-01"))
  df$type <- factor(df$type)
  df
}

build_master_dataset <- function(customer_df, tx_customer_summary, tx_type_summary) {
  customer_df %>%
    left_join(tx_customer_summary, by = "customer_id") %>%
    left_join(tx_type_summary, by = "customer_id")
}

get_predictor_sets <- function(df) {
  demographic <- intersect(c(
    "age", "gender", "location", "income_bracket", "occupation",
    "education_level", "marital_status", "household_size", "acquisition_channel"
  ), names(df))
  
  product <- intersect(c(
    "savings_account", "credit_card", "personal_loan", "investment_account",
    "insurance_product", "active_products", "bill_payment_user", "auto_savings_enabled"
  ), names(df))
  
  engagement <- intersect(c(
    "app_logins_frequency", "feature_usage_diversity", "international_transactions",
    "failed_transactions", "tx_count", "avg_tx_value", "total_tx_volume",
    "monthly_transaction_count", "average_transaction_value", "transaction_frequency",
    "weekend_transaction_ratio", "avg_daily_transactions", "customer_tenure",
    "preferred_transaction_type"
  ), names(df))
  
  service <- intersect(c(
    "base_satisfaction", "tx_satisfaction", "product_satisfaction",
    "satisfaction_score", "nps_score", "support_tickets_count",
    "resolved_tickets_ratio", "app_store_rating", "feedback_sentiment",
    "feature_requests", "complaint_topics", "churn_probability"
  ), names(df))
  
  all_full <- unique(c(demographic, product, engagement, service))
  
  list(
    "Demographics" = demographic,
    "Product Holding" = product,
    "Engagement & Transaction" = engagement,
    "Service & Satisfaction" = service,
    "Full Model" = all_full
  )
}

run_clustering <- function(df, features, k, sample_n) {
  vars <- intersect(features, names(df))
  if (length(vars) < 2) return(NULL)
  
  tmp <- df %>%
    dplyr::select(customer_id, customer_segment, clv_segment, dplyr::all_of(vars)) %>%
    tidyr::drop_na() # 数据已被全局插补，此处防极少数边角情况
  
  if (nrow(tmp) < k + 5) return(NULL)
  
  # 统一控制随机种子
  if (nrow(tmp) > sample_n) {
    set.seed(123)
    tmp <- dplyr::slice_sample(tmp, n = sample_n)
  }
  
  x <- tmp %>% dplyr::select(dplyr::all_of(vars))
  x[] <- lapply(x, as.numeric)
  
  set.seed(123) # K-means 收敛也需固定 Seed
  x_scaled <- scale(x)
  km <- stats::kmeans(x_scaled, centers = k, nstart = 20)
  pc <- stats::prcomp(x_scaled)
  
  out <- tmp %>%
    dplyr::mutate(
      cluster = factor(paste("Cluster", km$cluster)),
      PC1 = pc$x[, 1],
      PC2 = pc$x[, 2]
    )
  
  profile <- out %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      dplyr::across(dplyr::all_of(vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  profile_long <- profile %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "feature",
      values_to = "mean_value"
    ) %>%
    dplyr::group_by(feature) %>%
    dplyr::mutate(
      std_value = ifelse(stats::sd(mean_value, na.rm = TRUE) == 0, 0,
                         (mean_value - mean(mean_value, na.rm = TRUE)) /
                           stats::sd(mean_value, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()
  
  list(
    data = out,
    profile = profile,
    profile_long = profile_long,
    vars = vars
  )
}

fit_tree_model <- function(df, predictors, maxdepth, minsplit, cp, train_ratio, seed) {
  vars <- intersect(predictors, names(df))
  vars <- setdiff(vars, c(
    "customer_lifetime_value", "clv_segment", "customer_id",
    "first_tx", "last_tx", "last_survey_date", "last_transaction_date",
    "first_transaction_date", "first_tx_date_tx", "last_tx_date_tx"
  ))
  if (length(vars) == 0 || !"clv_segment" %in% names(df)) return(NULL)
  
  tmp <- df %>%
    dplyr::select(clv_segment, dplyr::all_of(vars)) %>%
    dplyr::filter(!is.na(clv_segment))
  
  if (nrow(tmp) < 200) return(NULL)
  
  compress_levels <- function(x, max_levels = 10) {
    x <- as.character(x)
    x[is.na(x) | trimws(x) == ""] <- "Unknown"
    if (length(unique(x)) <= max_levels) return(factor(x))
    tab <- sort(table(x), decreasing = TRUE)
    keep <- names(tab)[seq_len(min(max_levels - 1, length(tab)))]
    x[!(x %in% keep)] <- "Other"
    factor(x)
  }
  
  kept <- character()
  for (v in vars) {
    x <- tmp[[v]]
    # 基础 NA 插补已经由全局 robust_impute 处理完成，这里只需针对 rpart 模型做高维因子压缩
    if (inherits(x, "Date")) x <- as.numeric(x)
    if (is.character(x) || is.factor(x)) {
      x <- compress_levels(x, max_levels = 10)
    }
    if (length(unique(stats::na.omit(x))) <= 1) next
    tmp[[v]] <- x
    kept <- c(kept, v)
  }
  
  if (length(kept) == 0) return(NULL)
  tmp <- tmp %>% dplyr::select(clv_segment, dplyr::all_of(kept))
  if (length(unique(tmp$clv_segment)) < 2) return(NULL)
  
  # 模型训练保留 15000 行高上限，保证模型学习效果
  if (nrow(tmp) > 15000) {
    set.seed(seed)
    per_class <- max(50, floor(15000 / length(unique(tmp$clv_segment))))
    tmp <- tmp %>%
      dplyr::group_by(clv_segment) %>%
      dplyr::group_modify(~ {
        if (nrow(.x) <= per_class) .x else dplyr::slice_sample(.x, n = per_class)
      }) %>%
      dplyr::ungroup()
  }
  
  set.seed(seed)
  idx <- sample.int(nrow(tmp), size = max(100, floor(train_ratio * nrow(tmp))))
  idx <- unique(idx)
  train <- tmp[idx, , drop = FALSE]
  test <- tmp[-idx, , drop = FALSE]
  if (nrow(test) < 50) return(NULL)
  
  form <- stats::as.formula(paste("clv_segment ~", paste(kept, collapse = " + ")))
  model <- tryCatch(
    rpart::rpart(
      form,
      data = train,
      method = "class",
      control = rpart::rpart.control(maxdepth = maxdepth, minsplit = minsplit, cp = cp, xval = 5)
    ),
    error = function(e) NULL
  )
  if (is.null(model)) return(NULL)
  
  pred <- tryCatch(stats::predict(model, test, type = "class"), error = function(e) NULL)
  if (is.null(pred)) return(NULL)
  
  lvl <- levels(tmp$clv_segment)
  conf_df <- as.data.frame(table(
    Actual = factor(test$clv_segment, levels = lvl),
    Predicted = factor(pred, levels = lvl)
  ))
  conf_df$Percent <- ave(conf_df$Freq, conf_df$Actual, FUN = function(x) {
    if (sum(x) == 0) rep(0, length(x)) else x / sum(x)
  })
  
  class_recall <- conf_df %>%
    dplyr::group_by(Actual) %>%
    dplyr::summarise(
      recall = ifelse(sum(Freq) == 0, NA_real_, sum(Freq[Actual == Predicted]) / sum(Freq)),
      support = sum(Freq),
      .groups = "drop"
    )
  
  importance_tbl <- if (!is.null(model$variable.importance) && length(model$variable.importance) > 0) {
    tibble::tibble(variable = names(model$variable.importance), importance = as.numeric(model$variable.importance)) %>%
      dplyr::arrange(dplyr::desc(importance))
  } else {
    tibble::tibble(variable = character(), importance = numeric())
  }
  
  rules_txt <- tryCatch(capture.output(rpart.plot::rpart.rules(model, roundint = FALSE)),
                        error = function(e) "Rule extraction unavailable for this tree.")
  
  frame_tbl <- model$frame
  leaf_tbl <- tibble::rownames_to_column(frame_tbl, var = "node") %>%
    dplyr::filter(var == "<leaf>") %>%
    dplyr::transmute(node = node, n = n, predicted_class = model$ylevels[yval], loss = dev, complexity = complexity)
  
  list(
    model = model,
    accuracy = mean(pred == test$clv_segment),
    conf_df = conf_df,
    recall_df = class_recall,
    importance_tbl = importance_tbl,
    rules_txt = rules_txt,
    leaf_tbl = leaf_tbl,
    train_n = nrow(train),
    test_n = nrow(test),
    terminal_nodes = sum(model$frame$var == "<leaf>"),
    predictors = kept,
    prepared_rows = nrow(tmp)
  )
}

segment_recommendations <- function(df, segment_name) {
  seg <- df %>% dplyr::filter(as.character(customer_segment) == segment_name)
  if (nrow(seg) == 0) return("No customers available in this segment under the current filters.")
  
  overall <- df
  recs <- c()
  
  if (safe_mean(seg$churn_probability) > safe_mean(overall$churn_probability)) {
    recs <- c(recs, "Retention should be prioritized through targeted outreach, service recovery, and personalised reminders.")
  }
  if (safe_mean(seg$satisfaction_score) < safe_mean(overall$satisfaction_score)) {
    recs <- c(recs, "Customer experience needs intervention: review complaint topics, reduce friction in high-failure journeys, and improve support resolution quality.")
  }
  if (safe_mean(seg$active_products) < safe_mean(overall$active_products)) {
    recs <- c(recs, "Cross-sell opportunities remain underdeveloped. Bundle savings, payments, and protection products to deepen wallet share.")
  }
  if (safe_mean(seg$app_logins_frequency) < safe_mean(overall$app_logins_frequency)) {
    recs <- c(recs, "Digital engagement is below average. Use in-app nudges, lifestyle offers, and guided feature adoption campaigns.")
  }
  if (segment_name == "power") {
    recs <- c(recs, "Power users should receive premium treatment, loyalty perks, and early access to advanced features to defend high-value relationships.")
  }
  if (segment_name == "inactive") {
    recs <- c(recs, "Inactive users need reactivation journeys, low-friction onboarding refreshers, and small incentives to restart behaviour.")
  }
  if (segment_name == "occasional") {
    recs <- c(recs, "Occasional users can be moved upward through habit-forming payment use cases and recurring transaction prompts.")
  }
  if (segment_name == "regular") {
    recs <- c(recs, "Regular users are good candidates for product deepening and targeted upsell based on usage intensity and life-stage signals.")
  }
  if (length(recs) == 0) {
    recs <- "This segment is relatively healthy under the current filters. Maintain engagement and monitor changes in churn, satisfaction, and product depth."
  }
  recs
}

# =========================================================
# Load prepared data from RDS
# =========================================================
app_data <- readRDS(RDS_FILE)

customer_raw <- app_data$customer_raw
transactions_raw <- app_data$transactions_raw

customer_data <- app_data$customer_data
transactions <- app_data$transactions

tx_customer_summary <- app_data$tx_customer_summary
tx_type_summary <- app_data$tx_type_summary
tx_volatility <- app_data$tx_volatility
tx_month_summary <- app_data$tx_month_summary
master_dataset <- app_data$master_dataset

predictor_sets <- app_data$predictor_sets
cluster_feature_choices <- app_data$cluster_feature_choices
default_cluster_vars <- app_data$default_cluster_vars
correlation_vars <- app_data$correlation_vars

all_income <- app_data$all_income
all_segment <- app_data$all_segment
all_clv <- app_data$all_clv
all_location <- app_data$all_location
all_tx_type <- app_data$all_tx_type
min_tx_date <- app_data$min_tx_date
max_tx_date <- app_data$max_tx_date

# =========================================================
# UI helpers
# =========================================================
section_box <- function(title, theme = c("blue", "green", "red", "gold"), ...) {
  theme <- match.arg(theme)
  tags$div(
    class = paste("section-box", paste0("theme-", theme)),
    tags$div(class = "section-box-header", title),
    tags$div(class = "section-box-body", ...)
  )
}

sidebar_filters_ui <- function() {
  tags$div(
    class = "filter-shell",
    tags$details(
      class = "filter-details",
      open = "open",
      tags$summary(HTML("<i class='fa fa-filter'></i> Global Reactive Filters")),
      tags$div(
        class = "filter-inner",
        selectInput("gender_filter", "Gender", choices = c("All", sort(unique(as.character(customer_data$gender)))), selected = "All"),
        selectizeInput("income_filter", "Income Bracket", choices = all_income, selected = all_income, multiple = TRUE, options = list(plugins = list("remove_button"))),
        selectizeInput("segment_filter", "Customer Segment", choices = all_segment, selected = all_segment, multiple = TRUE, options = list(plugins = list("remove_button"))),
        selectizeInput("clv_filter", "CLV Segment", choices = all_clv, selected = all_clv, multiple = TRUE, options = list(plugins = list("remove_button"))),
        selectizeInput("location_filter", "Location", choices = all_location, selected = all_location, multiple = TRUE, options = list(plugins = list("remove_button"))),
        checkboxGroupInput("tx_type_filter", "Transaction Types", choices = all_tx_type, selected = all_tx_type, inline = FALSE),
        dateRangeInput("date_filter", "Transaction Date Range", start = min_tx_date, end = max_tx_date, min = min_tx_date, max = max_tx_date),
        actionButton("reset_filters", "Reset Filters", class = "btn-reset-filters")
      )
    )
  )
}

cluster_controls_panel <- function() {
  section_box(
    "Clustering Controls", "green",
    selectizeInput("cluster_vars", "Variables for K-means", choices = cluster_feature_choices, selected = default_cluster_vars, multiple = TRUE, options = list(plugins = list("remove_button"))),
    selectInput("cluster_compare_metric", "Metric for Cluster Comparison", choices = default_cluster_vars),
    sliderInput("k_clusters", "Number of Clusters", min = 2, max = 6, value = 4, step = 1),
    sliderInput("cluster_sample_n", "Max Rows Used for Clustering", min = 2000, max = 15000, value = 8000, step = 1000),
    actionButton("run_cluster", "Run Clustering", class = "btn-green"),
    tags$p(class = "small-note", "Customer-level attributes are enriched with transaction summaries, type shares, and monthly volatility features before clustering.")
  )
}

hero_banner <- function() {
  tags$div(
    class = "hero-banner",
    tags$div(
      class = "hero-kicker",
      "COFINFAD · Colombian Fintech Financial Analytics Dataset"
    ),
    tags$div(
      class = "hero-main",
      tags$div(
        class = "hero-copy",
        tags$h2("DASHBOARD"),
        tags$p(""),
        tags$div(
          class = "hero-chip-row",
          tags$span(class = "hero-chip", "48,723 customers"),
          tags$span(class = "hero-chip", "3,159,157 transactions"),
          tags$span(class = "hero-chip", "57 variables"),
          tags$span(class = "hero-chip", "Jan–Dec 2023")
        )
      )
    )
  )
}

# =========================================================
# UI
# =========================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = tags$div(class = "brand-wrap", span(class = "brand-title", "COFINFAD Fintech"), span(class = "brand-subtitle", "Customer Intelligence Dashboard")), titleWidth = 360),
  dashboardSidebar(
    width = 330,
    sidebarMenu(
      id = "nav_tabs",
      menuItem("Dashboard", icon = icon("dashboard"), startExpanded = TRUE, menuSubItem("KPI Cards", tabName = "dash_kpi", selected = TRUE), menuSubItem("Customer Snapshot", tabName = "dash_customer"), menuSubItem("Segment Snapshot", tabName = "dash_segment")),
      menuItem("Data Analysis", icon = icon("chart-bar"), startExpanded = FALSE, menuSubItem("Customer Profile", tabName = "analysis_profile"), menuSubItem("Transaction Behavior", tabName = "analysis_tx"), menuSubItem("CLV / Churn Analysis", tabName = "analysis_clv")),
      menuItem("Clustering", icon = icon("sitemap"), startExpanded = FALSE, menuSubItem("Cluster Overview", tabName = "cluster_overview"), menuSubItem("Segment Profiles", tabName = "cluster_profiles"), menuSubItem("Cluster Comparison", tabName = "cluster_compare")),
      menuItem("CLV Decision Tree", icon = icon("random"), startExpanded = FALSE, menuSubItem("Model Setup", tabName = "tree_setup"), menuSubItem("Tree Plot", tabName = "tree_plot_tab"), menuSubItem("Performance", tabName = "tree_perf"), menuSubItem("Rules / Terminal Nodes", tabName = "tree_rules")),
      menuItem("Business Insights", icon = icon("lightbulb"), startExpanded = FALSE, menuSubItem("Key Findings", tabName = "biz_findings"), menuSubItem("Segment Actions", tabName = "biz_actions"), menuSubItem("CLV Driver Summary", tabName = "biz_drivers")),
      menuItem("Shared Data Backbone", icon = icon("database"), startExpanded = FALSE, menuSubItem("Customer Data", tabName = "data_customer"), menuSubItem("Transaction Data", tabName = "data_tx"), menuSubItem("Master Dataset", tabName = "data_master"), menuSubItem("Reactive Filters", tabName = "data_filters")),
      menuItem("About", tabName = "about_page", icon = icon("info-circle"))
    ),
    sidebar_filters_ui()
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
        body, .content-wrapper, .right-side { background: linear-gradient(180deg, #edf3f8 0%, #f6f9fc 100%); font-family: 'Inter', 'Segoe UI', Arial, sans-serif; color: #10233c; }
        .main-header .logo, .main-header .navbar { background: rgba(255,255,255,.92) !important; color: #10233c !important; box-shadow: 0 4px 18px rgba(16,35,60,.08); backdrop-filter: blur(8px); }
        .main-header .logo { font-weight: 900; font-size: 18px; }
        .brand-wrap { display: flex; flex-direction: column; line-height: 1.05; padding-top: 4px; }
        .brand-title { font-weight: 900; font-size: 19px; letter-spacing: .2px; color: #0f2744; }
        .brand-subtitle { font-size: 11px; font-weight: 700; color: #4f6b8a; text-transform: uppercase; letter-spacing: .12em; }
        .main-header .navbar .sidebar-toggle, .main-header .navbar .sidebar-toggle:hover { color: #10233c !important; background: transparent !important; }
        .main-sidebar, .left-side { background: linear-gradient(180deg, #0b1f38 0%, #11345b 42%, #0e7c86 100%) !important; }
        .sidebar { padding-top: 10px; }
        .sidebar-menu > li > a { color: rgba(255,255,255,.95) !important; font-weight: 700; font-size: 15px; padding: 14px 16px; }
        .sidebar-menu .treeview-menu > li > a { color: rgba(255,255,255,.84) !important; font-size: 14px; padding: 11px 16px 11px 32px; }
        .sidebar-menu > li.active > a, .sidebar-menu > li:hover > a, .sidebar-menu .treeview-menu > li.active > a, .sidebar-menu .treeview-menu > li > a:hover { background: rgba(255,255,255,.12) !important; color: #ffffff !important; border-left: 4px solid #f0c24f; }
        .sidebar-menu .treeview-menu { background: rgba(255,255,255,.04) !important; }
        .sidebar-menu .treeview-menu > li > a { border-left: 4px solid transparent; }
        .sidebar-menu > li.header { color: rgba(255,255,255,.65) !important; background: transparent !important; }
        .content-wrapper, .right-side { min-height: calc(100vh - 50px); }
        .content { padding: 24px 24px 30px; }

        .hero-banner { margin: 0 0 22px; padding: 24px 28px; border-radius: 24px; background: linear-gradient(120deg, #0b213c 0%, #124c73 52%, #0f8a8e 100%); box-shadow: 0 18px 40px rgba(13,46,87,.18); color: #ffffff; position: relative; overflow: hidden; }
        .hero-banner:before, .hero-banner:after { content: ''; position: absolute; border-radius: 50%; background: rgba(255,255,255,.08); }
        .hero-banner:before { width: 260px; height: 260px; right: -60px; top: -100px; }
        .hero-banner:after { width: 180px; height: 180px; right: 180px; bottom: -80px; }
        .hero-kicker { font-size: 12px; text-transform: uppercase; letter-spacing: .18em; font-weight: 800; opacity: .84; margin-bottom: 10px; }
        .hero-main { display: flex; align-items: stretch; justify-content: space-between; gap: 24px; position: relative; z-index: 2; }
        .hero-copy { max-width: 760px; }
        .hero-copy h2 { margin: 0 0 10px; font-size: clamp(28px, 3vw, 42px); font-weight: 900; line-height: 1.04; }
        .hero-copy p { margin: 0; max-width: 760px; font-size: 15px; line-height: 1.7; color: rgba(255,255,255,.92); }
        .hero-chip-row { margin-top: 16px; display: flex; flex-wrap: wrap; gap: 10px; }
        .hero-chip { display: inline-flex; align-items: center; padding: 8px 12px; border-radius: 999px; background: rgba(255,255,255,.14); color: #ffffff; font-size: 12px; font-weight: 700; border: 1px solid rgba(255,255,255,.12); }
        .hero-stat-panel { min-width: 230px; background: rgba(255,255,255,.14); border: 1px solid rgba(255,255,255,.14); border-radius: 20px; padding: 18px 18px 16px; box-shadow: inset 0 1px 0 rgba(255,255,255,.08); }
        .hero-stat-label { font-size: 12px; letter-spacing: .12em; text-transform: uppercase; font-weight: 800; opacity: .82; }
        .hero-stat-value { font-size: 30px; line-height: 1.05; font-weight: 900; margin: 8px 0 10px; }
        .hero-stat-note { font-size: 13px; line-height: 1.6; color: rgba(255,255,255,.9); }

        .filter-shell { padding: 18px 12px 18px; }
        .filter-details { border: 1px solid rgba(255,255,255,.12); border-radius: 18px; background: rgba(255,255,255,.08); overflow: hidden; box-shadow: 0 10px 24px rgba(0,0,0,.16); }
        .filter-details summary { list-style: none; cursor: pointer; color: #ffffff; font-weight: 800; padding: 18px 20px; font-size: 16px; display: flex; align-items: center; gap: 10px; }
        .filter-details summary::-webkit-details-marker { display: none; }
        .filter-inner { padding: 2px 18px 18px; color: #ffffff; }
        .filter-inner .form-group > label, .filter-inner .control-label, .filter-inner .checkbox label { color: #ffffff; font-weight: 700; }
        .filter-inner .form-control, .filter-inner .selectize-input, .filter-inner .selectize-control.multi .selectize-input, .filter-inner .date { border-radius: 14px !important; border: none !important; min-height: 44px; box-shadow: none !important; }
        .filter-inner .selectize-dropdown, .filter-inner .selectize-input.full { color: #10233c; }
        .filter-inner .checkbox { margin-top: 0; }
        .btn-reset-filters { width: 100%; border-radius: 12px; background: linear-gradient(135deg, #f0c24f 0%, #e0a929 100%); border: none; color: #10233c; font-weight: 800; padding: 10px 12px; box-shadow: 0 10px 18px rgba(224,169,41,.22); }
        .btn-green { background: linear-gradient(135deg, #138c90 0%, #0e7078 100%); color: #fff; border: none; border-radius: 12px; font-weight: 800; box-shadow: 0 10px 18px rgba(14,112,120,.18); }
        .btn-default, .btn, .download-btn, .btn-primary { border-radius: 12px; }

        .section-box { background: rgba(255,255,255,.96); border-radius: 22px; overflow: hidden; margin-bottom: 24px; box-shadow: 0 16px 34px rgba(16,35,60,.08); border: 1px solid rgba(16,35,60,.06); }
        .section-box-header { padding: 17px 22px; color: #ffffff; font-size: 17px; font-weight: 900; letter-spacing: .2px; }
        .section-box-body { padding: 18px 18px 14px; background: rgba(255,255,255,.98); }
        .theme-blue { border-color: rgba(25,104,144,.18); }
        .theme-blue .section-box-header { background: linear-gradient(135deg, #103c67 0%, #14638d 100%); }
        .theme-green { border-color: rgba(14,112,120,.18); }
        .theme-green .section-box-header { background: linear-gradient(135deg, #0e7078 0%, #12a39d 100%); }
        .theme-red { border-color: rgba(179,83,66,.18); }
        .theme-red .section-box-header { background: linear-gradient(135deg, #a24638 0%, #d06b4e 100%); }
        .theme-gold { border-color: rgba(224,169,41,.18); }
        .theme-gold .section-box-header { background: linear-gradient(135deg, #d59a16 0%, #f0c24f 100%); color: #10233c; }

        .metric-card { color: #ffffff; border-radius: 22px; padding: 24px 24px; min-height: 150px; box-shadow: 0 14px 30px rgba(16,35,60,.12); margin-bottom: 24px; overflow: hidden; position: relative; }
        .metric-card:after { content: ''; position: absolute; width: 130px; height: 130px; border-radius: 50%; right: -30px; top: -26px; background: rgba(255,255,255,.10); }
        .metric-title { font-size: 14px; font-weight: 800; opacity: .96; margin-bottom: 10px; text-transform: uppercase; letter-spacing: .06em; }
        .metric-value { font-size: clamp(30px, 2.4vw, 52px); font-weight: 900; line-height: 1; letter-spacing: -.8px; margin-bottom: 10px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; position: relative; z-index: 2; }
        .metric-subtitle { font-size: 13px; opacity: .92; position: relative; z-index: 2; }
        .shiny-table table { width: 100%; background: #ffffff; border-collapse: collapse; }
        .shiny-table th { background: #f5f7fb; color: #10233c; font-weight: 800; padding: 10px 12px; border-bottom: 1px solid #d9e2ef; }
        .shiny-table td { padding: 10px 12px; border-bottom: 1px solid #eef2f7; }
        .metric-slot.blue .metric-card { background: linear-gradient(135deg, #103c67 0%, #1e7ca7 100%); }
        .metric-slot.green .metric-card { background: linear-gradient(135deg, #0e7078 0%, #11a79f 100%); }
        .metric-slot.gold .metric-card { background: linear-gradient(135deg, #d59a16 0%, #f0c24f 100%); color: #10233c; }
        .metric-slot.red .metric-card { background: linear-gradient(135deg, #a24638 0%, #d06b4e 100%); }
        .metric-slot.gold .metric-title, .metric-slot.gold .metric-subtitle, .metric-slot.gold .metric-value { color: #10233c; }

        .insight-box { background: linear-gradient(180deg, #ffffff 0%, #f9fbfd 100%); border-left: 4px solid #0e7078; border-radius: 16px; padding: 16px 18px; margin-bottom: 14px; box-shadow: 0 8px 18px rgba(16,35,60,.05); }
        .small-note { color: #64748b; font-size: 12px; line-height: 1.55; margin-top: 8px; }
        .summary-chip { display: inline-block; background: #ebf3f5; color: #10233c; border-radius: 999px; padding: 7px 10px; margin: 0 6px 6px 0; font-size: 12px; font-weight: 700; border: 1px solid rgba(16,35,60,.06); }

        .plot-placeholder, .text-placeholder { min-height: 280px; display: flex; align-items: center; justify-content: center; color: #8a94a6; font-size: 16px; }
        .dataTables_wrapper .dataTables_length select, .dataTables_wrapper .dataTables_filter input { border-radius: 10px; border: 1px solid #d9e2ef; }
        .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: #0f8a8e !important; color: #ffffff !important; border: none !important; }
        .tab-content > .tab-pane { animation: fadeIn .25s ease-out; }
        @keyframes fadeIn { from { opacity: 0; transform: translateY(4px); } to { opacity: 1; transform: translateY(0); } }
        .box { border-top: 0 !important; }
        @media (max-width: 1024px) { .hero-main { flex-direction: column; } .hero-stat-panel { min-width: 100%; } }
      "))),
      hero_banner(),
      tabItems(
      tabItem(tabName = "dash_kpi", fluidRow(column(3, div(class = "metric-slot blue", uiOutput("kpi_customers"))), column(3, div(class = "metric-slot green", uiOutput("kpi_avg_clv"))), column(3, div(class = "metric-slot gold", uiOutput("kpi_avg_churn"))), column(3, div(class = "metric-slot red", uiOutput("kpi_total_tx")))), fluidRow(column(8, section_box("Monthly Transaction Amount", "blue", plotOutput("dashboard_monthly_amount", height = 360))), column(4, section_box("Transaction Type Mix", "blue", plotOutput("dashboard_tx_mix", height = 360)))), fluidRow(column(12, section_box("Current Filter Snapshot", "gold", uiOutput("filter_summary_ui"))))),
      tabItem(tabName = "dash_customer", fluidRow(column(6, section_box("Age Distribution", "blue", plotOutput("profile_age_plot", height = 320))), column(6, section_box("Income Bracket Composition", "blue", plotOutput("profile_income_plot", height = 320)))), fluidRow(column(6, section_box("Gender Composition", "blue", plotOutput("profile_gender_plot", height = 320))), column(6, section_box("Top Occupations", "blue", plotOutput("profile_occupation_plot", height = 320))))),
      tabItem(tabName = "dash_segment", fluidRow(column(4, section_box("Segment Size", "blue", plotOutput("segment_size_plot", height = 320))), column(4, section_box("Average CLV by Segment", "blue", plotOutput("segment_clv_plot", height = 320))), column(4, section_box("Average Churn by Segment", "blue", plotOutput("segment_churn_plot", height = 320))))),
      tabItem(tabName = "analysis_profile", fluidRow(column(6, section_box("Gender x Income Heatmap", "blue", plotOutput("profile_heatmap", height = 340))), column(6, section_box("Average CLV by Acquisition Channel", "blue", plotOutput("channel_clv_plot", height = 340)))), fluidRow(column(6, section_box("Satisfaction by CLV Segment", "blue", plotOutput("satisfaction_clv_plot", height = 330))), column(6, section_box("Tenure by Customer Segment", "blue", plotOutput("tenure_segment_plot", height = 330))))),
      tabItem(tabName = "analysis_tx", fluidRow(column(6, section_box("Monthly Transaction Count", "blue", plotOutput("monthly_count_plot", height = 330))), column(6, section_box("Monthly Transaction Amount by Type", "blue", plotOutput("monthly_amount_type_plot", height = 330)))), fluidRow(column(6, section_box("Weekend Transaction Ratio by Segment", "blue", plotOutput("weekend_ratio_plot", height = 330))), column(6, section_box("Avg Daily Transactions vs Tenure", "blue", plotOutput("daily_tx_tenure_plot", height = 330))))),
      tabItem(tabName = "analysis_clv", fluidRow(column(8, section_box("CLV vs Churn Probability", "blue", plotOutput("clv_churn_scatter", height = 390))), column(4, section_box("Driver Correlations", "blue", selectInput("corr_target", "Correlation Target", choices = c("Customer Lifetime Value" = "customer_lifetime_value", "Churn Probability" = "churn_probability")), plotOutput("driver_corr_plot", height = 330)))), fluidRow(column(12, section_box("Top Correlation Table", "blue", DTOutput("driver_corr_tbl"))))),
      tabItem(tabName = "cluster_overview", fluidRow(column(4, cluster_controls_panel()), column(8, section_box("Cluster Overview", "green", plotOutput("cluster_scatter_plot", height = 420)), section_box("Cluster Size", "green", DTOutput("cluster_size_tbl"))))),
      tabItem(tabName = "cluster_profiles", fluidRow(column(4, section_box("Current Clustering Note", "green", tags$p("Use the Cluster Overview page to update variables, cluster count, and sampling settings before reviewing profile patterns."))), column(8, section_box("Cluster Heatmap (Standardised Means)", "green", plotOutput("cluster_heatmap_plot", height = 430)), section_box("Cluster Profile Table", "green", DTOutput("cluster_profile_tbl"))))),
      tabItem(tabName = "cluster_compare", fluidRow(column(4, section_box("Comparison Metric", "green", tags$p("The comparison chart below uses the metric currently selected in the clustering controls."))), column(8, section_box("Selected Metric by Cluster", "green", plotOutput("cluster_compare_plot", height = 420)), section_box("Segment Mix inside Each Cluster", "green", DTOutput("cluster_mix_tbl"))))),
      tabItem(tabName = "tree_setup", fluidRow(column(4, section_box("Model Controls", "red", selectInput("predictor_group", "Predictor Group", choices = names(predictor_sets), selected = "Full Model"), sliderInput("tree_maxdepth", "Maximum Tree Depth", min = 2, max = 8, value = 4, step = 1), sliderInput("tree_minsplit", "Minimum Split Size", min = 10, max = 100, value = 30, step = 5), sliderInput("tree_cp", "Complexity Parameter (cp)", min = 0.000, max = 0.050, value = 0.010, step = 0.001), sliderInput("tree_train_ratio", "Train/Test Split", min = 0.50, max = 0.90, value = 0.75, step = 0.05), numericInput("tree_seed", "Random Seed", value = 123, min = 1), actionButton("run_tree", "Train Decision Tree", class = "btn-primary"), tags$p(class = "small-note", "Target variable is clv_segment. customer_lifetime_value is excluded from the predictor set to avoid leakage."))), column(8, section_box("Model Setup Notes", "red", uiOutput("tree_status_ui"))))),
      tabItem(tabName = "tree_plot_tab", fluidRow(column(3, section_box("Workflow Note", "red", tags$p("Train the model in Model Setup first. The fitted tree below uses the current filtered cohort and selected predictor group."))), column(9, section_box("CLV Classification Tree", "red", plotOutput("tree_plot", height = 640))))),
      tabItem(tabName = "tree_perf", fluidRow(column(3, div(class = "metric-slot red", uiOutput("tree_accuracy_card"))), column(3, div(class = "metric-slot blue", uiOutput("tree_train_card"))), column(3, div(class = "metric-slot green", uiOutput("tree_test_card"))), column(3, div(class = "metric-slot gold", uiOutput("tree_leaf_card")))), fluidRow(column(6, section_box("Confusion Matrix", "red", plotOutput("tree_confusion_plot", height = 360))), column(6, section_box("Variable Importance", "red", plotOutput("tree_importance_plot", height = 360)))), fluidRow(column(12, section_box("Class Recall Table", "red", DTOutput("tree_recall_tbl"))))),
      tabItem(tabName = "tree_rules", fluidRow(column(8, section_box("Decision Rules", "red", verbatimTextOutput("tree_rules_text"))), column(4, section_box("Terminal Nodes", "red", DTOutput("tree_leaf_tbl"))))),
      tabItem(tabName = "biz_findings", section_box("Executive Highlights", "gold", uiOutput("business_findings_ui"))),
      tabItem(tabName = "biz_actions", fluidRow(column(4, section_box("Action Focus Segment", "gold", selectInput("action_segment", "Choose Segment", choices = all_segment, selected = all_segment[1]), uiOutput("segment_actions_ui"))), column(8, section_box("Selected Segment Snapshot", "gold", DTOutput("action_segment_tbl"))))),
      tabItem(tabName = "biz_drivers", fluidRow(column(7, section_box("Top CLV Drivers", "gold", plotOutput("biz_driver_plot", height = 420))), column(5, section_box("Interpretation", "gold", uiOutput("biz_driver_text")))), fluidRow(column(12, section_box("Driver Table", "gold", DTOutput("biz_driver_tbl"))))),
      tabItem(tabName = "data_customer", section_box("Filtered Customer Table", "gold", DTOutput("customer_data_tbl"))),
      tabItem(tabName = "data_tx", section_box("Filtered Transaction Sample (up to 1,000 rows)", "gold", DTOutput("transaction_data_tbl"))),
      tabItem(tabName = "data_master", section_box("Enriched Master Dataset", "gold", DTOutput("master_data_tbl"))),
      tabItem(tabName = "data_filters", fluidRow(column(6, section_box("Current Filter State", "gold", uiOutput("filter_state_ui"))), column(6, section_box("Download Filtered Data", "gold", downloadButton("download_customers", "Download Filtered Customers"), tags$span(" "), downloadButton("download_master", "Download Filtered Master Dataset"))))),
      tabItem(tabName = "about_page", fluidRow(column(7, section_box("About This App", "blue", tags$p("This Shiny app keeps your original analytical structure intact while upgrading the visual language to better match the COFINFAD Colombian fintech dataset theme."), tags$ul(tags$li("Dashboard: KPI cards, customer snapshot, and segment snapshot."), tags$li("Data Analysis: profile analysis, transaction behaviour, and CLV/churn diagnostics."), tags$li("Clustering: K-means segmentation with PCA overview, profile heatmap, and cluster comparison."), tags$li("CLV Decision Tree: configurable rpart model for clv_segment classification."), tags$li("Business Insights: executive findings, segment-level action suggestions, and driver summaries."), tags$li("Shared Data Backbone: filtered customer data, transaction data, enriched master data, and downloads.")), tags$hr(), tags$p("Design direction: deep fintech navy, digital teal, and gold signals for premium finance storytelling."), tags$p("Goal: keep the app more presentation-ready for demos, project showcases, and executive walkthroughs without changing the underlying logic."))), column(5, section_box("Dataset Snapshot", "gold", tags$div(class = "summary-chip", "COFINFAD"), tags$div(class = "summary-chip", "Colombian fintech"), tags$div(class = "summary-chip", "Customer analytics"), tags$div(class = "summary-chip", "CLV / churn / behavior"), tags$hr(), tags$p(paste("Customer records:", fmt_num(nrow(customer_data)))), tags$p(paste("Transaction records:", fmt_num(nrow(transactions)))), tags$p(paste("Transaction date range:", format(min_tx_date), "to", format(max_tx_date))), tags$p("The app expects customer_data.csv and transactions_data.csv to be placed in the same folder as app.R, unless you modify DATA_DIR in the configuration block.")))))
    )
  )
)

# =========================================================
# Server
# =========================================================
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "cluster_compare_metric", choices = input$cluster_vars, selected = input$cluster_vars[1])
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "gender_filter", selected = "All")
    updateSelectizeInput(session, "income_filter", selected = all_income, server = TRUE)
    updateSelectizeInput(session, "segment_filter", selected = all_segment, server = TRUE)
    updateSelectizeInput(session, "clv_filter", selected = all_clv, server = TRUE)
    updateSelectizeInput(session, "location_filter", selected = all_location, server = TRUE)
    updateCheckboxGroupInput(session, "tx_type_filter", selected = all_tx_type)
    updateDateRangeInput(session, "date_filter", start = min_tx_date, end = max_tx_date)
  })
  
  filtered_customers <- reactive({
    df <- customer_data
    
    if (!identical(input$gender_filter, "All")) {
      df <- df %>% dplyr::filter(as.character(gender) == input$gender_filter)
    }
    
    df %>%
      dplyr::filter(
        as.character(income_bracket) %in% input$income_filter,
        as.character(customer_segment) %in% input$segment_filter,
        as.character(clv_segment) %in% input$clv_filter,
        as.character(location) %in% input$location_filter
      )
  })
  
  filtered_tx_month <- reactive({
    ids <- filtered_customers()$customer_id
    req(length(ids) > 0)
    
    start_month <- as.Date(format(input$date_filter[1], "%Y-%m-01"))
    end_month <- as.Date(format(input$date_filter[2], "%Y-%m-01"))
    
    tx_month_summary %>%
      dplyr::filter(
        customer_id %in% ids,
        month >= start_month,
        month <= end_month,
        as.character(type) %in% input$tx_type_filter
      )
  })
  
  filtered_master <- reactive({
    ids <- filtered_customers()$customer_id
    req(length(ids) > 0)
    master_dataset %>% dplyr::filter(customer_id %in% ids)
  })
  
  filtered_tx_preview <- reactive({
    ids <- filtered_customers()$customer_id
    req(length(ids) > 0)
    
    out <- transactions %>%
      dplyr::filter(
        customer_id %in% ids,
        date >= input$date_filter[1],
        date <= input$date_filter[2],
        as.character(type) %in% input$tx_type_filter
      )
    
    if (nrow(out) > 1000) out <- dplyr::slice_sample(out, n = 1000)
    dplyr::arrange(out, dplyr::desc(date))
  })
  
  driver_correlation_tbl <- reactive({
    df <- filtered_customers()
    req(nrow(df) > 10)
    req(input$corr_target %in% c("customer_lifetime_value", "churn_probability"))
    
    vals <- sapply(correlation_vars, function(v) {
      x <- df[[v]]
      y <- df[[input$corr_target]]
      if (all(is.na(x)) || all(is.na(y))) return(NA_real_)
      if (stats::sd(x, na.rm = TRUE) == 0 || stats::sd(y, na.rm = TRUE) == 0) return(NA_real_)
      suppressWarnings(stats::cor(x, y, use = "complete.obs"))
    })
    
    tibble::tibble(
      variable = correlation_vars,
      correlation = as.numeric(vals)
    ) %>%
      dplyr::filter(!is.na(correlation)) %>%
      dplyr::arrange(dplyr::desc(abs(correlation)))
  })
  
  cluster_result <- eventReactive(input$run_cluster, {
    run_clustering(
      df = filtered_master(),
      features = input$cluster_vars,
      k = input$k_clusters,
      sample_n = input$cluster_sample_n
    )
  }, ignoreInit = FALSE)
  
  tree_result <- eventReactive(input$run_tree, {
    selected_predictors <- predictor_sets[[input$predictor_group]]
    fit_tree_model(
      df = filtered_master(),
      predictors = selected_predictors,
      maxdepth = input$tree_maxdepth,
      minsplit = input$tree_minsplit,
      cp = input$tree_cp,
      train_ratio = input$tree_train_ratio,
      seed = input$tree_seed
    )
  }, ignoreInit = TRUE)
  
  business_driver_tbl <- reactive({
    tr <- tree_result()
    if (!is.null(tr) && nrow(tr$importance_tbl) > 0) {
      return(tr$importance_tbl %>% dplyr::mutate(source = "Decision Tree Importance"))
    }
    corr_tbl <- tryCatch(driver_correlation_tbl(), error = function(e) NULL)
    if (is.null(corr_tbl) || nrow(corr_tbl) == 0) {
      return(tibble::tibble(variable = character(), importance = numeric(), source = character()))
    }
    corr_tbl %>%
      dplyr::rename(importance = correlation) %>%
      dplyr::mutate(source = paste0("Correlation with ", input$corr_target))
  })
  
  # ----------------------------------
  # Filter summary & KPIs
  # ----------------------------------
  output$filter_summary_ui <- renderUI({
    df <- filtered_customers()
    tx_df <- filtered_tx_month()
    tags$div(
      tags$p(strong("Customers: "), fmt_num(nrow(df))),
      tags$p(strong("Share of base: "), fmt_pct(nrow(df) / nrow(customer_data))),
      tags$p(strong("Avg CLV: "), fmt_num(safe_mean(df$customer_lifetime_value))),
      tags$p(strong("Avg Churn Probability: "), fmt_pct(safe_mean(df$churn_probability))),
      tags$p(strong("Total Tx Amount (filtered): "), fmt_num(sum(tx_df$total_amount, na.rm = TRUE))),
      tags$p(strong("Active period: "), paste(format(input$date_filter[1]), "to", format(input$date_filter[2]))),
      tags$div(class = "summary-chip", paste("Segments:", paste(input$segment_filter, collapse = ", "))),
      tags$div(class = "summary-chip", paste("CLV:", paste(input$clv_filter, collapse = ", ")))
    )
  })
  
  output$filter_state_ui <- renderUI({
    tags$div(
      tags$p(strong("Gender: "), input$gender_filter),
      tags$p(strong("Income Brackets: "), paste(input$income_filter, collapse = ", ")),
      tags$p(strong("Customer Segments: "), paste(input$segment_filter, collapse = ", ")),
      tags$p(strong("CLV Segments: "), paste(input$clv_filter, collapse = ", ")),
      tags$p(strong("Locations: "), paste(input$location_filter, collapse = ", ")),
      tags$p(strong("Transaction Types: "), paste(input$tx_type_filter, collapse = ", ")),
      tags$p(strong("Date Range: "), paste(format(input$date_filter[1]), "to", format(input$date_filter[2])))
    )
  })
  
  output$kpi_customers <- renderUI({ df <- filtered_customers(); metric_card("Filtered Customers", fmt_num(nrow(df)), paste(fmt_pct(nrow(df) / nrow(customer_data)), "of full customer base")) })
  output$kpi_avg_clv <- renderUI({ df <- filtered_customers(); metric_card("Average CLV", fmt_num_short(safe_mean(df$customer_lifetime_value), 1), paste("Median:", fmt_num_short(stats::median(df$customer_lifetime_value, na.rm = TRUE), 1))) })
  output$kpi_avg_churn <- renderUI({ df <- filtered_customers(); metric_card("Average Churn Probability", fmt_pct(safe_mean(df$churn_probability)), paste("Avg satisfaction:", round(safe_mean(df$satisfaction_score), 1))) })
  output$kpi_total_tx <- renderUI({ tx_df <- filtered_tx_month(); metric_card("Transaction Amount", fmt_num_short(sum(tx_df$total_amount, na.rm = TRUE), 1), paste("Transaction count:", fmt_num(sum(tx_df$tx_count, na.rm = TRUE)))) })
  
  # ----------------------------------
  # Dashboard plots (Blue Theme mapping)
  # ----------------------------------
  output$dashboard_monthly_amount <- renderPlot({
    df <- filtered_tx_month() %>% dplyr::group_by(month) %>% dplyr::summarise(total_amount = sum(total_amount, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No transactions available under the current filters."))
    ggplot(df, aes(month, total_amount)) + geom_line(linewidth = 1, color = "#8fb1db") + geom_point(size = 2, color = "#101b35") + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Amount") + theme_minimal(base_size = 13)
  })
  
  output$dashboard_tx_mix <- renderPlot({
    df <- filtered_tx_month() %>% dplyr::group_by(type) %>% dplyr::summarise(tx_count = sum(tx_count, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No transactions available under the current filters."))
    ggplot(df, aes(x = reorder(as.character(type), tx_count), y = tx_count)) + geom_col(fill = "#8fb1db") + coord_flip() + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Transaction Count") + theme_minimal(base_size = 13)
  })
  
  output$profile_age_plot <- renderPlot({
    df <- filtered_customers()
    ggplot(df, aes(age)) + geom_histogram(bins = 25, fill = "#8fb1db", color = "white") + labs(x = "Age", y = "Customers") + theme_minimal(base_size = 13)
  })
  
  output$profile_income_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(income_bracket, sort = TRUE)
    ggplot(df, aes(x = reorder(as.character(income_bracket), n), y = n)) + geom_col(fill = "#8fb1db") + coord_flip() + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Customers") + theme_minimal(base_size = 13)
  })
  
  output$profile_gender_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(gender)
    ggplot(df, aes(x = as.character(gender), y = n)) + geom_col(fill = "#8fb1db") + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Customers") + theme_minimal(base_size = 13)
  })
  
  output$profile_occupation_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(occupation, sort = TRUE) %>% dplyr::slice_head(n = 10)
    ggplot(df, aes(x = reorder(as.character(occupation), n), y = n)) + geom_col(fill = "#8fb1db") + coord_flip() + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Customers") + theme_minimal(base_size = 13)
  })
  
  output$segment_size_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(customer_segment, sort = TRUE)
    ggplot(df, aes(x = reorder(as.character(customer_segment), n), y = n)) + geom_col(fill = "#8fb1db") + coord_flip() + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Customers") + theme_minimal(base_size = 13)
  })
  
  output$segment_clv_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::group_by(customer_segment) %>% dplyr::summarise(avg_clv = mean(customer_lifetime_value, na.rm = TRUE), .groups = "drop")
    ggplot(df, aes(x = reorder(as.character(customer_segment), avg_clv), y = avg_clv)) + geom_col(fill = "#8fb1db") + coord_flip() + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Average CLV") + theme_minimal(base_size = 13)
  })
  
  output$segment_churn_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::group_by(customer_segment) %>% dplyr::summarise(avg_churn = mean(churn_probability, na.rm = TRUE), .groups = "drop")
    ggplot(df, aes(x = reorder(as.character(customer_segment), avg_churn), y = avg_churn)) + geom_col(fill = "#8fb1db") + coord_flip() + scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = "Average Churn Probability") + theme_minimal(base_size = 13)
  })
  
  # ----------------------------------
  # Data Analysis plots (Blue Theme mapping)
  # ----------------------------------
  output$profile_heatmap <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(gender, income_bracket)
    ggplot(df, aes(x = as.character(gender), y = as.character(income_bracket), fill = n)) + geom_tile() + geom_text(aes(label = scales::comma(n)), size = 3.4, color = "white") + scale_fill_gradient(low = "white", high = "#8fb1db") + labs(x = NULL, y = NULL, fill = "Count") + theme_minimal(base_size = 13)
  })
  
  output$channel_clv_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::group_by(acquisition_channel) %>% dplyr::summarise(avg_clv = mean(customer_lifetime_value, na.rm = TRUE), .groups = "drop")
    ggplot(df, aes(x = reorder(as.character(acquisition_channel), avg_clv), y = avg_clv)) + geom_col(fill = "#8fb1db") + coord_flip() + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Average CLV") + theme_minimal(base_size = 13)
  })
  
  output$satisfaction_clv_plot <- renderPlot({
    df <- filtered_customers()
    ggplot(df, aes(x = clv_segment, y = satisfaction_score)) + geom_boxplot(fill = "#8fb1db", alpha = 0.8) + labs(x = "CLV Segment", y = "Satisfaction Score") + theme_minimal(base_size = 13)
  })
  
  output$tenure_segment_plot <- renderPlot({
    df <- filtered_customers()
    ggplot(df, aes(x = customer_segment, y = customer_tenure)) + geom_boxplot(fill = "#8fb1db", alpha = 0.8) + labs(x = "Customer Segment", y = "Customer Tenure") + theme_minimal(base_size = 13)
  })
  
  output$monthly_count_plot <- renderPlot({
    df <- filtered_tx_month() %>% dplyr::group_by(month) %>% dplyr::summarise(tx_count = sum(tx_count, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No transactions available under the current filters."))
    ggplot(df, aes(month, tx_count)) + geom_line(linewidth = 1, color = "#8fb1db") + geom_point(size = 2, color = "#101b35") + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Transaction Count") + theme_minimal(base_size = 13)
  })
  
  output$monthly_amount_type_plot <- renderPlot({
    df <- filtered_tx_month() %>% dplyr::group_by(month, type) %>% dplyr::summarise(total_amount = sum(total_amount, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No transactions available under the current filters."))
    ggplot(df, aes(month, total_amount, group = type)) + geom_line(linewidth = 1, color = "#8fb1db") + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "Amount", colour = "Type") + facet_wrap(~ type, scales = "free_y") + theme_minimal(base_size = 12)
  })
  
  output$weekend_ratio_plot <- renderPlot({
    df <- filtered_customers()
    ggplot(df, aes(x = customer_segment, y = weekend_transaction_ratio)) + geom_boxplot(fill = "#8fb1db", alpha = 0.8) + scale_y_continuous(labels = scales::percent) + labs(x = "Customer Segment", y = "Weekend Transaction Ratio") + theme_minimal(base_size = 13)
  })
  
  output$daily_tx_tenure_plot <- renderPlot({
    df <- filtered_customers()
    # 统一 EDA 散点图的采样上限和 Seed 控制
    if (nrow(df) > 8000) {
      set.seed(123)
      df <- dplyr::slice_sample(df, n = 8000)
    }
    ggplot(df, aes(customer_tenure, avg_daily_transactions)) + geom_point(alpha = 0.4, color = "#8fb1db") + geom_smooth(method = "lm", se = FALSE, color = "#bc6d51") + labs(x = "Customer Tenure", y = "Average Daily Transactions") + theme_minimal(base_size = 13)
  })
  
  output$clv_churn_scatter <- renderPlot({
    df <- filtered_customers()
    # 统一 EDA 散点图的采样上限和 Seed 控制
    if (nrow(df) > 8000) {
      set.seed(123)
      df <- dplyr::slice_sample(df, n = 8000)
    }
    ggplot(df, aes(churn_probability, customer_lifetime_value, colour = clv_segment)) + geom_point(alpha = 0.45) + scale_color_brewer(palette = "Set1") + scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::comma) + labs(x = "Churn Probability", y = "Customer Lifetime Value", colour = "CLV Segment") + theme_minimal(base_size = 13)
  })
  
  output$driver_corr_plot <- renderPlot({
    df <- driver_correlation_tbl() %>% dplyr::slice_head(n = 10)
    ggplot(df, aes(x = reorder(variable, abs(correlation)), y = correlation)) + geom_col(fill = "#8fb1db") + coord_flip() + labs(x = NULL, y = "Correlation") + theme_minimal(base_size = 13)
  })
  
  output$driver_corr_tbl <- renderDT({ DT::datatable(driver_correlation_tbl(), options = list(pageLength = 10, scrollX = TRUE)) })
  
  # ----------------------------------
  # Clustering outputs (Green Theme mapping)
  # ----------------------------------
  output$cluster_scatter_plot <- renderPlot({
    cl <- cluster_result()
    validate(need(!is.null(cl), "Unable to run clustering. Check selected variables and sample size."))
    ggplot(cl$data, aes(PC1, PC2, colour = cluster)) + geom_point(alpha = 0.7) + scale_color_brewer(palette = "Set2") + labs(x = "PC1", y = "PC2", colour = "Cluster") + theme_minimal(base_size = 13)
  })
  
  output$cluster_size_tbl <- renderDT({
    cl <- cluster_result()
    validate(need(!is.null(cl), "Unable to run clustering. Check selected variables and sample size."))
    tbl <- cl$data %>% dplyr::count(cluster, name = "customers")
    DT::datatable(tbl, options = list(dom = "t", pageLength = 10))
  })
  
  output$cluster_heatmap_plot <- renderPlot({
    cl <- cluster_result()
    validate(need(!is.null(cl), "Unable to run clustering. Check selected variables and sample size."))
    ggplot(cl$profile_long, aes(x = feature, y = cluster, fill = std_value)) + geom_tile() + geom_text(aes(label = round(mean_value, 2)), size = 3.2, colour = "white") + scale_fill_gradient2(low = "white", mid = "#e8edf4", high = "#6dac69") + labs(x = NULL, y = NULL, fill = "Std. Mean") + theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$cluster_profile_tbl <- renderDT({
    cl <- cluster_result()
    validate(need(!is.null(cl), "Unable to run clustering. Check selected variables and sample size."))
    DT::datatable(cl$profile, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  output$cluster_compare_plot <- renderPlot({
    cl <- cluster_result()
    validate(need(!is.null(cl), "Unable to run clustering. Check selected variables and sample size."))
    validate(need(input$cluster_compare_metric %in% names(cl$data), "Choose a valid metric for comparison."))
    ggplot(cl$data, aes(x = cluster, y = .data[[input$cluster_compare_metric]], fill = cluster)) + geom_boxplot(show.legend = FALSE) + scale_fill_brewer(palette = "Set2") + labs(x = "Cluster", y = input$cluster_compare_metric) + theme_minimal(base_size = 13)
  })
  
  output$cluster_mix_tbl <- renderDT({
    cl <- cluster_result()
    validate(need(!is.null(cl), "Unable to run clustering. Check selected variables and sample size."))
    mix_tbl <- cl$data %>% dplyr::count(cluster, customer_segment) %>% dplyr::group_by(cluster) %>% dplyr::mutate(share = n / sum(n)) %>% dplyr::ungroup()
    DT::datatable(mix_tbl, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ----------------------------------
  # Decision tree outputs (Red Theme mapping)
  # ----------------------------------
  output$tree_status_ui <- renderUI({
    tr <- tree_result()
    msg <- if (is.null(tr)) "Click Train Decision Tree to fit the current model on the filtered master dataset." else paste0("Model trained on ", fmt_num(tr$prepared_rows), " prepared rows using ", length(tr$predictors), " predictors. Accuracy: ", fmt_pct(tr$accuracy, 1), ".")
    div(class = "insight-box", msg)
  })
  
  output$tree_plot <- renderPlot({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    rpart.plot::rpart.plot(tr$model, type = 4, extra = 104, fallen.leaves = TRUE, tweak = 1.05, box.palette = "OrRd")
  })
  
  output$tree_accuracy_card <- renderUI({ tr <- tree_result(); validate(need(!is.null(tr), "Unable to train decision tree under the current filters.")); metric_card("Accuracy", fmt_pct(tr$accuracy), "Test-set accuracy") })
  output$tree_train_card <- renderUI({ tr <- tree_result(); validate(need(!is.null(tr), "Unable to train decision tree under the current filters.")); metric_card("Train Rows", fmt_num(tr$train_n), paste(length(tr$predictors), "predictors used")) })
  output$tree_test_card <- renderUI({ tr <- tree_result(); validate(need(!is.null(tr), "Unable to train decision tree under the current filters.")); metric_card("Test Rows", fmt_num(tr$test_n), paste("Predictor set:", input$predictor_group)) })
  output$tree_leaf_card <- renderUI({ tr <- tree_result(); validate(need(!is.null(tr), "Unable to train decision tree under the current filters.")); metric_card("Terminal Nodes", fmt_num(tr$terminal_nodes), "Interpretability remains explicit") })
  
  output$tree_confusion_plot <- renderPlot({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    ggplot(tr$conf_df, aes(x = Predicted, y = Actual, fill = Percent)) + geom_tile() + geom_text(aes(label = scales::comma(Freq)), colour = "white", size = 4) + scale_fill_gradient(low = "white", high = "#bc6d51", labels = scales::percent) + labs(x = "Predicted", y = "Actual", fill = "Row %") + theme_minimal(base_size = 13)
  })
  
  output$tree_importance_plot <- renderPlot({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    validate(need(nrow(tr$importance_tbl) > 0, "No variable importance available for the current tree."))
    imp <- tr$importance_tbl %>% dplyr::slice_head(n = 10)
    ggplot(imp, aes(x = reorder(variable, importance), y = importance)) + geom_col(fill = "#bc6d51") + coord_flip() + labs(x = NULL, y = "Importance") + theme_minimal(base_size = 13)
  })
  
  output$tree_recall_tbl <- renderDT({
    tr <- tree_result()
    if (is.null(tr)) return(DT::datatable(data.frame(Status = "Run the decision tree to populate recall metrics."), options = list(dom = "t"), rownames = FALSE))
    DT::datatable(tr$recall_df, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  }, server = FALSE)
  
  output$tree_rules_text <- renderText({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    paste(tr$rules_txt, collapse = "\n")
  })
  
  output$tree_leaf_tbl <- renderDT({
    tr <- tree_result()
    if (is.null(tr)) return(DT::datatable(data.frame(Status = "Run the decision tree to populate terminal nodes."), options = list(dom = "t"), rownames = FALSE))
    DT::datatable(tr$leaf_tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)
  
  # ----------------------------------
  # Business insights (Gold Theme mapping)
  # ----------------------------------
  output$business_findings_ui <- renderUI({
    df <- filtered_customers()
    if (nrow(df) == 0) return(div(class = "insight-box", "No customers remain under the current filters."))
    
    tx_df <- filtered_tx_month()
    tr <- tree_result()
    
    highest_value_segment <- df %>% dplyr::group_by(customer_segment) %>% dplyr::summarise(avg_clv = mean(customer_lifetime_value, na.rm = TRUE), .groups = "drop") %>% dplyr::slice_max(avg_clv, n = 1, with_ties = FALSE)
    highest_risk_segment <- df %>% dplyr::group_by(customer_segment) %>% dplyr::summarise(avg_churn = mean(churn_probability, na.rm = TRUE), .groups = "drop") %>% dplyr::slice_max(avg_churn, n = 1, with_ties = FALSE)
    tx_peak_month <- if (nrow(tx_df) > 0) tx_df %>% dplyr::group_by(month) %>% dplyr::summarise(total_amount = sum(total_amount, na.rm = TRUE), .groups = "drop") %>% dplyr::slice_max(total_amount, n = 1, with_ties = FALSE) else NULL
    top_driver <- if (!is.null(tr) && nrow(tr$importance_tbl) > 0) tr$importance_tbl$variable[1] else {
      drv <- business_driver_tbl()
      if (nrow(drv) > 0) drv$variable[1] else "No current driver available"
    }
    
    tagList(
      div(class = "insight-box", tags$strong("Portfolio value concentration: "), paste0(as.character(highest_value_segment$customer_segment[1]), " currently generates the highest average CLV within the active filter scope.")),
      div(class = "insight-box", tags$strong("Retention priority: "), paste0(as.character(highest_risk_segment$customer_segment[1]), " should be prioritised because it carries the highest average churn probability.")),
      div(class = "insight-box", tags$strong("Transaction seasonality: "), if (!is.null(tx_peak_month) && nrow(tx_peak_month) > 0) paste0("The strongest monthly transaction amount appears in ", format(tx_peak_month$month[1], "%Y-%m"), ".") else "No filtered transaction month is currently available."),
      div(class = "insight-box", tags$strong("Model-based CLV signal: "), paste0("Top driver under the current analytical setting: ", top_driver, ".")),
      div(class = "insight-box", tags$strong("Action implication: "), "Combine segment-level retention priorities with the strongest CLV driver to move from descriptive analytics into targeted intervention.")
    )
  })
  
  output$segment_actions_ui <- renderUI({
    recs <- segment_recommendations(filtered_customers(), input$action_segment)
    tagList(lapply(recs, function(x) div(class = "insight-box", x)))
  })
  
  output$action_segment_tbl <- renderDT({
    df <- filtered_customers()
    seg <- df %>% dplyr::filter(as.character(customer_segment) == input$action_segment)
    validate(need(nrow(seg) > 0, "No customers available in this segment under the current filters."))
    
    tbl <- tibble::tibble(
      metric = c("Customers", "Average CLV", "Average Churn Probability", "Average Satisfaction", "Average Active Products", "Average App Logins", "Average Tx Value"),
      value = c(fmt_num(nrow(seg)), fmt_num(mean(seg$customer_lifetime_value, na.rm = TRUE)), fmt_pct(mean(seg$churn_probability, na.rm = TRUE)), round(mean(seg$satisfaction_score, na.rm = TRUE), 2), round(mean(seg$active_products, na.rm = TRUE), 2), round(mean(seg$app_logins_frequency, na.rm = TRUE), 2), fmt_num(mean(seg$avg_tx_value, na.rm = TRUE)))
    )
    DT::datatable(tbl, options = list(dom = "t", pageLength = 10))
  })
  
  output$biz_driver_plot <- renderPlot({
    df <- business_driver_tbl() %>% dplyr::slice_head(n = 10)
    validate(need(nrow(df) > 0, "No CLV driver summary is available yet."))
    ggplot(df, aes(x = reorder(variable, abs(importance)), y = importance)) + geom_col(fill = "#d3ad57") + coord_flip() + labs(x = NULL, y = ifelse(any(df$source == "Decision Tree Importance"), "Importance", "Correlation")) + theme_minimal(base_size = 13)
  })
  
  output$biz_driver_text <- renderUI({
    df <- business_driver_tbl()
    if (nrow(df) == 0) return(div(class = "insight-box", "Run the decision tree or use the CLV/Churn correlation panel to surface drivers."))
    
    top_name <- df$variable[1]
    source_name <- df$source[1]
    
    tagList(
      div(class = "insight-box", paste0("The most prominent driver currently surfaced is ", top_name, ".")),
      div(class = "insight-box", paste0("Source of evidence: ", source_name, ".")),
      div(class = "insight-box", "Use this section to connect technical findings back to strategy: which levers should the business intervene on, which cohorts should be prioritised, and what operational action is feasible from the dashboard evidence?")
    )
  })
  
  output$biz_driver_tbl <- renderDT({
    df <- business_driver_tbl()
    if (nrow(df) == 0) return(DT::datatable(data.frame(Status = "No CLV driver summary is available yet."), options = list(dom = "t"), rownames = FALSE))
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)
  
  # ----------------------------------
  # Data tables & Downloads
  # ----------------------------------
  output$customer_data_tbl <- renderDT({
    df <- filtered_customers() %>% dplyr::select(customer_id, age, gender, location, income_bracket, occupation, customer_segment, clv_segment, active_products, satisfaction_score, churn_probability, customer_lifetime_value) %>% dplyr::slice_head(n = 1000)
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$transaction_data_tbl <- renderDT({
    df <- filtered_tx_preview()
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$master_data_tbl <- renderDT({
    df <- filtered_master() %>% dplyr::select(dplyr::any_of(c("customer_id", "customer_segment", "clv_segment", "customer_lifetime_value", "churn_probability", "tx_n", "tx_total_amount", "tx_avg_amount", "tx_sd_amount", "active_days", "first_tx_date_tx", "last_tx_date_tx", "share_Deposit", "share_Payment", "share_Transfer", "share_Withdrawal", "monthly_n_sd", "monthly_amount_sd", "monthly_n_mean", "monthly_amount_mean"))) %>% dplyr::slice_head(n = 1000)
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)
  
  output$download_customers <- downloadHandler(filename = function() { paste0("filtered_customers_", Sys.Date(), ".csv") }, content = function(file) { readr::write_csv(filtered_customers(), file) })
  output$download_master <- downloadHandler(filename = function() { paste0("filtered_master_dataset_", Sys.Date(), ".csv") }, content = function(file) { readr::write_csv(filtered_master(), file) })
}

shinyApp(ui, server)