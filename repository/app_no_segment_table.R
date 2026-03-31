required_packages <- c(
  "shiny", "shinydashboard", "bslib", "dplyr", "tidyr", "ggplot2", "DT", "lubridate",
  "scales", "readr", "forcats", "rpart", "rpart.plot", "stringr", "tibble"
)

missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Please install these packages before running the app: ",
    paste(missing_pkgs, collapse = ", ")
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

# =========================================================
# Configuration
# Put app.R, customer_data.csv, and transactions_data.csv
# in the same folder, or change DATA_DIR below.
# =========================================================
DATA_DIR <- "."
CUSTOMER_FILE <- file.path(DATA_DIR, "customer_data.csv")
TRANSACTION_FILE <- file.path(DATA_DIR, "transactions_data.csv")

if (!file.exists(CUSTOMER_FILE)) stop("Cannot find: ", CUSTOMER_FILE)
if (!file.exists(TRANSACTION_FILE)) stop("Cannot find: ", TRANSACTION_FILE)

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
    tidyr::drop_na()

  if (nrow(tmp) < k + 5) return(NULL)
  if (nrow(tmp) > sample_n) tmp <- dplyr::slice_sample(tmp, n = sample_n)

  x <- tmp %>% dplyr::select(dplyr::all_of(vars))
  x[] <- lapply(x, as.numeric)

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
    if (inherits(x, "Date")) x <- as.numeric(x)
    if (is.logical(x)) {
      x <- factor(ifelse(is.na(x), "Unknown", ifelse(x, "Yes", "No")))
    } else if (is.character(x) || is.factor(x)) {
      x <- compress_levels(x, max_levels = 10)
    } else if (is.numeric(x) || is.integer(x)) {
      med <- suppressWarnings(stats::median(x, na.rm = TRUE))
      if (!is.finite(med)) med <- 0
      x[is.na(x)] <- med
    } else {
      next
    }
    if (length(unique(stats::na.omit(x))) <= 1) next
    tmp[[v]] <- x
    kept <- c(kept, v)
  }

  if (length(kept) == 0) return(NULL)
  tmp <- tmp %>% dplyr::select(clv_segment, dplyr::all_of(kept))
  if (length(unique(tmp$clv_segment)) < 2) return(NULL)

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
# Load and prepare data
# =========================================================
customer_raw <- readr::read_csv(CUSTOMER_FILE, show_col_types = FALSE)
transactions_raw <- readr::read_csv(TRANSACTION_FILE, show_col_types = FALSE)


customer_data <- prepare_customer_data(customer_raw)
transactions <- prepare_transactions(transactions_raw)

tx_customer_summary <- transactions %>%
  dplyr::group_by(customer_id) %>%
  dplyr::summarise(
    tx_n = dplyr::n(),
    tx_total_amount = sum(amount, na.rm = TRUE),
    tx_avg_amount = mean(amount, na.rm = TRUE),
    tx_sd_amount = stats::sd(amount, na.rm = TRUE),
    active_days = dplyr::n_distinct(date),
    first_tx_date_tx = min(date, na.rm = TRUE),
    last_tx_date_tx = max(date, na.rm = TRUE),
    .groups = "drop"
  )

tx_type_summary <- transactions %>%
  dplyr::count(customer_id, type) %>%
  dplyr::group_by(customer_id) %>%
  dplyr::mutate(type_share = n / sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::select(customer_id, type, type_share) %>%
  tidyr::pivot_wider(names_from = type, values_from = type_share, values_fill = 0, names_prefix = "share_")

tx_volatility <- transactions %>%
  dplyr::mutate(month = as.Date(format(date, "%Y-%m-01"))) %>%
  dplyr::group_by(customer_id, month) %>%
  dplyr::summarise(monthly_n = dplyr::n(), monthly_amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(customer_id) %>%
  dplyr::summarise(
    monthly_n_sd = stats::sd(monthly_n, na.rm = TRUE),
    monthly_amount_sd = stats::sd(monthly_amount, na.rm = TRUE),
    monthly_n_mean = mean(monthly_n, na.rm = TRUE),
    monthly_amount_mean = mean(monthly_amount, na.rm = TRUE),
    .groups = "drop"
  )

tx_month_summary <- transactions %>%
  dplyr::mutate(month = as.Date(format(date, "%Y-%m-01"))) %>%
  dplyr::group_by(customer_id, month, type) %>%
  dplyr::summarise(tx_count = dplyr::n(), total_amount = sum(amount, na.rm = TRUE), .groups = "drop")

master_dataset <- customer_data %>%
  dplyr::left_join(tx_customer_summary, by = "customer_id") %>%
  dplyr::left_join(tx_type_summary, by = "customer_id") %>%
  dplyr::left_join(tx_volatility, by = "customer_id")

predictor_sets <- get_predictor_sets(master_dataset)

cluster_feature_choices <- intersect(c(
  "age", "active_products", "app_logins_frequency", "feature_usage_diversity",
  "credit_utilization_ratio", "international_transactions", "failed_transactions",
  "satisfaction_score", "support_tickets_count", "weekend_transaction_ratio",
  "customer_tenure", "tx_n", "tx_total_amount", "tx_avg_amount",
  "tx_sd_amount", "active_days", "share_Deposit", "share_Payment",
  "share_Transfer", "share_Withdrawal", "monthly_n_sd", "monthly_amount_sd",
  "monthly_n_mean", "monthly_amount_mean"
), names(master_dataset))

default_cluster_vars <- intersect(c(
  "active_products", "app_logins_frequency", "feature_usage_diversity",
  "satisfaction_score", "customer_tenure", "tx_n", "tx_total_amount",
  "share_Payment", "share_Transfer"
), cluster_feature_choices)
if (length(default_cluster_vars) < 2) default_cluster_vars <- head(cluster_feature_choices, min(4, length(cluster_feature_choices)))

correlation_vars <- intersect(c(
  "age", "household_size", "active_products", "app_logins_frequency",
  "feature_usage_diversity", "credit_utilization_ratio", "international_transactions",
  "failed_transactions", "tx_count", "avg_tx_value", "base_satisfaction",
  "tx_satisfaction", "product_satisfaction", "satisfaction_score", "nps_score",
  "support_tickets_count", "resolved_tickets_ratio", "app_store_rating",
  "monthly_transaction_count", "average_transaction_value", "transaction_frequency",
  "weekend_transaction_ratio", "avg_daily_transactions", "customer_tenure",
  "tx_n", "tx_total_amount", "tx_avg_amount", "active_days"
), names(master_dataset))

all_income <- sort(unique(as.character(customer_data$income_bracket)))
all_segment <- sort(unique(as.character(customer_data$customer_segment)))
all_clv <- levels(customer_data$clv_segment)
all_location <- sort(unique(as.character(customer_data$location)))
all_tx_type <- sort(unique(as.character(transactions$type)))
min_tx_date <- min(transactions$date, na.rm = TRUE)
max_tx_date <- max(transactions$date, na.rm = TRUE)


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
        selectInput(
          "gender_filter",
          "Gender",
          choices = c("All", sort(unique(as.character(customer_data$gender)))),
          selected = "All"
        ),
        selectizeInput(
          "income_filter",
          "Income Bracket",
          choices = all_income,
          selected = all_income,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        selectizeInput(
          "segment_filter",
          "Customer Segment",
          choices = all_segment,
          selected = all_segment,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        selectizeInput(
          "clv_filter",
          "CLV Segment",
          choices = all_clv,
          selected = all_clv,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        selectizeInput(
          "location_filter",
          "Location",
          choices = all_location,
          selected = all_location,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        checkboxGroupInput(
          "tx_type_filter",
          "Transaction Types",
          choices = all_tx_type,
          selected = all_tx_type,
          inline = FALSE
        ),
        dateRangeInput(
          "date_filter",
          "Transaction Date Range",
          start = min_tx_date,
          end = max_tx_date,
          min = min_tx_date,
          max = max_tx_date
        ),
        actionButton("reset_filters", "Reset Filters", class = "btn-reset-filters")
      )
    )
  )
}

cluster_controls_panel <- function() {
  section_box(
    "Clustering Controls", "green",
    selectizeInput(
      "cluster_vars",
      "Variables for K-means",
      choices = cluster_feature_choices,
      selected = default_cluster_vars,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    selectInput("cluster_compare_metric", "Metric for Cluster Comparison", choices = default_cluster_vars),
    sliderInput("k_clusters", "Number of Clusters", min = 2, max = 6, value = 4, step = 1),
    sliderInput("cluster_sample_n", "Max Rows Used for Clustering", min = 2000, max = 15000, value = 8000, step = 1000),
    actionButton("run_cluster", "Run Clustering", class = "btn-green"),
    tags$p(
      class = "small-note",
      "Customer-level attributes are enriched with transaction summaries, type shares, and monthly volatility features before clustering."
    )
  )
}

# =========================================================
# UI
# =========================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(class = "brand-title", "Fintech Customer"),
    titleWidth = 330
  ),
  dashboardSidebar(
    width = 330,
    sidebarMenu(
      id = "nav_tabs",
      menuItem(
        "Dashboard", icon = icon("dashboard"), startExpanded = TRUE,
        menuSubItem("KPI Cards", tabName = "dash_kpi", selected = TRUE),
        menuSubItem("Customer Snapshot", tabName = "dash_customer"),
        menuSubItem("Segment Snapshot", tabName = "dash_segment")
      ),
      menuItem(
        "Data Analysis", icon = icon("bar-chart-o"), startExpanded = FALSE,
        menuSubItem("Customer Profile", tabName = "analysis_profile"),
        menuSubItem("Transaction Behavior", tabName = "analysis_tx"),
        menuSubItem("CLV / Churn Analysis", tabName = "analysis_clv")
      ),
      menuItem(
        "Clustering", icon = icon("sitemap"), startExpanded = FALSE,
        menuSubItem("Cluster Overview", tabName = "cluster_overview"),
        menuSubItem("Segment Profiles", tabName = "cluster_profiles"),
        menuSubItem("Cluster Comparison", tabName = "cluster_compare")
      ),
      menuItem(
        "CLV Decision Tree", icon = icon("random"), startExpanded = FALSE,
        menuSubItem("Model Setup", tabName = "tree_setup"),
        menuSubItem("Tree Plot", tabName = "tree_plot_tab"),
        menuSubItem("Performance", tabName = "tree_perf"),
        menuSubItem("Rules / Terminal Nodes", tabName = "tree_rules")
      ),
      menuItem(
        "Business Insights", icon = icon("lightbulb-o"), startExpanded = FALSE,
        menuSubItem("Key Findings", tabName = "biz_findings"),
        menuSubItem("Segment Actions", tabName = "biz_actions"),
        menuSubItem("CLV Driver Summary", tabName = "biz_drivers")
      ),
      menuItem(
        "Shared Data Backbone", icon = icon("database"), startExpanded = FALSE,
        menuSubItem("Customer Data", tabName = "data_customer"),
        menuSubItem("Transaction Data", tabName = "data_tx"),
        menuSubItem("Master Dataset", tabName = "data_master"),
        menuSubItem("Reactive Filters", tabName = "data_filters")
      ),
      menuItem("About", tabName = "about_page", icon = icon("info-circle"))
    ),
    sidebar_filters_ui()
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("\n        body, .content-wrapper, .right-side { background: #dfe5eb; font-family: 'Inter', 'Segoe UI', Arial, sans-serif; }\n        .main-header .logo, .main-header .navbar { background: #ffffff !important; color: #1f2937 !important; box-shadow: 0 1px 8px rgba(15,23,42,.08); }\n        .main-header .logo { font-weight: 800; font-size: 20px; }\n        .main-header .navbar .sidebar-toggle, .main-header .navbar .sidebar-toggle:hover { color: #1f2937 !important; background: transparent !important; }\n        .main-sidebar, .left-side { background: linear-gradient(180deg, #0a1229 0%, #101b35 100%) !important; }\n        .sidebar { padding-top: 10px; }\n        .sidebar-menu > li > a { color: rgba(255,255,255,.94) !important; font-weight: 700; font-size: 15px; padding: 14px 16px; }\n        .sidebar-menu .treeview-menu > li > a { color: rgba(255,255,255,.82) !important; font-size: 14px; padding: 11px 16px 11px 32px; }\n        .sidebar-menu > li.active > a, .sidebar-menu > li:hover > a, .sidebar-menu .treeview-menu > li.active > a, .sidebar-menu .treeview-menu > li > a:hover { background: rgba(255,255,255,.10) !important; color: #ffffff !important; border-left: 4px solid #8fb1db; }\n        .sidebar-menu .treeview-menu { background: rgba(255,255,255,.03) !important; }\n        .sidebar-menu .treeview-menu > li > a { border-left: 4px solid transparent; }\n        .sidebar-menu > li.header { color: rgba(255,255,255,.65) !important; background: transparent !important; }\n        .content-wrapper, .right-side { min-height: calc(100vh - 50px); }\n        .content { padding: 24px 24px 30px; }\n\n        .filter-shell { padding: 18px 12px 18px; }\n        .filter-details { border: 1px solid rgba(255,255,255,.10); border-radius: 18px; background: rgba(255,255,255,.06); overflow: hidden; box-shadow: 0 10px 24px rgba(0,0,0,.16); }\n        .filter-details summary { list-style: none; cursor: pointer; color: #ffffff; font-weight: 800; padding: 18px 20px; font-size: 16px; display: flex; align-items: center; gap: 10px; }\n        .filter-details summary::-webkit-details-marker { display: none; }\n        .filter-inner { padding: 2px 18px 18px; color: #ffffff; }\n        .filter-inner .form-group > label, .filter-inner .control-label, .filter-inner .checkbox label { color: #ffffff; font-weight: 700; }\n        .filter-inner .form-control, .filter-inner .selectize-input, .filter-inner .selectize-control.multi .selectize-input, .filter-inner .date { border-radius: 14px !important; border: none !important; min-height: 44px; }\n        .filter-inner .selectize-dropdown, .filter-inner .selectize-input.full { color: #1f2937; }\n        .filter-inner .checkbox { margin-top: 0; }\n        .btn-reset-filters { width: 100%; border-radius: 12px; background: #d3ad57; border: none; color: #1f2937; font-weight: 700; padding: 10px 12px; }\n        .btn-green { background: #6dac69; color: #fff; border: none; border-radius: 12px; font-weight: 700; }\n\n        .section-box { background: #ffffff; border-radius: 18px; overflow: hidden; margin-bottom: 24px; box-shadow: 0 10px 24px rgba(15,23,42,.08); border: 1px solid transparent; }\n        .section-box-header { padding: 16px 20px; color: #ffffff; font-size: 17px; font-weight: 800; letter-spacing: .1px; }\n        .section-box-body { padding: 18px 18px 14px; background: #ffffff; }\n        .theme-blue { border-color: #8fb1db; }\n        .theme-blue .section-box-header { background: #8fb1db; }\n        .theme-green { border-color: #6dac69; }\n        .theme-green .section-box-header { background: #6dac69; }\n        .theme-red { border-color: #bc6d51; }\n        .theme-red .section-box-header { background: #bc6d51; }\n        .theme-gold { border-color: #d3ad57; }\n        .theme-gold .section-box-header { background: #d3ad57; }\n\n        .metric-card { color: #ffffff; border-radius: 18px; padding: 22px 24px; min-height: 148px; box-shadow: 0 10px 24px rgba(15,23,42,.10); margin-bottom: 24px; overflow: hidden; }\n        .metric-title { font-size: 15px; font-weight: 700; opacity: .96; margin-bottom: 10px; }\n        .metric-value { font-size: clamp(30px, 2.4vw, 52px); font-weight: 900; line-height: 1; letter-spacing: -.8px; margin-bottom: 10px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }\n        .metric-subtitle { font-size: 13px; opacity: .92; }
    .shiny-table table { width: 100%; background: #ffffff; border-collapse: collapse; }
    .shiny-table th { background: #f5f7fb; color: #1f2937; font-weight: 700; padding: 10px 12px; border-bottom: 1px solid #d9e2ef; }
    .shiny-table td { padding: 10px 12px; border-bottom: 1px solid #eef2f7; }\n        .metric-slot.blue .metric-card { background: #8fb1db; }\n        .metric-slot.green .metric-card { background: #6dac69; }\n        .metric-slot.gold .metric-card { background: #d3ad57; }\n        .metric-slot.red .metric-card { background: #bc6d51; }\n\n        .insight-box { background: #ffffff; border-left: 4px solid #8fb1db; border-radius: 14px; padding: 16px 18px; margin-bottom: 14px; box-shadow: 0 4px 12px rgba(15,23,42,.05); }\n        .small-note { color: #64748b; font-size: 12px; line-height: 1.55; margin-top: 8px; }\n        .summary-chip { display: inline-block; background: #e8edf4; color: #1f2937; border-radius: 999px; padding: 7px 10px; margin: 0 6px 6px 0; font-size: 12px; font-weight: 600; }\n\n        .plot-placeholder, .text-placeholder { min-height: 280px; display: flex; align-items: center; justify-content: center; color: #8a94a6; font-size: 16px; }\n        .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: #e8edf4 !important; border-color: #cdd7e5 !important; }\n        table.dataTable thead th { font-weight: 800; }\n        .content .row { margin-left: -12px; margin-right: -12px; }\n        .content .col-sm-1, .content .col-sm-2, .content .col-sm-3, .content .col-sm-4, .content .col-sm-5, .content .col-sm-6, .content .col-sm-7, .content .col-sm-8, .content .col-sm-9, .content .col-sm-10, .content .col-sm-11, .content .col-sm-12 { padding-left: 12px; padding-right: 12px; }\n      "))
    ),
    tabItems(
      tabItem(
        tabName = "dash_kpi",
        fluidRow(
          column(3, div(class = "metric-slot blue", uiOutput("kpi_customers"))),
          column(3, div(class = "metric-slot green", uiOutput("kpi_avg_clv"))),
          column(3, div(class = "metric-slot gold", uiOutput("kpi_avg_churn"))),
          column(3, div(class = "metric-slot red", uiOutput("kpi_total_tx")))
        ),
        fluidRow(
          column(8, section_box("Monthly Transaction Amount", "blue", plotOutput("dashboard_monthly_amount", height = 360))),
          column(4, section_box("Transaction Type Mix", "blue", plotOutput("dashboard_tx_mix", height = 360)))
        ),
        fluidRow(
          column(12, section_box("Current Filter Snapshot", "gold", uiOutput("filter_summary_ui")))
        )
      ),
      tabItem(
        tabName = "dash_customer",
        fluidRow(
          column(6, section_box("Age Distribution", "blue", plotOutput("profile_age_plot", height = 320))),
          column(6, section_box("Income Bracket Composition", "blue", plotOutput("profile_income_plot", height = 320)))
        ),
        fluidRow(
          column(6, section_box("Gender Composition", "blue", plotOutput("profile_gender_plot", height = 320))),
          column(6, section_box("Top Occupations", "blue", plotOutput("profile_occupation_plot", height = 320)))
        )
      ),
      tabItem(
        tabName = "dash_segment",
        fluidRow(
          column(4, section_box("Segment Size", "blue", plotOutput("segment_size_plot", height = 320))),
          column(4, section_box("Average CLV by Segment", "blue", plotOutput("segment_clv_plot", height = 320))),
          column(4, section_box("Average Churn by Segment", "blue", plotOutput("segment_churn_plot", height = 320)))
        ),
        fluidRow(

        )
      ),
      tabItem(
        tabName = "analysis_profile",
        fluidRow(
          column(6, section_box("Gender x Income Heatmap", "blue", plotOutput("profile_heatmap", height = 340))),
          column(6, section_box("Average CLV by Acquisition Channel", "blue", plotOutput("channel_clv_plot", height = 340)))
        ),
        fluidRow(
          column(6, section_box("Satisfaction by CLV Segment", "blue", plotOutput("satisfaction_clv_plot", height = 330))),
          column(6, section_box("Tenure by Customer Segment", "blue", plotOutput("tenure_segment_plot", height = 330)))
        )
      ),
      tabItem(
        tabName = "analysis_tx",
        fluidRow(
          column(6, section_box("Monthly Transaction Count", "blue", plotOutput("monthly_count_plot", height = 330))),
          column(6, section_box("Monthly Transaction Amount by Type", "blue", plotOutput("monthly_amount_type_plot", height = 330)))
        ),
        fluidRow(
          column(6, section_box("Weekend Transaction Ratio by Segment", "blue", plotOutput("weekend_ratio_plot", height = 330))),
          column(6, section_box("Avg Daily Transactions vs Tenure", "blue", plotOutput("daily_tx_tenure_plot", height = 330)))
        )
      ),
      tabItem(
        tabName = "analysis_clv",
        fluidRow(
          column(8, section_box("CLV vs Churn Probability", "blue", plotOutput("clv_churn_scatter", height = 390))),
          column(4, section_box(
            "Driver Correlations", "blue",
            selectInput("corr_target", "Correlation Target", choices = c(
              "Customer Lifetime Value" = "customer_lifetime_value",
              "Churn Probability" = "churn_probability"
            )),
            plotOutput("driver_corr_plot", height = 330)
          ))
        ),
        fluidRow(
          column(12, section_box("Top Correlation Table", "blue", DTOutput("driver_corr_tbl")))
        )
      ),
      tabItem(
        tabName = "cluster_overview",
        fluidRow(
          column(4, cluster_controls_panel()),
          column(8,
                 section_box("Cluster Overview", "green", plotOutput("cluster_scatter_plot", height = 420)),
                 section_box("Cluster Size", "green", DTOutput("cluster_size_tbl"))
          )
        )
      ),
      tabItem(
        tabName = "cluster_profiles",
        fluidRow(
          column(4, section_box("Current Clustering Note", "green", tags$p("Use the Cluster Overview page to update variables, cluster count, and sampling settings before reviewing profile patterns."))),
          column(8,
                 section_box("Cluster Heatmap (Standardised Means)", "green", plotOutput("cluster_heatmap_plot", height = 430)),
                 section_box("Cluster Profile Table", "green", DTOutput("cluster_profile_tbl"))
          )
        )
      ),
      tabItem(
        tabName = "cluster_compare",
        fluidRow(
          column(4, section_box("Comparison Metric", "green", tags$p("The comparison chart below uses the metric currently selected in the clustering controls."))),
          column(8,
                 section_box("Selected Metric by Cluster", "green", plotOutput("cluster_compare_plot", height = 420)),
                 section_box("Segment Mix inside Each Cluster", "green", DTOutput("cluster_mix_tbl"))
          )
        )
      ),
      tabItem(
        tabName = "tree_setup",
        fluidRow(
          column(4,
                 section_box(
                   "Model Controls", "red",
                   selectInput("predictor_group", "Predictor Group", choices = names(predictor_sets), selected = "Full Model"),
                   sliderInput("tree_maxdepth", "Maximum Tree Depth", min = 2, max = 8, value = 4, step = 1),
                   sliderInput("tree_minsplit", "Minimum Split Size", min = 10, max = 100, value = 30, step = 5),
                   sliderInput("tree_cp", "Complexity Parameter (cp)", min = 0.000, max = 0.050, value = 0.010, step = 0.001),
                   sliderInput("tree_train_ratio", "Train/Test Split", min = 0.50, max = 0.90, value = 0.75, step = 0.05),
                   numericInput("tree_seed", "Random Seed", value = 123, min = 1),
                   actionButton("run_tree", "Train Decision Tree", class = "btn-primary"),
                   tags$p(class = "small-note", "Target variable is clv_segment. customer_lifetime_value is excluded from the predictor set to avoid leakage.")
                 )
          ),
          column(8,
                 section_box("Model Setup Notes", "red", uiOutput("tree_status_ui"))
          )
        )
      ),
      tabItem(
        tabName = "tree_plot_tab",
        fluidRow(
          column(3, section_box("Workflow Note", "red", tags$p("Train the model in Model Setup first. The fitted tree below uses the current filtered cohort and selected predictor group."))),
          column(9, section_box("CLV Classification Tree", "red", plotOutput("tree_plot", height = 640)))
        )
      ),
      tabItem(
        tabName = "tree_perf",
        fluidRow(
          column(3, div(class = "metric-slot red", uiOutput("tree_accuracy_card"))),
          column(3, div(class = "metric-slot blue", uiOutput("tree_train_card"))),
          column(3, div(class = "metric-slot green", uiOutput("tree_test_card"))),
          column(3, div(class = "metric-slot gold", uiOutput("tree_leaf_card")))
        ),
        fluidRow(
          column(6, section_box("Confusion Matrix", "red", plotOutput("tree_confusion_plot", height = 360))),
          column(6, section_box("Variable Importance", "red", plotOutput("tree_importance_plot", height = 360)))
        ),
        fluidRow(
          column(12, section_box("Class Recall Table", "red", DTOutput("tree_recall_tbl")))
        )
      ),
      tabItem(
        tabName = "tree_rules",
        fluidRow(
          column(8, section_box("Decision Rules", "red", verbatimTextOutput("tree_rules_text"))),
          column(4, section_box("Terminal Nodes", "red", DTOutput("tree_leaf_tbl")))
        )
      ),
      tabItem(
        tabName = "biz_findings",
        section_box("Executive Highlights", "gold", uiOutput("business_findings_ui"))
      ),
      tabItem(
        tabName = "biz_actions",
        fluidRow(
          column(4,
                 section_box(
                   "Action Focus Segment", "gold",
                   selectInput("action_segment", "Choose Segment", choices = all_segment, selected = all_segment[1]),
                   uiOutput("segment_actions_ui")
                 )
          ),
          column(8, section_box("Selected Segment Snapshot", "gold", DTOutput("action_segment_tbl")))
        )
      ),
      tabItem(
        tabName = "biz_drivers",
        fluidRow(
          column(7, section_box("Top CLV Drivers", "gold", plotOutput("biz_driver_plot", height = 420))),
          column(5, section_box("Interpretation", "gold", uiOutput("biz_driver_text")))
        ),
        fluidRow(
          column(12, section_box("Driver Table", "gold", DTOutput("biz_driver_tbl")))
        )
      ),
      tabItem(
        tabName = "data_customer",
        section_box("Filtered Customer Table", "gold", DTOutput("customer_data_tbl"))
      ),
      tabItem(
        tabName = "data_tx",
        section_box("Filtered Transaction Sample (up to 1,000 rows)", "gold", DTOutput("transaction_data_tbl"))
      ),
      tabItem(
        tabName = "data_master",
        section_box("Enriched Master Dataset", "gold", DTOutput("master_data_tbl"))
      ),
      tabItem(
        tabName = "data_filters",
        fluidRow(
          column(6, section_box("Current Filter State", "gold", uiOutput("filter_state_ui"))),
          column(6, section_box(
            "Download Filtered Data", "gold",
            downloadButton("download_customers", "Download Filtered Customers"),
            tags$span(" "),
            downloadButton("download_master", "Download Filtered Master Dataset")
          ))
        )
      ),
      tabItem(
        tabName = "about_page",
        section_box(
          "About This App", "blue",
          tags$p("This Shiny app follows the architecture we discussed earlier and organises the analysis into seven major sections: Dashboard, Data Analysis, Clustering, CLV Decision Tree, Business Insights, About, and Shared Data Backbone."),
          tags$ul(
            tags$li("Dashboard: KPI cards, customer snapshot, and segment snapshot."),
            tags$li("Data Analysis: profile analysis, transaction behaviour, and CLV/churn diagnostics."),
            tags$li("Clustering: K-means segmentation with PCA overview, profile heatmap, and cluster comparison."),
            tags$li("CLV Decision Tree: configurable rpart model for clv_segment classification."),
            tags$li("Business Insights: executive findings, segment-level action suggestions, and driver summaries."),
            tags$li("Shared Data Backbone: filtered customer data, transaction data, enriched master data, and downloads.")
          ),
          tags$hr(),
          tags$p(paste("Customer records:", fmt_num(nrow(customer_data)))),
          tags$p(paste("Transaction records:", fmt_num(nrow(transactions)))),
          tags$p(paste("Transaction date range:", format(min_tx_date), "to", format(max_tx_date))),
          tags$p("The app expects customer_data.csv and transactions_data.csv to be placed in the same folder as app.R, unless you modify DATA_DIR in the configuration block.")
        )
      )
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
  # Filter summary
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

  # ----------------------------------
  # KPI cards
  # ----------------------------------
  output$kpi_customers <- renderUI({
    df <- filtered_customers()
    metric_card(
      "Filtered Customers",
      fmt_num(nrow(df)),
      paste(fmt_pct(nrow(df) / nrow(customer_data)), "of full customer base")
    )
  })

  output$kpi_avg_clv <- renderUI({
    df <- filtered_customers()
    metric_card(
      "Average CLV",
      fmt_num_short(safe_mean(df$customer_lifetime_value), 1),
      paste("Median:", fmt_num_short(stats::median(df$customer_lifetime_value, na.rm = TRUE), 1))
    )
  })

  output$kpi_avg_churn <- renderUI({
    df <- filtered_customers()
    metric_card(
      "Average Churn Probability",
      fmt_pct(safe_mean(df$churn_probability)),
      paste("Avg satisfaction:", round(safe_mean(df$satisfaction_score), 1))
    )
  })

  output$kpi_total_tx <- renderUI({
    tx_df <- filtered_tx_month()
    metric_card(
      "Transaction Amount",
      fmt_num_short(sum(tx_df$total_amount, na.rm = TRUE), 1),
      paste("Transaction count:", fmt_num(sum(tx_df$tx_count, na.rm = TRUE)))
    )
  })

  # ----------------------------------
  # Dashboard plots
  # ----------------------------------
  output$dashboard_monthly_amount <- renderPlot({
    df <- filtered_tx_month() %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(total_amount = sum(total_amount, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No transactions available under the current filters."))

    ggplot(df, aes(month, total_amount)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Amount") +
      theme_minimal(base_size = 13)
  })

  output$dashboard_tx_mix <- renderPlot({
    df <- filtered_tx_month() %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(tx_count = sum(tx_count, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No transactions available under the current filters."))

    ggplot(df, aes(x = reorder(as.character(type), tx_count), y = tx_count)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Transaction Count") +
      theme_minimal(base_size = 13)
  })

  output$profile_age_plot <- renderPlot({
    df <- filtered_customers()
    ggplot(df, aes(age)) +
      geom_histogram(bins = 25) +
      labs(x = "Age", y = "Customers") +
      theme_minimal(base_size = 13)
  })

  output$profile_income_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(income_bracket, sort = TRUE)
    ggplot(df, aes(x = reorder(as.character(income_bracket), n), y = n)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Customers") +
      theme_minimal(base_size = 13)
  })

  output$profile_gender_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(gender)
    ggplot(df, aes(x = as.character(gender), y = n)) +
      geom_col() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Customers") +
      theme_minimal(base_size = 13)
  })

  output$profile_occupation_plot <- renderPlot({
    df <- filtered_customers() %>%
      dplyr::count(occupation, sort = TRUE) %>%
      dplyr::slice_head(n = 10)

    ggplot(df, aes(x = reorder(as.character(occupation), n), y = n)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Customers") +
      theme_minimal(base_size = 13)
  })

  output$segment_size_plot <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(customer_segment, sort = TRUE)
    ggplot(df, aes(x = reorder(as.character(customer_segment), n), y = n)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Customers") +
      theme_minimal(base_size = 13)
  })

  output$segment_clv_plot <- renderPlot({
    df <- filtered_customers() %>%
      dplyr::group_by(customer_segment) %>%
      dplyr::summarise(avg_clv = mean(customer_lifetime_value, na.rm = TRUE), .groups = "drop")

    ggplot(df, aes(x = reorder(as.character(customer_segment), avg_clv), y = avg_clv)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Average CLV") +
      theme_minimal(base_size = 13)
  })

  output$segment_churn_plot <- renderPlot({
    df <- filtered_customers() %>%
      dplyr::group_by(customer_segment) %>%
      dplyr::summarise(avg_churn = mean(churn_probability, na.rm = TRUE), .groups = "drop")

    ggplot(df, aes(x = reorder(as.character(customer_segment), avg_churn), y = avg_churn)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "Average Churn Probability") +
      theme_minimal(base_size = 13)
  })

  # ----------------------------------
  # Data Analysis plots
  # ----------------------------------
  output$profile_heatmap <- renderPlot({
    df <- filtered_customers() %>% dplyr::count(gender, income_bracket)

    ggplot(df, aes(x = as.character(gender), y = as.character(income_bracket), fill = n)) +
      geom_tile() +
      geom_text(aes(label = scales::comma(n)), size = 3.4, color = "white") +
      labs(x = NULL, y = NULL, fill = "Count") +
      theme_minimal(base_size = 13)
  })

  output$channel_clv_plot <- renderPlot({
    df <- filtered_customers() %>%
      dplyr::group_by(acquisition_channel) %>%
      dplyr::summarise(avg_clv = mean(customer_lifetime_value, na.rm = TRUE), .groups = "drop")

    ggplot(df, aes(x = reorder(as.character(acquisition_channel), avg_clv), y = avg_clv)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Average CLV") +
      theme_minimal(base_size = 13)
  })

  output$satisfaction_clv_plot <- renderPlot({
    df <- filtered_customers()
    ggplot(df, aes(x = clv_segment, y = satisfaction_score)) +
      geom_boxplot() +
      labs(x = "CLV Segment", y = "Satisfaction Score") +
      theme_minimal(base_size = 13)
  })

  output$tenure_segment_plot <- renderPlot({
    df <- filtered_customers()
    ggplot(df, aes(x = customer_segment, y = customer_tenure)) +
      geom_boxplot() +
      labs(x = "Customer Segment", y = "Customer Tenure") +
      theme_minimal(base_size = 13)
  })

  output$monthly_count_plot <- renderPlot({
    df <- filtered_tx_month() %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(tx_count = sum(tx_count, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No transactions available under the current filters."))

    ggplot(df, aes(month, tx_count)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Transaction Count") +
      theme_minimal(base_size = 13)
  })

  output$monthly_amount_type_plot <- renderPlot({
    df <- filtered_tx_month() %>%
      dplyr::group_by(month, type) %>%
      dplyr::summarise(total_amount = sum(total_amount, na.rm = TRUE), .groups = "drop")

    validate(need(nrow(df) > 0, "No transactions available under the current filters."))

    ggplot(df, aes(month, total_amount, group = type)) +
      geom_line(linewidth = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = "Amount", colour = "Type") +
      facet_wrap(~ type, scales = "free_y") +
      theme_minimal(base_size = 12)
  })

  output$weekend_ratio_plot <- renderPlot({
    df <- filtered_customers()
    ggplot(df, aes(x = customer_segment, y = weekend_transaction_ratio)) +
      geom_boxplot() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Customer Segment", y = "Weekend Transaction Ratio") +
      theme_minimal(base_size = 13)
  })

  output$daily_tx_tenure_plot <- renderPlot({
    df <- filtered_customers()
    if (nrow(df) > 5000) df <- dplyr::slice_sample(df, n = 5000)

    ggplot(df, aes(customer_tenure, avg_daily_transactions)) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Customer Tenure", y = "Average Daily Transactions") +
      theme_minimal(base_size = 13)
  })

  output$clv_churn_scatter <- renderPlot({
    df <- filtered_customers()
    if (nrow(df) > 5000) df <- dplyr::slice_sample(df, n = 5000)

    ggplot(df, aes(churn_probability, customer_lifetime_value, colour = clv_segment)) +
      geom_point(alpha = 0.45) +
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Churn Probability", y = "Customer Lifetime Value", colour = "CLV Segment") +
      theme_minimal(base_size = 13)
  })

  output$driver_corr_plot <- renderPlot({
    df <- driver_correlation_tbl() %>% dplyr::slice_head(n = 10)

    ggplot(df, aes(x = reorder(variable, abs(correlation)), y = correlation)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Correlation") +
      theme_minimal(base_size = 13)
  })

  output$driver_corr_tbl <- renderDT({
    DT::datatable(
      driver_correlation_tbl(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  # ----------------------------------
  # Clustering outputs
  # ----------------------------------
  output$cluster_scatter_plot <- renderPlot({
    cl <- cluster_result()
    validate(need(!is.null(cl), "Unable to run clustering. Check selected variables and sample size."))

    ggplot(cl$data, aes(PC1, PC2, colour = cluster)) +
      geom_point(alpha = 0.7) +
      labs(x = "PC1", y = "PC2", colour = "Cluster") +
      theme_minimal(base_size = 13)
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

    ggplot(cl$profile_long, aes(x = feature, y = cluster, fill = std_value)) +
      geom_tile() +
      geom_text(aes(label = round(mean_value, 2)), size = 3.2, colour = "white") +
      labs(x = NULL, y = NULL, fill = "Std. Mean") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

    ggplot(cl$data, aes(x = cluster, y = .data[[input$cluster_compare_metric]], fill = cluster)) +
      geom_boxplot(show.legend = FALSE) +
      labs(x = "Cluster", y = input$cluster_compare_metric) +
      theme_minimal(base_size = 13)
  })

  output$cluster_mix_tbl <- renderDT({
    cl <- cluster_result()
    validate(need(!is.null(cl), "Unable to run clustering. Check selected variables and sample size."))

    mix_tbl <- cl$data %>%
      dplyr::count(cluster, customer_segment) %>%
      dplyr::group_by(cluster) %>%
      dplyr::mutate(share = n / sum(n)) %>%
      dplyr::ungroup()

    DT::datatable(mix_tbl, options = list(pageLength = 10, scrollX = TRUE))
  })

  # ----------------------------------
  # Decision tree outputs
  # ----------------------------------
  output$tree_status_ui <- renderUI({
    tr <- tree_result()
    msg <- if (is.null(tr)) {
      "Click Train Decision Tree to fit the current model on the filtered master dataset."
    } else {
      paste0("Model trained on ", fmt_num(tr$prepared_rows), " prepared rows using ", length(tr$predictors), " predictors. Accuracy: ", fmt_pct(tr$accuracy, 1), ".")
    }
    div(class = "insight-box", msg)
  })

  output$tree_plot <- renderPlot({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))

    rpart.plot::rpart.plot(
      tr$model,
      type = 4,
      extra = 104,
      fallen.leaves = TRUE,
      tweak = 1.05,
      box.palette = "GnBu"
    )
  })

  output$tree_accuracy_card <- renderUI({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    metric_card("Accuracy", fmt_pct(tr$accuracy), "Test-set accuracy")
  })

  output$tree_train_card <- renderUI({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    metric_card("Train Rows", fmt_num(tr$train_n), paste(length(tr$predictors), "predictors used"))
  })

  output$tree_test_card <- renderUI({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    metric_card("Test Rows", fmt_num(tr$test_n), paste("Predictor set:", input$predictor_group))
  })

  output$tree_leaf_card <- renderUI({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    metric_card("Terminal Nodes", fmt_num(tr$terminal_nodes), "Interpretability remains explicit")
  })

  output$tree_confusion_plot <- renderPlot({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))

    ggplot(tr$conf_df, aes(x = Predicted, y = Actual, fill = Percent)) +
      geom_tile() +
      geom_text(aes(label = scales::comma(Freq)), colour = "white", size = 4) +
      scale_fill_gradient(labels = scales::percent) +
      labs(x = "Predicted", y = "Actual", fill = "Row %") +
      theme_minimal(base_size = 13)
  })

  output$tree_importance_plot <- renderPlot({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    validate(need(nrow(tr$importance_tbl) > 0, "No variable importance available for the current tree."))

    imp <- tr$importance_tbl %>% dplyr::slice_head(n = 10)

    ggplot(imp, aes(x = reorder(variable, importance), y = importance)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Importance") +
      theme_minimal(base_size = 13)
  })

  output$tree_recall_tbl <- renderDT({
    tr <- tree_result()
    if (is.null(tr)) {
      return(DT::datatable(data.frame(Status = "Run the decision tree to populate recall metrics."), options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(tr$recall_df, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  }, server = FALSE)

  output$tree_rules_text <- renderText({
    tr <- tree_result()
    validate(need(!is.null(tr), "Unable to train decision tree under the current filters."))
    paste(tr$rules_txt, collapse = "\n")
  })

  output$tree_leaf_tbl <- renderDT({
    tr <- tree_result()
    if (is.null(tr)) {
      return(DT::datatable(data.frame(Status = "Run the decision tree to populate terminal nodes."), options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(tr$leaf_tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)

  # ----------------------------------
  # Business insights
  # ----------------------------------
  output$business_findings_ui <- renderUI({
    df <- filtered_customers()
    if (nrow(df) == 0) {
      return(div(class = "insight-box", "No customers remain under the current filters."))
    }

    tx_df <- filtered_tx_month()
    tr <- tree_result()

    highest_value_segment <- df %>%
      dplyr::group_by(customer_segment) %>%
      dplyr::summarise(avg_clv = mean(customer_lifetime_value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::slice_max(avg_clv, n = 1, with_ties = FALSE)

    highest_risk_segment <- df %>%
      dplyr::group_by(customer_segment) %>%
      dplyr::summarise(avg_churn = mean(churn_probability, na.rm = TRUE), .groups = "drop") %>%
      dplyr::slice_max(avg_churn, n = 1, with_ties = FALSE)

    tx_peak_month <- if (nrow(tx_df) > 0) {
      tx_df %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(total_amount = sum(total_amount, na.rm = TRUE), .groups = "drop") %>%
        dplyr::slice_max(total_amount, n = 1, with_ties = FALSE)
    } else NULL

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
      metric = c(
        "Customers", "Average CLV", "Average Churn Probability", "Average Satisfaction",
        "Average Active Products", "Average App Logins", "Average Tx Value"
      ),
      value = c(
        fmt_num(nrow(seg)),
        fmt_num(mean(seg$customer_lifetime_value, na.rm = TRUE)),
        fmt_pct(mean(seg$churn_probability, na.rm = TRUE)),
        round(mean(seg$satisfaction_score, na.rm = TRUE), 2),
        round(mean(seg$active_products, na.rm = TRUE), 2),
        round(mean(seg$app_logins_frequency, na.rm = TRUE), 2),
        fmt_num(mean(seg$avg_tx_value, na.rm = TRUE))
      )
    )

    DT::datatable(tbl, options = list(dom = "t", pageLength = 10))
  })

  output$biz_driver_plot <- renderPlot({
    df <- business_driver_tbl() %>% dplyr::slice_head(n = 10)
    validate(need(nrow(df) > 0, "No CLV driver summary is available yet."))

    ggplot(df, aes(x = reorder(variable, abs(importance)), y = importance)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = ifelse(any(df$source == "Decision Tree Importance"), "Importance", "Correlation")) +
      theme_minimal(base_size = 13)
  })

  output$biz_driver_text <- renderUI({
    df <- business_driver_tbl()
    if (nrow(df) == 0) {
      return(div(class = "insight-box", "Run the decision tree or use the CLV/Churn correlation panel to surface drivers."))
    }

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
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Status = "No CLV driver summary is available yet."), options = list(dom = "t"), rownames = FALSE))
    }
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)

  # ----------------------------------
  # Data tables
  # ----------------------------------
  output$customer_data_tbl <- renderDT({
    df <- filtered_customers() %>%
      dplyr::select(
        customer_id, age, gender, location, income_bracket, occupation,
        customer_segment, clv_segment, active_products, satisfaction_score,
        churn_probability, customer_lifetime_value
      ) %>%
      dplyr::slice_head(n = 1000)

    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$transaction_data_tbl <- renderDT({
    df <- filtered_tx_preview()
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$master_data_tbl <- renderDT({
    df <- filtered_master() %>%
      dplyr::select(dplyr::any_of(c(
        "customer_id", "customer_segment", "clv_segment", "customer_lifetime_value",
        "churn_probability", "tx_n", "tx_total_amount", "tx_avg_amount",
        "tx_sd_amount", "active_days", "first_tx_date_tx", "last_tx_date_tx",
        "share_Deposit", "share_Payment", "share_Transfer", "share_Withdrawal",
        "monthly_n_sd", "monthly_amount_sd", "monthly_n_mean", "monthly_amount_mean"
      ))) %>%
      dplyr::slice_head(n = 1000)
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)

  # ----------------------------------
  # Downloads
  # ----------------------------------
  output$download_customers <- downloadHandler(
    filename = function() {
      paste0("filtered_customers_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_customers(), file)
    }
  )

  output$download_master <- downloadHandler(
    filename = function() {
      paste0("filtered_master_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_master(), file)
    }
  )
}

shinyApp(ui, server)
