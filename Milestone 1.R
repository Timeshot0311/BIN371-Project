df1 <- read.csv("anthropometry_national_zaf.csv")
# print(df1)
df2 <- read.csv("literacy_national_zaf.csv")
# print(df2)
df3 <- read.csv("maternal-mortality_national_zaf.csv")
# print(df3)
df4 <- read.csv("symptoms-of-acute-respiratory-infection-ari_national_zaf.csv")
# print(df4)

df_list <- list(df1, df2, df3, df4)

# Function to find duplicates
check_duplicates <- function(data, show = TRUE) {
  duplicate_rows <- data[duplicated(data), ]
  
  if (show) {
    if (nrow(duplicate_rows) > 0) {
      cat("Duplicates found:\n")
      print(duplicate_rows)
    } else {
      cat("No duplicate rows found.\n")
    }
  }
  return(invisible(duplicate_rows))
}

#Function to check empty values
check_empty_values <- function(data) {
  na_counts <- sapply(data, function(x) sum(is.na(x)))
  empty_counts <- sapply(data, function(x) sum(x == "", na.rm = TRUE))
  string_na_counts <- sapply(data, function(x) sum(x == "NA", na.rm = TRUE))
  
  result <- data.frame(
    Column = names(data),
    NA_Count = na_counts,
    Empty_String_Count = empty_counts,
    "Text_'NA'_Count" = string_na_counts
  )
  
  result <- result[rowSums(result[,-1]) > 0, ]
  
  if (nrow(result) == 0) {
    cat("No missing or empty values found.\n")
  } else {
    cat("Columns with missing/empty values:\n")
    print(result)
  }
  
  return(invisible(result))
}

#Dataframe Names
print("Dataframe Column Names")
for(i in 1:length(df_list)){
  cat("Dataframe", i, "\n")
  print(names(df_list[[i]]))
  cat("\n")
}

#Duplicates Check
print("Dataframe Duplication Tests")
for(i in 1:length(df_list)){
  cat("Dataframe", i, "\n")
  check_duplicates(df_list[[i]])
  cat("\n")
}

#Empty Values Check
print("Dataframe Empty Valus Check")
for(i in 1:length(df_list)){
  cat("Dataframe", i, "\n")
  check_empty_values(df_list[[i]])
  cat("\n")
}

# ===================== ADD-ONS (reports + visuals + numeric fix) =====================

# libs + output dir
suppressWarnings(suppressMessages({
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
  library(tidyverse)   # dplyr, ggplot2, readr, purrr
}))
out_dir <- "outputs_m1"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Name the dataframes for reporting
df_names <- c("Anthropometry","Literacy","MaternalMortality","ARI_Symptoms")
names(df_list) <- df_names

# ---------- 1) Dimensions ----------
dims <- tibble(
  Dataset = df_names,
  Rows    = sapply(df_list, nrow),
  Columns = sapply(df_list, ncol)
)
write.csv(dims, file.path(out_dir, "01_dimensions.csv"), row.names = FALSE)
print(dims)

# ---------- 2) Missingness per column (incl. empty strings and text 'NA') ----------
missing_tbl <- function(df, name){
  tibble(
    Dataset     = name,
    Column      = names(df),
    Missing     = sapply(df, function(x) sum(is.na(x))),
    EmptyString = sapply(df, function(x) if(is.character(x) || is.factor(x)) sum(x == "", na.rm = TRUE) else 0),
    TextNA      = sapply(df, function(x) if(is.character(x) || is.factor(x)) sum(x == "NA", na.rm = TRUE) else 0),
    PctMissing  = round(100 * sapply(df, function(x) sum(is.na(x))) / nrow(df), 2)
  ) %>% arrange(desc(Missing))
}

missing_all <- bind_rows(
  missing_tbl(df_list[[1]], names(df_list)[1]),
  missing_tbl(df_list[[2]], names(df_list)[2]),
  missing_tbl(df_list[[3]], names(df_list)[3]),
  missing_tbl(df_list[[4]], names(df_list)[4])
)
write.csv(missing_all, file.path(out_dir, "02_missingness.csv"), row.names = FALSE)
print(head(missing_all, 20))

# ---------- 3) Coerce Value safely -> Value_num (handles "12%", "1,234", etc.) ----------
make_value_num <- function(df) {
  if ("Value" %in% names(df)) {
    df %>% mutate(Value_num = readr::parse_number(as.character(Value)))
  } else {
    df
  }
}

df_list <- lapply(df_list, make_value_num)

# Quick sanity check of originally non-numeric Value entries
for (i in seq_along(df_list)) {
  if ("Value" %in% names(df_list[[i]])) {
    bad <- suppressWarnings(sum(is.na(as.numeric(df_list[[i]]$Value)) & !is.na(df_list[[i]]$Value)))
    cat(names(df_list)[i], "- non-numeric Value entries initially:", bad, "\n")
  }
}

# ---------- 4) Numeric summaries using Value_num ----------
num_summary <- function(df, name){
  if (!"Value_num" %in% names(df)) {
    return(tibble(
      Dataset = name,
      Metric  = "Value_num",
      Min     = NA_real_,
      Mean    = NA_real_,
      Median  = NA_real_,
      Max     = NA_real_,
      SD      = NA_real_
    ))
  }
  v <- df$Value_num
  tibble(
    Dataset = name,
    Metric  = "Value_num",
    Min     = suppressWarnings(min(v, na.rm = TRUE)),
    Mean    = suppressWarnings(mean(v, na.rm = TRUE)),
    Median  = suppressWarnings(median(v, na.rm = TRUE)),
    Max     = suppressWarnings(max(v, na.rm = TRUE)),
    SD      = suppressWarnings(sd(v, na.rm = TRUE))
  )
}

num_sum_all <- bind_rows(
  num_summary(df_list[[1]], names(df_list)[1]),
  num_summary(df_list[[2]], names(df_list)[2]),
  num_summary(df_list[[3]], names(df_list)[3]),
  num_summary(df_list[[4]], names(df_list)[4])
)
write.csv(num_sum_all, file.path(out_dir, "03_numeric_summaries.csv"), row.names = FALSE)
print(num_sum_all)

# ---------- 5) Outlier scan (IQR rule) on Value_num ----------
outlier_scan <- function(df, name){
  if (!"Value_num" %in% names(df)) return(tibble(Dataset=name, Outliers=NA_integer_))
  v <- df$Value_num
  if (all(is.na(v))) return(tibble(Dataset=name, Outliers=NA_integer_))
  q1  <- quantile(v, 0.25, na.rm=TRUE); q3 <- quantile(v, 0.75, na.rm=TRUE)
  iqr <- q3 - q1
  flags <- v < (q1 - 1.5*iqr) | v > (q3 + 1.5*iqr)
  tibble(Dataset=name, Outliers=sum(flags, na.rm=TRUE))
}

outliers_all <- bind_rows(
  outlier_scan(df_list[[1]], names(df_list)[1]),
  outlier_scan(df_list[[2]], names(df_list)[2]),
  outlier_scan(df_list[[3]], names(df_list)[3]),
  outlier_scan(df_list[[4]], names(df_list)[4])
)
write.csv(outliers_all, file.path(out_dir, "04_outliers_value_num.csv"), row.names = FALSE)
print(outliers_all)

# ---------- 6) Plots / EDA (saved to PNGs) ----------
has_cols <- function(df, cols) all(cols %in% names(df))

# 6.1 Histogram of Value_num
for (nm in names(df_list)) {
  df <- df_list[[nm]]
  if ("Value_num" %in% names(df) && !all(is.na(df$Value_num))) {
    p <- ggplot(df, aes(x = Value_num)) +
      geom_histogram(bins = 20) +
      labs(title = paste(nm, "- Distribution of Value"), x = "Value", y = "Count") +
      theme_minimal()
    ggsave(filename = file.path(out_dir, paste0("plot_", nm, "_hist_value_num.png")),
           plot = p, width = 7, height = 4, dpi = 150)
  }
}

# 6.2 Boxplot Value_num by Sex (if column exists)
for (nm in names(df_list)) {
  df <- df_list[[nm]]
  if (has_cols(df, c("Sex","Value_num")) && !all(is.na(df$Value_num))) {
    p <- ggplot(df, aes(x = Sex, y = Value_num)) +
      geom_boxplot() +
      labs(title = paste(nm, "- Value by Sex"), x = "Sex", y = "Value") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("plot_", nm, "_box_by_sex.png")),
           p, width = 7, height = 4, dpi = 150)
  }
}

# 6.3 Boxplot Value_num by Age (if column exists)
for (nm in names(df_list)) {
  df <- df_list[[nm]]
  if (has_cols(df, c("Age","Value_num")) && !all(is.na(df$Value_num))) {
    p <- ggplot(df, aes(x = Age, y = Value_num)) +
      geom_boxplot() +
      labs(title = paste(nm, "- Value by Age Group"), x = "Age Group", y = "Value") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("plot_", nm, "_box_by_age.png")),
           p, width = 9, height = 5, dpi = 150)
  }
}

# 6.4 Line plot over Period (if Period exists)
for (nm in names(df_list)) {
  df <- df_list[[nm]]
  if (has_cols(df, c("Period","Value_num")) && !all(is.na(df$Value_num))) {
    # Try numeric Period if possible
    if (is.character(df$Period) || is.factor(df$Period)) {
      suppressWarnings(df$Period_num <- as.numeric(as.character(df$Period)))
      xcol <- ifelse(any(!is.na(df$Period_num)), "Period_num", "Period")
    } else {
      xcol <- "Period"
    }
    p <- ggplot(df, aes(x = .data[[xcol]], y = Value_num, group = 1)) +
      geom_line() + geom_point() +
      labs(title = paste(nm, "- Value over Time"), x = "Period", y = "Value") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("plot_", nm, "_line_over_time.png")),
           p, width = 8, height = 4, dpi = 150)
  }
}

# 6.5 Top 10 Indicators by mean(Value_num) (if Indicator exists)
for (nm in names(df_list)) {
  df <- df_list[[nm]]
  if (has_cols(df, c("Indicator","Value_num")) && !all(is.na(df$Value_num))) {
    top_ind <- df %>%
      group_by(Indicator) %>%
      summarise(mean_value = mean(Value_num, na.rm=TRUE), .groups = "drop") %>%
      slice_max(order_by = mean_value, n = 10)
    p <- ggplot(top_ind, aes(x = reorder(Indicator, mean_value), y = mean_value)) +
      geom_col() + coord_flip() +
      labs(title = paste(nm, "- Top 10 Indicators by Mean Value"),
           x = "Indicator", y = "Mean(Value)") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("plot_", nm, "_top10_indicator_mean.png")),
           p, width = 8, height = 5, dpi = 150)
  }
}

# ---------- 7) Cleaning plan skeleton for Milestone 2 ----------
clean_notes <- tribble(
  ~Dataset, ~Action, ~Reason,
  "Anthropometry",     "TBD", "e.g., drop rows with NA in Value_num; unify Sex levels",
  "Literacy",          "TBD", "e.g., keep relevant Age groups; standardize Region names",
  "MaternalMortality", "TBD", "e.g., impute small NA with median; remove obvious data-entry outliers",
  "ARI_Symptoms",      "TBD", "e.g., filter to recent Period; encode Subgroup consistently"
)
write.csv(clean_notes, file.path(out_dir, "05_cleaning_plan_skeleton.csv"), row.names = FALSE)

cat("\n=== Milestone 1 script complete ===\n",
    "See output folder:", normalizePath(out_dir), "\n",
    "Written files:\n",
    " - 01_dimensions.csv\n - 02_missingness.csv\n - 03_numeric_summaries.csv\n - 04_outliers_value_num.csv\n - 05_cleaning_plan_skeleton.csv\n",
    " - plot_* PNGs (histograms, boxplots, time-series, top indicators)\n")

# ===================== END – BIN371 Milestone 1 =====================





