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






