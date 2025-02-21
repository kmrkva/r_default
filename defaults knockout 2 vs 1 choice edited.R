```rez
library(lme4)
library(lmerTest)

d12initial <- read.csv("/Users/Kellen_Mrkva/Documents/defaults 1 vs 2 choice n220.csv")

# Step 1: Remove the 2nd and 3rd rows from d12
d12_clean <- d12initial[-c(1:3), ]  # Remove rows 2 and 3

# Step 2: Save the cleaned data to a temporary file
temp_file <- tempfile(fileext = ".csv")
write.csv(d12_clean, temp_file, row.names = FALSE)  # Save without row names

# Step 3: Re-read the file to allow R to detect column types
d12 <- read.csv(temp_file, stringsAsFactors = FALSE)  # Use stringsAsFactors=FALSE for better type handling

# Function to identify choice domain columns based on filtering criteria
identify_choice_columns <- function(df, start_col, exclude_cols = c()) {
  start_index <- which(names(df) == start_col)  # Start at the given column
  valid_cols <- c()  # Store matching column names
  
  for (i in start_index:ncol(df)) {
    col_name <- names(df)[i]
    col_values <- df[[i]]
    
    # Skip columns already assigned to a previous choice domain
    if (col_name %in% exclude_cols) next 
    
    # Check if column is character and exclude unwanted values
    if (is.character(col_values) && 
        !any(grepl("Agree|Disagree|Strongly disagree|Strongly agree|\\||_", col_values, ignore.case = TRUE))) {
      
      valid_cols <- c(valid_cols, col_name)  # Store the column name
      
      # Stop when we find 6 columns
      if (length(valid_cols) == 6) {
        break
      }
    }
  }
  
  return(valid_cols)
}

# Initialize an empty list to track assigned columns and prevent duplicates
assigned_columns <- c()

# Identify the 6 columns for each choice domain
adobe_cols <- identify_choice_columns(d12, "def_adobe", assigned_columns)
assigned_columns <- c(assigned_columns, adobe_cols)

max_cols   <- identify_choice_columns(d12, adobe_cols[6], assigned_columns)
assigned_columns <- c(assigned_columns, max_cols)

wsj_cols   <- identify_choice_columns(d12, max_cols[6], assigned_columns)
assigned_columns <- c(assigned_columns, wsj_cols)

boxy_cols  <- identify_choice_columns(d12, wsj_cols[6], assigned_columns)
assigned_columns <- c(assigned_columns, boxy_cols)

amzn_cols  <- identify_choice_columns(d12, boxy_cols[6], assigned_columns)
assigned_columns <- c(assigned_columns, amzn_cols)

wal_cols   <- identify_choice_columns(d12, amzn_cols[6], assigned_columns)
assigned_columns <- c(assigned_columns, wal_cols)

gopro_cols <- identify_choice_columns(d12, wal_cols[6], assigned_columns)
assigned_columns <- c(assigned_columns, gopro_cols)

ret_cols   <- identify_choice_columns(d12, gopro_cols[6], assigned_columns)
assigned_columns <- c(assigned_columns, ret_cols)

debt_cols  <- identify_choice_columns(d12, ret_cols[6], assigned_columns)
assigned_columns <- c(assigned_columns, debt_cols)

# Print the identified column sets
print("Adobe columns:"); print(adobe_cols)
print("Max columns:"); print(max_cols)
print("WSJ columns:"); print(wsj_cols)
print("Boxy columns:"); print(boxy_cols)
print("Amazon columns:"); print(amzn_cols)
print("Walmart columns:"); print(wal_cols)
print("GoPro columns:"); print(gopro_cols)
print("Retail columns:"); print(ret_cols)
print("Debt columns:"); print(debt_cols)

# Function to print tables of observed values for each choice domain
print_value_tables <- function(df, col_set, label) {
  cat("\n----------------------------\n")
  cat("Observed values for", label, "\n")
  cat("----------------------------\n")
  
  for (col in col_set) {
    cat("\nColumn:", col, "\n")
    print(table(df[[col]], useNA = "ifany"))  # Print value frequencies
  }
}

# Print observed values for each choice domain
print_value_tables(d12, adobe_cols, "Adobe")
print_value_tables(d12, max_cols, "Max")
print_value_tables(d12, wsj_cols, "WSJ")
print_value_tables(d12, boxy_cols, "Boxy")
print_value_tables(d12, amzn_cols, "Amazon")
print_value_tables(d12, wal_cols, "Walmart")
print_value_tables(d12, gopro_cols, "GoPro")
print_value_tables(d12, ret_cols, "Retail")
print_value_tables(d12, debt_cols, "Debt")

# Swap order of 2 columns (only 1 where the no default condition was first in the df)

# Swap Q274 and Q275 in the dataframe while keeping their column positions
d12$Q275_temp <- d12$Q275
d12$Q274_temp <- d12$Q274

d12$Q275 <- d12$Q274_temp
d12$Q274 <- d12$Q275_temp

# Function to extract default value from the first column of each choice domain
extract_default_val <- function(df, first_col) {
  def_val <- df[[first_col]][grepl("^def ", df[[first_col]])]  # Find values starting with "def "
  
  if (length(def_val) > 0) {
    return(unique(def_val)[1])  # Return first unique match
  } else {
    return(NA)  # Return NA if no default value found
  }
}

# Create a data frame with choice labels, default values, and columns used
choice_labels <- c("adobe", "max", "wsj", "boxy", "amzn", "wal", "gopro", "ret", "debt")
choice_columns <- list(adobe_cols, max_cols, wsj_cols, boxy_cols, amzn_cols, wal_cols, gopro_cols, ret_cols, debt_cols)

# Extract default values for each choice domain
default_vals <- sapply(choice_columns, function(cols) extract_default_val(d12, cols[1]))

# Convert column lists to strings for easier display
col_strings <- sapply(choice_columns, function(cols) paste(cols, collapse = ", "))

# Create the final data frame
default_df <- data.frame(
  Choice_Type = choice_labels,
  Default_Val = default_vals,
  Columns_Used = col_strings,
  stringsAsFactors = FALSE
)

# Print the dataframe
print(default_df)

# Identify the 18 new choice columns (ensure these are declared after the columns are created)
choice_cols <- c("adobe_choice1", "adobe_choice2", "max_choice1", "max_choice2", 
                 "wsj_choice1", "wsj_choice2", "boxy_choice1", "boxy_choice2", 
                 "amzn_choice1", "amzn_choice2", "wal_choice1", "wal_choice2", 
                 "gopro_choice1", "gopro_choice2", "ret_choice1", "ret_choice2", 
                 "debt_choice1", "debt_choice2")

# Function to create choice columns for each domain
create_choice_columns <- function(df, domain_cols, choice1_col, choice2_col) {
  # Create the first choice column using the 3rd and 5th columns of the domain
  df[[choice1_col]] <- ifelse(!is.na(df[[domain_cols[3]]]) & df[[domain_cols[3]]] != "", 
                              df[[domain_cols[3]]], 
                              ifelse(!is.na(df[[domain_cols[5]]]) & df[[domain_cols[5]]] != "", 
                                     df[[domain_cols[5]]], NA))
  
  # Create the second choice column using the 1st, 2nd, 4th, and 6th columns of the domain
  df[[choice2_col]] <- ifelse(!is.na(df[[domain_cols[1]]]) & df[[domain_cols[1]]] != "", 
                              df[[domain_cols[1]]], NA)
  df[[choice2_col]] <- ifelse(!is.na(df[[domain_cols[2]]]) & df[[domain_cols[2]]] != "", 
                              df[[domain_cols[2]]], df[[choice2_col]])
  df[[choice2_col]] <- ifelse(!is.na(df[[domain_cols[4]]]) & df[[domain_cols[4]]] != "", 
                              df[[domain_cols[4]]], df[[choice2_col]])
  df[[choice2_col]] <- ifelse(!is.na(df[[domain_cols[6]]]) & df[[domain_cols[6]]] != "", 
                              df[[domain_cols[6]]], df[[choice2_col]])
  
  return(df)
}

# Apply the function to each choice domain
choice_domains <- list(adobe_cols, max_cols, wsj_cols, boxy_cols, amzn_cols, wal_cols, gopro_cols, ret_cols, debt_cols)
choice_names <- c("adobe", "max", "wsj", "boxy", "amzn", "wal", "gopro", "ret", "debt")
for (i in 1:length(choice_domains)) {
  d12 <- create_choice_columns(d12, choice_domains[[i]], paste0(choice_names[i], "_choice1"), paste0(choice_names[i], "_choice2"))
}

# Replace -99 with "None of the above" and clean up other values in the new choice columns
choice_cols <- c(paste0(choice_names, "_choice1"), paste0(choice_names, "_choice2"))
d12[choice_cols] <- lapply(d12[choice_cols], function(x) {
  x <- ifelse(x == -99, "None of the above", x)
  x <- gsub("^def ", "", x)
  return(x)
})

# Recode "Photogrpahy (1TB)" as "Photography (1TB)"
d12[choice_cols] <- lapply(d12[choice_cols], function(x) gsub("Photogrpahy \\(1TB\\)", "Photography (1TB)", x))

# Replace -99 with "None of the above"
d12[choice_cols] <- lapply(d12[choice_cols], function(x) ifelse(x == -99, "None of the above", x))

# Remove "def " from values in all 18 columns
d12[choice_cols] <- lapply(d12[choice_cols], function(x) gsub("^def ", "", x))

# Create "choose_nothing" columns for each choice domain
create_choose_nothing_columns <- function(df, choice1_col, choice2_col, new_col) {
  df[[new_col]] <- ifelse(df$single == 0,
                          ifelse(df[[choice1_col]] == "None of the above", 1, 0),
                          ifelse(df[[choice2_col]] == "None of the above", 1, 0))
  return(df)
}

# Apply the function to each choice domain
for (name in choice_names) {
  d12 <- create_choose_nothing_columns(d12, paste0(name, "_choice1"), paste0(name, "_choice2"), paste0("choose_nothing_", name))
}

# Print first few rows to verify
head(d12[c(choice_cols, paste0("choose_nothing_", choice_names))])

# Define the choice domains and corresponding columns
choice_domains <- c("adobe", "max", "wsj", "boxy", "amzn", "wal", "gopro", "ret", "debt")
choice2_cols <- paste0(tolower(choice_domains), "_choice2")
choice1_cols <- paste0(tolower(choice_domains), "_choice1")
default_cols <- c("Adobe.1choice.bothconditions.def.nodef._DO", "Max.1choice._DO", 
                  "WSJ.1choice_DO", "BoxyCharm.1choice.._DO", "Amazon.1choice_DO", 
                  "Walmart.1choice_DO", "GoPro.1choice_DO", "retirementamount.1choice_DO", 
                  "ccdebtrepay.1choice_DO")
choose_nothing_cols <- paste0("choose_nothing_", tolower(choice_domains))
fl_cols <- c("FL_285_DO", "FL_290_DO", "FL_297_DO", "FL_316_DO", "FL_325_DO", "FL_331_DO", "FL_337_DO", "FL_342_DO", "FL_348_DO")

# Check if all specified columns are present in the dataframe
all_cols <- c(choice2_cols, choice1_cols, default_cols, choose_nothing_cols, fl_cols)
missing_cols <- setdiff(all_cols, names(d12))
if (length(missing_cols) > 0) {
  stop("The following columns are missing in the dataframe: ", paste(missing_cols, collapse = ", "))
}

# Create a list of data frames, each containing the 9 rows for a single ResponseId
long_list <- lapply(1:nrow(d12), function(i) {
  base_row <- d12[i, ]
  long_df <- data.frame(
    ResponseId = base_row$ResponseId,
    choice_domain = choice_domains,
    choice2_val = as.character(unlist(base_row[choice2_cols])),
    choice1_val = as.character(unlist(base_row[choice1_cols])),
    default_val = NA,  # Initialize with NA
    choose_nothing = as.numeric(unlist(base_row[choose_nothing_cols]))
  )
  # Replicate other columns for each choice domain
  other_cols <- base_row[ , !(names(base_row) %in% c("ResponseId", choice2_cols, choice1_cols, default_cols, choose_nothing_cols, fl_cols))]
  long_df <- cbind(long_df, other_cols[rep(1, 9), ])
  
  # Assign the correct default_val based on the row position within each ResponseId group
  if (!is.na(base_row$single)) {
    if (base_row$single == 1) {
      long_df$default_val <- as.character(unlist(base_row[default_cols]))[1:9]
    } else {
      long_df$default_val <- as.character(unlist(base_row[fl_cols]))[1:9]
    }
  } else {
    long_df$default_val <- NA  # Handle NA case
  }
  
  return(long_df)
})

# Combine the list of data frames into a single data frame
d12_long <- do.call(rbind, long_list)

# Create the defaultdummy variable
d12_long$defaultdummy <- ifelse(grepl("Q274|Q269|Q290|Q353|def_|default_r|withdefault", d12_long$default_val), 1,
                                ifelse(grepl("nd_|nodefault|Q275|Q291|Q366", d12_long$default_val), 0, NA))

# coding whether the potential default was chosen.
cleaned_default_vals <- gsub("^def ", "", default_vals)

# Create default_chosen in d12_long
d12_long$default_chosen <- ifelse(
  is.na(d12_long$choice2_val) | d12_long$choice2_val == "", 
  d12_long$choice2_val,  # Keep NA or empty values as is
  ifelse(d12_long$choice2_val %in% cleaned_default_vals, 1, 0)
)

# Print first few rows to verify
head(d12_long[, c("choice2_val", "default_chosen")])

d12_long$default_chosen <- as.numeric(d12_long$default_chosen)

# coding whether they chose anything
# d12_long$choose_nothing<-NA
# d12_long$choose_nothing[d12_long$choice1_val=="None of the above"]<-1
# d12_long$choose_nothing[d12_long$choice1_val=="Choose something"]<-0

m1 <- lmer(default_chosen ~ defaultdummy + (1 | ResponseId), data = d12_long)
summary(m1)

```