d12initial<-read.csv("/Users/Kellen_Mrkva/Documents/defaults 1 vs 2 choice n220.csv")

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


#swap order of 2 columns (only 1 where the no default condition was first in the df)

# Swap Q274 and Q275
d12[, c("Q274", "Q275")] <- d12[, c("Q275", "Q274")]

# Ensure Q275 appears first in wal_cols
if ("Q274" %in% wal_cols & "Q275" %in% wal_cols) {
  wal_cols <- c("Q275", "Q274", setdiff(wal_cols, c("Q274", "Q275")))
}


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
choice_labels <- c("Adobe", "Max", "WSJ", "Boxy", "Amazon", "Walmart", "GoPro", "Retail", "Debt")
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

# Define a function to create the new columns for each choice domain
create_choice_columns <- function(domain_cols, choice1_col, choice2_col) {
  # Column 1 (choice1): Take data from the 3rd and 5th columns (if available)
  d12[[choice1_col]] <- ifelse(!is.na(d12[[domain_cols[3]]]) & d12[[domain_cols[3]]] != "", 
                               d12[[domain_cols[3]]], 
                               ifelse(!is.na(d12[[domain_cols[5]]]) & d12[[domain_cols[5]]] != "", 
                                      d12[[domain_cols[5]]], NA))
  
  # Column 2 (choice2): Take data from the 1st, 2nd, 4th, and 6th columns (if available)
  d12[[choice2_col]] <- d12[[domain_cols[1]]]
  d12[[choice2_col]] <- ifelse(!is.na(d12[[domain_cols[2]]]) & d12[[domain_cols[2]]] != "", 
                               d12[[domain_cols[2]]], d12[[choice2_col]])
  d12[[choice2_col]] <- ifelse(!is.na(d12[[domain_cols[4]]]) & d12[[domain_cols[4]]] != "", 
                               d12[[domain_cols[4]]], d12[[choice2_col]])
  d12[[choice2_col]] <- ifelse(!is.na(d12[[domain_cols[6]]]) & d12[[domain_cols[6]]] != "", 
                               d12[[domain_cols[6]]], d12[[choice2_col]])
}

# Apply this function to each of the 9 choice domains
create_choice_columns(adobe_cols, "adobe_choice1", "adobe_choice2")
create_choice_columns(max_cols, "max_choice1", "max_choice2")
create_choice_columns(wsj_cols, "wsj_choice1", "wsj_choice2")
create_choice_columns(boxy_cols, "boxy_choice1", "boxy_choice2")
create_choice_columns(amzn_cols, "amzn_choice1", "amzn_choice2")
create_choice_columns(wal_cols, "wal_choice1", "wal_choice2")
create_choice_columns(gopro_cols, "gopro_choice1", "gopro_choice2")
create_choice_columns(ret_cols, "ret_choice1", "ret_choice2")
create_choice_columns(debt_cols, "debt_choice1", "debt_choice2")

# Now, replace -99 with "None of the above" and clean up other values

# Ensure we have the newly created choice columns in the dataframe
d12[choice_cols] <- lapply(d12[choice_cols], function(x) {
  # Replace -99 with "None of the above"
  x <- ifelse(x == -99, "None of the above", x)
  # Remove "def " from values
  x <- gsub("^def ", "", x)
  return(x)
})

# Print first few rows to verify
head(d12[choice_cols])




####remove this assuming the changes work
# Identify the 18 new choice columns
choice_cols <- c("adobe_choice1", "adobe_choice2", "max_choice1", "max_choice2", 
                 "wsj_choice1", "wsj_choice2", "boxy_choice1", "boxy_choice2", 
                 "amzn_choice1", "amzn_choice2", "wal_choice1", "wal_choice2", 
                 "gopro_choice1", "gopro_choice2", "ret_choice1", "ret_choice2", 
                 "debt_choice1", "debt_choice2")

# Replace -99 with "None of the above"
d12[choice_cols] <- lapply(d12[choice_cols], function(x) ifelse(x == -99, "None of the above", x))

# Remove "def " from values in all 18 columns
d12[choice_cols] <- lapply(d12[choice_cols], function(x) gsub("^def ", "", x))

# Recode "Photogrpahy (1TB)" as "Photography (1TB)"
d12[choice_cols] <- lapply(d12[choice_cols], function(x) gsub("Photogrpahy \\(1TB\\)", "Photography (1TB)", x))

# Print first few rows to verify
head(d12[choice_cols])


