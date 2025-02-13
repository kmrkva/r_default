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



# Create "choose_default" columns for each choice domain
#create_choose_default_columns <- function(df, choice2_col, default_val, new_col) {
#  df[[new_col]] <- ifelse(df[[choice2_col]] == default_val, 1, 0)
#  return(df)
#}

# Apply the function to each choice domain using default values
#for (i in 1:length(choice_names)) {
#  d12 <- create_choose_default_columns(d12, paste0(choice_names[i], "_choice2"), default_df$Default_Val[i], paste0("choose_default_", choice_names[i]))
#}


# Move values from FL_285_DO to FL_348_DO to the corresponding new columns
fl_cols <- c("FL_285_DO", "FL_290_DO", "FL_133_DO", "FL_297_DO", "FL_316_DO", "FL_230_DO", "FL_325_DO", "FL_233_DO", "FL_331_DO", "FL_236_DO", 
             "FL_337_DO", "FL_239_DO", "FL_342_DO", "FL_240_DO", "FL_348_DO")

# Identify the starting column for the new columns
start_col <- which(names(d12) == "Adobe.1choice.bothconditions.def.nodef._DO")

# Loop through the FL columns and update the corresponding new columns
for (i in 1:length(fl_cols)) {
  new_col_index <- start_col + (i - 1)
  d12[, new_col_index] <- ifelse(!is.na(d12[[fl_cols[i]]]) & d12[[fl_cols[i]]] != "", d12[[fl_cols[i]]], d12[, new_col_index])
}
