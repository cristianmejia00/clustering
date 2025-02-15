



abs_fields <- unique(ABS_2021_journal_rankings_v2$Field)

# First, install and load the required package if you haven't already
# install.packages("readxl")
library(readxl)

# File paths
excel_file <- "/Users/cristian/Desktop/CABS_2024/02_edited.xlsx"

# Get all sheet names from the Excel file
sheet_names <- excel_sheets(excel_file)

# Create a list to store all the data frames
all_sheets <- list()

# Read each sheet into the list
for(sheet in sheet_names) {
  all_sheets[[sheet]] <- read_excel(excel_file, sheet = sheet)
}

# Bind all sheets
ttt <- bind_rows(all_sheets)

#new_row
results <- list()
nr <- c()
p <- 0
for (i in ttt$Field) {
  # 01 - Field
  if (i %in% abs_fields) {
    print("new_row_found")
    p <- 1
    results <- c(results, list(nr))
    nr <- c()
    nr[1] <- i
    next
  }
  # 02 - Journal
  if (p == 1) {
    p <- 2
    nr[2] <- i
    next
  }
  # 03 - ABS 2024
  if (p == 2) {
    p <- 3
    nr[3] <- i
    print(nr)
    next
  }
}

# To data frame
results <- results[2:length(results)]
lll <- as.data.frame.list(results) %>% t() %>% as.data.frame()
rownames(lll) <- NULL
colnames(lll) <- c("Field", "Journal", "AJG 2024")

# Write
write.csv(lll, file="cabs_ajg_2024.csv", row.names = FALSE)
?tempfile
