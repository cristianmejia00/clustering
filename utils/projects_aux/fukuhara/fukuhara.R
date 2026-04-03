library(readr)
library(dplyr)

# Read file
savedrecs <- read_delim("~/Desktop/savedrecs.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)

# Format year
savedrecs$PY <- as.numeric(format(as.Date(savedrecs$DT, format = "%b %d %Y"), "%Y"))

# Rename columns
savedrecs <- savedrecs %>% rename(
  "Date" = "DT",
  "AB" = "LA",
  "DT" = "AB", 
  "Countries" = "PC",
)

# Add column CR
savedrecs$CR = "_"

# Add column DI
savedrecs$DI <- savedrecs$UT

# Write
write.table(savedrecs, file = "~/Desktop/savedrecs_formatted.txt", sep = '\t', row.names = FALSE)
