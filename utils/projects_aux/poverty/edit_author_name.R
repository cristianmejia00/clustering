library(readr)
library(dplyr)
mkt <- read_delim("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q318_MKT_poverty/a01_tm__f01_e01__km01_CORRECT_IN_USE/WOS/savedrecs.txt", 
                  delim = "\t", escape_double = FALSE, 
                  trim_ws = TRUE)


# Check colnames
colnames(mkt)

# Edit Author columns
mkt <- mkt %>% 
  mutate(
    AU = tolower(AU) %>% gsub('madhubalan', 'madhu', .),
    AF = tolower(AF) %>% gsub('madhubalan', 'madhu', .),
    C1 = tolower(AF) %>% gsub('madhubalan', 'madhu', .)
  )

# Write the file to use in VosViewer
write.csv(mkt, 'saverecs_edited.csv', row.names = FALSE)

# Check the papers by Viswanathan
test <- mkt %>% filter(grepl("viswan", AU))
test$AU


