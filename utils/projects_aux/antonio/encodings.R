# This file was created in 2025 January to Help Antonio solve problems with his data.
# I found the files have multiple encoding problems so this is one file to help trasnform the data to a useful encoding


# Read the file
library(readr)
dataset_comp <- read_csv("~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q320_antonio/a01_cn__f01_dc__c01_lv/dataset_comp.csv")
colnames(dataset_comp)

# Avoid incorrect EOL (end of lines)
dataset_comp$CR <- gsub("\r|\r\n", "____", dataset_comp$CR)

# Write the file
write.table(dataset_comp %>% dplyr::select(-c(X_N,uuid,AB,OA,FU,LA,SN,Countries,IsoCountries,Institutions,RP,AF,DE,DI,C1,DT,J9,TI)),
            sep = "\t",
            fileEncoding = "UTF-8",
            row.names = FALSE,
            qmethod = "escape",
            "~/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive/Q320_antonio/a01_cn__f01_dc__c01_lv/dataset_comp.tsv")

dataset_comp$CR[5778]

dataset$WC