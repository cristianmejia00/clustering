# 2023-10-06

# Get clean "patent as IP" dataset

# First import the base dataset using readr.
# Our base dataset is this one: https://academic-landscape.com/analysis/48195/0#c0
# Corresponding to the most up to date dataset of TS='patent'

# From it we need, the clusters that cover patents as IP
# And then, remove unnecessary papers (e.g. by keyword)

#################################################
# INCLUSION CRITERIA:

# These clusters are the clusters we need.
# I selected them by manually checking their contents
cls <- c(1,3,5,8,10,12,13,15,16,18,20,24,26,29,34,36,37,38,39,40,41,44,48,49,52,53,54,56,57,58)
selected <- dataset[dataset$"_C" %in% cls,]
nrow(selected) / nrow(dataset)

#################################################
# EXCLUSION CRITERIA:

# These are the UT of the papers mentioning these 4 keywords.
# I found these papers in a previous solution within the selected clusters
# But they do not refer to patent as IP. 
# The PATENT-1 study refers to a clinical trial that evaluated the efficacy and safety of riociguat, 
# a drug developed for the treatment of pulmonary arterial hypertension (PAH) 
# and chronic thromboembolic pulmonary hypertension (CTEPH).
test1 <- dataset[grepl("patent-1", tmp_text),"UT"]
test2 <- dataset[grepl("patent-2", tmp_text),"UT"]
test3 <- dataset[grepl("patent plus", tmp_text),"UT"]
test4 <- dataset[grepl("pulmonary arterial hypertension", tmp_text),"UT"]

banned <- unique(c(test1,test2,test3,test4))
banned

#load('banned.rdata')
selected <- selected[!(selected$UT %in% banned),]

#################################################
# Format and Save:
selected$'_N' <- NULL
selected$'_C' <- NULL
selected$'_D' <- NULL
selected$'_E' <- NULL
selected$'_F' <- NULL
selected$'_Y' <- NULL

write.table(selected, file='selected.txt', sep = '\t', row.names = FALSE)



#################################################
#################################################
#################################################
# Orphans

colnames(orphans)
orphans$X_N <- NULL
orphans$X_C <- NULL
orphans$X_D <- NULL
orphans$X_E <- NULL

write.table(orphans, file='orphans.tsv', sep = '\t', row.names = FALSE)
