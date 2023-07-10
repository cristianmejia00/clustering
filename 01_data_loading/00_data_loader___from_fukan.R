# Dataset treatment for Fukan System datasets

# Inputs: 
# - Path to the folder having the `mission.facet.all.tsv` and `mission.facet.orphans.tsv`
# - Path to the folder with the `mission.pairs.tsv` file

# Output:
# - dataset.csv and orphans.csv properly formatted for my topic analyzer
# - copy of mission.pairs.tsv as network.csv
# - all saved to the `/inputs` folder

# "Properly fomratted" means to remove unnecessaruy columns, check column types, and check column headers.

###########################################################################################
# OPTIONS
###########################################################################################
## Select the input folders:
dataset_folder <- choose.dir()
network_folder <- choose.dir()


## Query_id 
## This has de form Qxxx whith the query number from the query control file
dataset_metadata <- list("query_id" = "Q269", 
                         "fukan_url" = "https://academic-landscape.com/analysis/47540/0#c0")


###########################################################################################
# RUN FROM HERE
###########################################################################################
## Libraries
source("04_utils/02_libraries.R")
source(file.path(getwd(), "04_utils", "read_from_fukan_function.R"))

## Path to `/inputs`
input_folder <- "C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics"

## Read files
dataset <- read_from_fukan_2(dataset_folder)
orphans <- read_from_fukan_2(dataset_folder, what = "orphans")
network <- fread(paste(network_folder, "\\mission.pairs.tsv", sep = ""), sep = '\t')

## Create directories
dir.create(file.path(input_folder, dataset_metadata$query_id), showWarnings = FALSE)
save(dataset,orphans,network,dataset_metadata, 
     file = file.path(input_folder, dataset_metadata$query_id, "dataset.rdata"))

## Clear environment
if (file.exists(file.path(input_folder, dataset_metadata$query_id, "dataset.rdata"))) {
  #rm(list = (setdiff(ls(), c("dataset", "orphans", "network", "dataset_metadata"))))
  rm(list = (ls()))
}
