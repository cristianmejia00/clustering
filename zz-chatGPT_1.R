# 20230715

# Using OpenAI in R.


# Libraries
library(reticulate)
library(glue)

##########################################################
# Load libraries
source("04_utils/02_libraries.R")

# Load settings from the project we are interested in
source("settings.R")


# One time operation to generate a python env
#reticulate::conda_create(envname = 'openai_env', packages = 'openai', python_version = '3.9')

# Activate enviroment
reticulate::use_condaenv('openai_env')

# Attach key.
# In VSCode create a file `openai.key`
# Is only one line with the OpenAi key.
# `credentials/openai.key` was added to .gitignore so is not committed to the repo.
# import Openai Python library
openai <- reticulate::import('openai')
client = openai$OpenAI(api_key = readr::read_file('05_assets/credentials/openai.key'))


# utils
#' @description
#' Get answers from OpenAI's GPT. Here used for ARTICLE summarization.
#' @param prompt LIST. A prompt in the format of OpenAI. See the code `zz-prompts.R` for details.
#' @param model STRING {gpt-3.5-turbo-0613} the OpenAI Moodel to use. Options: gpt-3.5-turbo-0613, gpt-4, 'gpt-4-0613'
#' @param temperature NUMBER. Between 0 and 2. 0 means less randomness and 2 more creative.
#' @param max_tokens INTEGER. The approx MAX size possible for the reply from ChatGPT.
#' @param n INTEGER. Number of reply variations to get.
#' @returns The JSON reply from OpenAI in R's LIST form. The actual reply text is located at `x$choices[[1]]$message$content` 
ask_gpt <- function(prompt, 
                    model = 'gpt-4o',#'gpt-3.5-turbo-0125', 
                    temperature = 0.1, 
                    max_tokens = 500, 
                    n = 1) {
  response <- client$chat$completions$create(model = model, 
                                             messages = prompt,
                                             temperature = temperature,
                                             max_tokens = as.integer(max_tokens),
                                             n = as.integer(n))
}



#' @description
#' Function to get a subset of the cluster containing the combination of
#' top 5 most linked (X_E), most cited (Z9), and Most linked of the most recent
#' @param dataset DATAFRAME. the dataset
#' @param cluster INTEGER. the cluster number to subset. Compatible with X_C, meaning sypport for cluster 99.
#' @returns DATAFRAME. The largest possible is of `top * 3` when all 3 conditions are different
get_cluster_data <- function(dataset, cluster, top = 5) {
  cluster_data <- subset(dataset, X_C == cluster, select = c('X_C','TI','AB','AU','PY','UT','Z9','X_E', 'summary'))
  if (nrow(cluster_data) > top) {
    selected_papers <- c(
      # Most connected
      cluster_data$UT[order(cluster_data$X_E, decreasing = TRUE)][1:top],
      # Most cited
      cluster_data$UT[order(cluster_data$Z9, decreasing = TRUE)][1:top],
      # Newest most connected (X_E is preferred over Z9 because most of paper wont have citations)
      cluster_data$UT[order(cluster_data$PY, cluster_data$X_E, decreasing = TRUE)][1:top]
    ) %>% unique()
    # Only retain selected papers
    cluster_data <- cluster_data[cluster_data$UT %in% selected_papers,]
  }
  cluster_data$text <- paste(cluster_data$TI, cluster_data$AB, sep = ' ')
  return(cluster_data)
}

#' @description
#' AskGPT to summarize each article in the given dataset. Each summary is appended to column `summary`
#' @param dataset DATAFRAME. the dataset
#' @returns DATAFRAME. the same dataset with the column summary appended.
get_papers_summary <- function(cl_dataset) {
  #cl_dataset$summary <- ''
  starting <- 1
  ending <- nrow(cl_dataset)
  while(starting < ending) {
    for(idx in c(starting:ending)) {
      print(paste(cl_dataset$X_C[idx], as.character(idx), cl_dataset$TI[idx], sep = "; "))
      article_summary <- tryCatch({
        if (nchar(cl_dataset$summary[idx]) == 0) {
          article_summary <- ask_gpt(prompt_summarize_a_paper(topic = MAIN_TOPIC,
                                                              topic_description = MAIN_TOPIC_DESCRIPTION,
                                                              article_text = cl_dataset$text[idx]),
                                     temperature = 0.7)
          cl_dataset$summary[idx] <- article_summary$choices[[1]]$message$content
          #article_summary
        }
      },
      error = function(err){
        message(glue('error found in {idx}'))
        message(err)
      }, 
      finally = {
        starting <- idx
        Sys.sleep(5)
      }) 
    }
    #starting <- idx
  }
  return(cl_dataset)
}

###################################
###################################
# Initialize
###################################
#rcs_merged <- rcs
rcs_merged$description <- ''
rcs_merged$name <- ''
dataset$summary <- ''
source("zz-chatGPT_0_prompts.R")

###################################
###################################
# Article summary
###################################
# The oldest article(s) in the dataset.
# When there are many "old papers" we analyze only the two most cited.
oldest_year <- 1910#min(dataset$PY, na.rm = TRUE)
oldest_data <- subset(dataset, PY <= oldest_year)#subset(dataset, PY == oldest_year)
if (nrow(oldest_data) > 2) {
  oldest_data <- oldest_data[order(oldest_data$Z9, decreasing = FALSE)[c(1:2)],]
}
oldest_data$summary <- ''
for (i in nrow(oldest_data)) {
  old_UT <- oldest_data$UT[i]
  old_summary <- ask_gpt(prompt_summarize_a_paper(topic = MAIN_TOPIC,
                                                  topic_description = MAIN_TOPIC_DESCRIPTION,
                                                  article_text = paste(oldest_data$TI[i], oldest_data$AB[i], sep = ' ')))
  oldest_data$summary[i] <- old_summary$choices[[1]]$message$content
  dataset$summary[which(dataset$UT == old_UT)] <- old_summary$choices[[1]]$message$content
}

# The following are needed but they are covered in the next block. 
# The most cited article in the dataset
# The top 3 most connected per cluster 
# The top 3 most cited per cluster

###################################
###################################
# Cluster description and name
###################################
# Only needed once
# rcs_merged$description <- ''
# rcs_merged$name <- ''
# dataset$summary <- ''
#list_of_clusters <- dataset$X_C %>% unique() %>% sort()

# Start where the loop was interrupted
# list_of_clusters <- dataset$X_C %>% unique() %>% sort()
# list_of_clusters <- list_of_clusters[list_of_clusters != 99]
list_of_clusters <- paste(c("9-3", "9-2", "7-6", "3-7", "19-0",
                      "10-5", "10-4", "10-6", "11-5", "11-1",
                      "4-4", "4-8", "4-1", "4-3", "4-10",
                      "7-7", "13-0", "1-1", "11-6", "1-6", 
                      "2-1", "1-1", "1-2", "5-1", "1-3"), "---",  sep = '')
list_of_clusters <- rcs$cluster[gsub("---","",rcs$cluster_code) %in% c("9-3", "9-2", "7-6", "3-7", "19-0",
                                                                      "10-5", "10-4", "10-6", "11-5", "11-1",
                                                                      "4-4", "4-8", "4-1", "4-3", "4-10",
                                                                      "7-7", "13-0", "1-1", "11-6", "1-6", 
                                                                      "2-1", "1-1", "1-2", "5-1", "1-3")]
# list_of_clusters <- list_of_clusters[c(13:length(list_of_clusters))]
# list_of_clusters <- list(5)
dataset$X_C <- dataset$subcluster_label1
rcs_merged$cluster <- rcs$cluster_code

for (cluster in list_of_clusters) {
  # Get this cluster tops
  print('=================================================================')
  print(glue('cluster: {cluster}'))
  cluster_data <- get_cluster_data(dataset, cluster = cluster, top = 3)
  # Summarize each of the selected papers
  cluster_data <- get_papers_summary(cluster_data)
  # Assign the summaries to the main dataset
  print('asign summaries to main dataset')
  dataset$summary[match(cluster_data$UT, dataset$UT)] <- cluster_data$summary
  
  # Generate the bulk text
  print('get bulk text')
  print(glue('Total selected papers for this cluster: {nrow(cluster_data)}'))
  my_texts <- list()
  for (i in c(1:min(10,nrow(cluster_data)))) {
    my_texts[i] <- glue('##### {cluster_data$text[[i]]}')
  }
  #print(length(my_texts))
  my_texts <- paste(my_texts, collapse = ' ')
  my_texts <- substr(my_texts, 1, (3500 * 4))
  
  # Get the topic of the cluster
  print('Get cluster topic')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_description <- ask_gpt(prompt_cluster_description(topic = MAIN_TOPIC, 
                                                                topic_description = MAIN_TOPIC_DESCRIPTION,
                                                                cluster_text = my_texts),
                                     model='gpt-4o', #'gpt-3.5-turbo-0125',#'gpt-4',
                                     temperature = 0.2)
      cluster_description <- cluster_description$choices[[1]]$message$content
      cluster_completed <- TRUE
      cluster_description
    }, 
    error = function(err){
      message(glue('Error getting topic description of cluster {cluster}. Trying again'))
      message(err)
    })
  }
  rcs_merged$description[which(rcs_merged$cluster_code == cluster)] <- cluster_description

  # Get the name of the cluster
  print('Get cluster name')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_name <- ask_gpt(prompt_cluster_name(topic = MAIN_TOPIC, 
                                                  topic_description = MAIN_TOPIC_DESCRIPTION,
                                                  cluster_description = cluster_description), 
                              model='gpt-4', #'gpt-3.5-turbo-0125',#'gpt-4',
                              max_tokens = 60,
                              temperature = 0.3)
      cluster_name <- cluster_name$choices[[1]]$message$content
      cluster_completed <- TRUE
      cluster_name
    }, 
    error = function(err){
      message(glue('Error getting topic name of cluster {cluster}. Trying again'))
      message(err)
    })
  }
  rcs_merged$name[which(rcs_merged$cluster_code == cluster)] <- cluster_name
}

# We do this to keep copy of the edits in case we mess it.
rcs_merged$name2 <- gsub('^.*?"','',rcs_merged$name) %>% gsub('".$','', .) %>% gsub('"','', .)
rcs_merged$cluster_name <- rcs_merged$name2
rcs_merged$cluster_name[rcs_merged$cluster_code == 99] <- 'Others'

rcs_merged$detailed_description <- rcs_merged$description 

for (cluster in list_of_clusters) {
  print('=================================================================')
  print(glue('cluster: {cluster}'))
  
  # Get the topic of the cluster
  print('Get enhanced description')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_description <- ask_gpt(prompt_cluster_description_enhanced(cluster_description = rcs_merged$detailed_description[rcs_merged$cluster == cluster]),
                                     model='gpt-4o', #'gpt-3.5-turbo-0125',#'gpt-4',
                                     temperature = 1)
      cluster_description <- cluster_description$choices[[1]]$message$content
      cluster_completed <- TRUE
    }, 
    error = function(err){
      message(glue('Error getting topic enhanced description of cluster {cluster}. Trying again'))
      message(err)
    })
  }
  rcs_merged$description[which(rcs_merged$cluster_code == cluster)] <- cluster_description
}

colnames(rcs_merged)
test <- rcs_merged[,c("cluster","cluster_name", "description")]
write.csv(test, file='Q293_llm_names.csv')
getwd()
###################################
###################################
# Cluster figure caption
###################################
figure_caption <- ask_gpt(prompt_figure_caption(MAIN_TOPIC))
figure_caption$choices[[1]]$message$content
