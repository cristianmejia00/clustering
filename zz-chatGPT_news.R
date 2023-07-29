# 20230715

# Using OpenAI in R.

# Globals

# The topic used to infer the query
MAIN_TOPIC <- settings$analysis_metadata$project_name

# It means that you know about ...
MAIN_TOPIC_DESCRIPTION <- 'the palm oil industry. Meaning you know about the palm oil supply chain, and what are current efforts to achieve sustainable palm oil'
#'technological forecasting and social change, foresight, and roadmapping in the context of management and innovation.'
#'Food waste and food loss. This is food that is not eaten. Food loss and waste occurs at all stages of the food supply chain – production, processing, sales, and consumption.'


# Libraries
library(reticulate)
library(glue)

# One time operation to generate a python env
#reticulate::conda_create(envname = 'openai_env', packages = 'openai', python_version = '3.9')

# Activate enviroment
reticulate::use_condaenv('openai_env')

# import Openai Python library
openai <- reticulate::import('openai')


# Attach key.
# In VSCode create a file `openai.key`
# Is only one line with the OpenAi key.
# `credentials/openai.key` was added to .gitignore so is not comitted to the repo.
openai$api_key <- readr::read_file('05_assets/credentials/openai.key')


# utils
#' @description
#' Get answers from OpenAI's GPT. Here used for summarization.
#' @param prompt LIST. A prompt in the format of OpenAI. See the code `zz-prompts.R` for details.
#' @param model STRING {gpt-3.5-turbo-0613} the OpenAI Moodel to use. Options: gpt-3.5-turbo-0613, gpt-4
#' @param temperature NUMBER. Between 0 and 2. 0 means less randomness and 2 more creative.
#' @param max_tokens INTEGER. The approx MAX size possible for the reply from ChatGPT.
#' @param n INTEGER. Number of reply variations to get.
#' @returns The JSON reply from OpenAI in R's LIST form. The actual reply text is located at `x$choices[[1]]$message$content` 
ask_gpt <- function(prompt, model = 'gpt-3.5-turbo-0613', temperature = 0.1, max_tokens = 500, n = 1) {
  response <- openai$ChatCompletion$create(
    model = model, #'gpt-4-0613'
    messages = prompt,
    temperature = temperature,
    max_tokens = as.integer(max_tokens),
    n = as.integer(n)
  )
}

#' @description
#' Function to get a subset of the cluster containing the combination of
#' top 5 most linked (X_E), most cited (Z9), and Most linked of the most recent
#' @param dataset DATAFRAME. the dataset
#' @param cluster INTEGER. the cluster number to subset. Compatible with X_C, meaning sypport for cluster 99.
#' @returns DATAFRAME. The largest possible is of `top * 3` when all 3 conditions are different
get_cluster_data <- function(dataset, cluster, top = 5) {
  cluster_data <- dataset[dataset$X_C == cluster, c('X_C','TI','UT','new_score','LP_clean2','TD','sentiment','sentiment_factor')]
  if (nrow(cluster_data) > top) {
    selected_papers <- c(
      # Highest Score
      cluster_data$UT[order(cluster_data$new_score, decreasing = TRUE)][1:top],
      # Most positive
      cluster_data$UT[order(cluster_data$sentiment, decreasing = TRUE)][1:top],
      # Most negative
      cluster_data$UT[order(cluster_data$sentiment, decreasing = FALSE)][1:top]
    ) %>% unique()
    # Only retain selected papers
    cluster_data <- cluster_data[cluster_data$UT %in% selected_papers,]
  }
  cluster_data$text <- paste(cluster_data$TI, cluster_data$LP_clean2, cluster_data$TD, sep = ' ')
  return(cluster_data)
}

#' @description
#' AskGPT to summarize each article in the given dataset. Each summary is appended to column `summary`
#' @param dataset DATAFRAME. the dataset
#' @returns DATAFRAME. the same dataset with the column summary appended.
get_papers_summary <- function(cl_dataset) {
  cl_dataset$summary <- ''
  starting <- 1
  ending <- nrow(cl_dataset)
  while(starting < ending) {
    for(idx in c(starting:ending)) {
      print(paste(cl_dataset$X_C[idx], as.character(idx), cl_dataset$TI[idx], sep = "; "))
      article_summary <- tryCatch({
        article_summary <- ask_gpt(prompt_summarize_a_paper(topic = MAIN_TOPIC,
                                                            topic_description = MAIN_TOPIC_DESCRIPTION,
                                                            article_text = cl_dataset$text[idx]),
                                   temperature = 0.7)
        cl_dataset$summary[idx] <- article_summary$choices[[1]]$message$content
        article_summary
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
rcs_merged$description <- ''
rcs_merged$name <- ''
rcs_merged$pos_description <- ''
rcs_merged$neg_description <- ''
dataset$summary <- ''

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
list_of_clusters <- dataset$X_C %>% unique() %>% sort()
list_of_clusters <- list_of_clusters[8:15]
# list_of_clusters <- list_of_clusters[c(13:length(list_of_clusters))]
# list_of_clusters <- list(5)

for (cluster in list_of_clusters) {
  # Get this cluster tops
  print(glue('cluster: {cluster}'))
  cluster_data <- get_cluster_data(dataset, cluster = cluster, top = 5)
  # Order from highest score
  cluster_data <- cluster_data[order(cluster_data$new_score, decreasing = TRUE),]
  # Summarize each of the selected papers
  cluster_data <- get_papers_summary(cluster_data)
  # Assign the summaries to the main dataset
  print('asign summaries to main dataset')
  dataset$summary[match(cluster_data$UT, dataset$UT)] <- cluster_data$summary
  
  ################################################################### 1: DESCRIPTION
  # Generate the bulk text
  print('get bulk text')
  print(nrow(cluster_data))

  my_texts <- list()
  for (i in c(1:min(15,nrow(cluster_data)))) {
    my_texts[i] <- glue('##### {cluster_data$text[[i]]}')
  }
  print(length(my_texts))
  my_texts <- paste(my_texts, collapse = ' ')
  my_texts <- substr(my_texts, 1, (7000 * 4))
  

  # Get the topic of the cluster
  print('Get cluster topic')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_description <- ask_gpt(prompt_cluster_description(topic = MAIN_TOPIC, 
                                                                topic_description = MAIN_TOPIC_DESCRIPTION,
                                                                cluster_text = my_texts),
                                     model = 'gpt-4',
                                     temperature = 0.7)
      cluster_description <- cluster_description$choices[[1]]$message$content
      cluster_completed <- TRUE
      cluster_description
    }, 
    error = function(err){
      message(glue('Error getting topic description of cluster {i}. Trying again'))
      message(err)
    })
  }
  rcs_merged$description[which(rcs_merged$cluster_code == cluster)] <- cluster_description
  
  
  ################################################################### 2: NAME
  # Get the name of the cluster
  print('Get cluster name')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_name <- ask_gpt(prompt_cluster_name(topic = MAIN_TOPIC, 
                                                  topic_description = MAIN_TOPIC_DESCRIPTION,
                                                  cluster_description = cluster_description), 
                              model = 'gpt-4',
                              max_tokens = 50,
                              temperature = 0.4)
      cluster_name <- cluster_name$choices[[1]]$message$content
      cluster_completed <- TRUE
      cluster_name
    }, 
    error = function(err){
      message(glue('Error getting topic description of cluster {i}. Trying again'))
      message(err)
    })
  }
  rcs_merged$name[which(rcs_merged$cluster_code == cluster)] <- cluster_name
  
  ################################################################### 3: POSITIVE DESCRIPTION
  # Generate the bulk text
  print('get bulk text of positive news')
  cluster_data_backup <- cluster_data
  cluster_data <- cluster_data_backup[order(cluster_data_backup$sentiment, decreasing = TRUE),][1:5,]
  my_texts <- list()
  for (i in c(1:min(5,nrow(cluster_data)))) {
    my_texts[i] <- glue('##### {cluster_data$text[[i]]}')
  }
  print(length(my_texts))
  my_texts <- paste(my_texts, collapse = ' ')
  my_texts <- substr(my_texts, 1, (7000 * 4))
  
  
  # Get the topic of the cluster
  print('Get positive cluster topic')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_description <- ask_gpt(prompt_cluster_sentiment_description(topic = MAIN_TOPIC, 
                                                                          topic_description = MAIN_TOPIC_DESCRIPTION,
                                                                          sentiment = 'positive',
                                                                          cluster_text = my_texts),
                                     model = 'gpt-4',
                                     temperature = 0.7)
      cluster_description <- cluster_description$choices[[1]]$message$content
      cluster_completed <- TRUE
      cluster_description
    }, 
    error = function(err){
      message(glue('Error getting topic description of cluster {i}. Trying again'))
      message(err)
    })
  }
  rcs_merged$pos_description[which(rcs_merged$cluster_code == cluster)] <- cluster_description
  
  
  ################################################################### 4: NEGATIVE DESCRIPTION
  # Generate the bulk text
  print('get bulk text of negative news')
  cluster_data <- cluster_data_backup[order(cluster_data_backup$sentiment, decreasing = FALSE),][1:5,]
  my_texts <- list()
  for (i in c(1:min(15,nrow(cluster_data)))) {
    my_texts[i] <- glue('##### {cluster_data$text[[i]]}')
  }
  print(length(my_texts))
  my_texts <- paste(my_texts, collapse = ' ')
  my_texts <- substr(my_texts, 1, (7000 * 4))
  
  
  # Get the topic of the cluster
  print('Get negative cluster topic')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_description <- ask_gpt(prompt_cluster_sentiment_description(topic = MAIN_TOPIC, 
                                                                          topic_description = MAIN_TOPIC_DESCRIPTION,
                                                                          sentiment = 'negative',
                                                                          cluster_text = my_texts),
                                     model = 'gpt-4',
                                     temperature = 0.7)
      cluster_description <- cluster_description$choices[[1]]$message$content
      cluster_completed <- TRUE
      cluster_description
    }, 
    error = function(err){
      message(glue('Error getting topic description of cluster {i}. Trying again'))
      message(err)
    })
  }
  rcs_merged$neg_description[which(rcs_merged$cluster_code == cluster)] <- cluster_description
}



###################################
# Cluster description and name when the process was interrupted above. 
###################################
# Copy the inners of the for loop above.

#cluster <- 1
# --- Copy Here ---

###################################
# Assign cluster names
###################################
# We do this to keep copy of the edits in case we mess it.
rcs_merged$name2 <- gsub('^.*?"','',rcs_merged$name) %>% gsub('".$','', .) %>% gsub('"','', .)
rcs_merged$cluster_name <- rcs_merged$name2
rcs_merged$cluster_name[rcs_merged$cluster_code == 99] <- 'Others'

###################################
###################################
# Cluster figure caption
###################################
figure_caption <- ask_gpt(prompt_figure_caption(MAIN_TOPIC))
figure_caption$choices[[1]]$message$content
