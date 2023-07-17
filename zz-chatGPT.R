# 20230715

# Using OpenAI in R.

# Globals

# The topic used to infer the query
MAIN_TOPIC <- 'Future Scenarios'

# It means that you know about ...
MAIN_TOPIC_DESCRIPTION <- 'technological forecasting and social change, foresight, and roadmapping in the context of management and innovation.'



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
ask_gpt <- function(prompt, temperature = 0.1, max_tokens = 500, n = 1) {
  response <- openai$ChatCompletion$create(
    model = 'gpt-3.5-turbo-0613', #'gpt-4-0613'
    messages = prompt,
    temperature = temperature,
    max_tokens = as.integer(max_tokens),
    n = as.integer(n)
  )
}

#' Function to get a subset of the cluster containing the combination of
#' top 5 most linked (X_E), most cited (Z9), and Most linked of the most recent
#' param dataset: the dataset
#' param cluster: the cluster number to subset. Compatible with X_C, meaning sypport for cluster 99.
#' return a data frame. The largest possible is of size 15 when all 3 conditions are different
get_cluster_data <- function(dataset, cluster, top = 5) {
  cluster_data <- subset(dataset, X_C == cluster, select = c('X_C','TI','AB','AU','PY','UT','Z9','X_E'))
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


#' AskGPT to summarize each article in the given dataset. Each summary is appended to column `summary`
#' param dataset: the dataset
#' returns the same dataset with column summary appended.
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
                                         article_text = cl_dataset$text[idx]))
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

# ttt <- get_cluster_data(dataset, 20)
# ttt <- get_papers_summary(ttt)
###################################
###################################
# Article summary
###################################
# The most cited article in the dataset

# The oldest article in the dataset

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
list_of_clusters <- list_of_clusters[c(6:length(list_of_clusters))]



for (cluster in list_of_clusters) {
  # Get this cluster tops
  print(glue('cluster: {cluster}'))
  cluster_data <- get_cluster_data(dataset, cluster = cluster, top = 5)
  # Summarize each of the selected papers
  cluster_data <- get_papers_summary(cluster_data)
  # Assign the summaries to the main dataset
  print('asign summaries to main dataset')
  dataset$summary[match(cluster_data$UT, dataset$UT)] <- cluster_data$summary
  
  # Generate the bulk text
  print('get bulk text')
  print(nrow(cluster_data))
  my_texts <- list()
  for (i in c(1:min(10,nrow(cluster_data)))) {
    my_texts[i] <- glue('##### {cluster_data$text[[i]]}')
  }
  print(length(my_texts))
  my_texts <- paste(my_texts, collapse = ' ')
  
  # Get the topic of the cluster
  print('Get cluster topic')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_description <- ask_gpt(prompt_cluster_description(topic = MAIN_TOPIC, 
                                                                topic_description = MAIN_TOPIC_DESCRIPTION,
                                                                cluster_text = my_texts))
      cluster_description <- cluster_description$choices[[1]]$message$content
      cluster_completed <- TRUE
      cluster_description
    }, 
    error = function(err){
      message(glue('Error getting topic description of cluster {i}. Trying again'))
      message(err)
    })
  }
  rcs_merged$description[which(rcs_merged$cluster == cluster)] <- cluster_description

  # Get the name of the cluster
  print('Get cluster name')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_name <- ask_gpt(prompt_cluster_name(topic = MAIN_TOPIC, 
                                                  topic_description = MAIN_TOPIC_DESCRIPTION,
                                                  cluster_description = cluster_description), 
                              max_tokens = 50)
      cluster_name <- cluster_name$choices[[1]]$message$content
      cluster_completed <- TRUE
      cluster_name
    }, 
    error = function(err){
      message(glue('Error getting topic description of cluster {i}. Trying again'))
      message(err)
    })
  }
  rcs_merged$name[which(rcs_merged$cluster == cluster)] <- cluster_name
}


###################################
###################################
# Cluster figure caption
###################################
figure_caption <- ask_gpt(prompt_figure_caption(MAIN_TOPIC))
figure_caption$choices[[1]]$message$content
