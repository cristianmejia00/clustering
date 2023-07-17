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
#library(simplemarkdown)
#install.packages('Rtools')
#install.packages('simplemarkdown')
#colnames(rcs_merged)
#write.csv(rcs_merged[,c(1,4,5,7,9,10,17,18,20,24,27,28,37,38)], file = 'rcs_merged.csv', row.names = FALSE)



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


###################################
###################################
# Article summary
###################################


###################################
###################################
# Cluster description and name
###################################
cluster_data <- subset(dataset, X_C == 20, select = c('TI','AB','AU','PY','UT','Z9','X_E'))
if (nrow(cluster_data) > 5) {
  selected_papers <- c(
    # Most connected
    cluster_data$UT[order(cluster_data$X_E, decreasing = TRUE)][1:5],
    # Most cited
    cluster_data$UT[order(cluster_data$Z9, decreasing = TRUE)][1:5],
    # Newest most connected (X_E is preferred over Z9 because most of paper wont have citations)
    cluster_data$UT[order(cluster_data$PY, cluster_data$X_E, decreasing = TRUE)][1:5]
  ) %>% unique()
  # Only reatain selected papers
  cluster_data <- cluster_data[cluster_data$UT %in% selected_papers,]
}
cluster_data$text <- paste(cluster_data$TI, cluster_data$AB, sep = ' ')

my_texts <- list()
for (i in c(1:10)) {
  my_texts[[i]] <- glue('##### {cluster_data$text[[i]]}')
}
my_texts <- paste(my_texts, collapse = ' ')


cluster_description <- ask_gpt(prompt_cluster_description(topic = MAIN_TOPIC, 
                                                          topic_description = MAIN_TOPIC_DESCRIPTION,
                                                          cluster_text = my_texts))
cluster_description <- cluster_description$choices[[1]]$message$content
cluster_description

cluster_name <- ask_gpt(prompt_cluster_name(topic = MAIN_TOPIC,
                                            topic_description = MAIN_TOPIC_DESCRIPTION,
                                            cluster_description = cluster_description), 
                        max_tokens = 50)
cluster_name$choices[[1]]$message$content



###################################
###################################
# Cluster figure caption
###################################
figure_caption <- ask_gpt(prompt_figure_caption(MAIN_TOPIC))
figure_caption$choices[[1]]$message$content
