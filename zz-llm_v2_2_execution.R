# 20230715

# Using OpenAI and Claude in R.
source("zz-llm_v2_0_prompts.R")
source("zz-llm_v2_1_functions.R")

###################################
###################################
# Initialize
###################################
#rcs_merged <- rcs
rcs_merged$description <- ''
rcs_merged$name <- ''
dataset$summary <- ''

###################################
###################################
# Article summary
###################################
# The oldest article(s) in the dataset.
# When there are many "old papers" we analyze only the two most cited.
oldest_year <- min(dataset$PY, na.rm = TRUE)
oldest_data <- subset(dataset, PY <= oldest_year)#subset(dataset, PY == oldest_year)
if (nrow(oldest_data) > 2) {
  oldest_data <- oldest_data[order(oldest_data$Z9, decreasing = FALSE)[c(1:2)],]
}
oldest_data$summary <- ''
for (i in nrow(oldest_data)) {
  old_UT <- oldest_data$UT[i]
  prompt_old <- prompt_summarize_a_paper(topic = MAIN_TOPIC,
                                         topic_description = MAIN_TOPIC_DESCRIPTION,
                                         article_text = paste(oldest_data$TI[i], oldest_data$AB[i], sep = ' '))
  old_summary <- ask_gpt(system_prompt = prompt_old$system,
                         user_prompt = prompt_old$user)
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
dataset$X_C_backup <- dataset$X_C
dataset$X_C <- dataset$subcluster_label1
list_of_clusters <- dataset$X_C %>% unique() %>% sort()


# Compute summaries
COMPUTE_SUMMARIES = TRUE

for (cluster in list_of_clusters) {
  if (COMPUTE_SUMMARIES) {
    # Get this cluster tops
    print('=================================================================')
    print(glue('cluster: {cluster}'))
    cluster_data <- get_cluster_data(dataset, cluster = cluster, top = 3)
    # Summarize each of the selected papers
    cluster_data <- get_papers_summary(cluster_data)
    # Assign the summaries to the main dataset
    print('asign summaries to main dataset')
    dataset$summary[match(cluster_data$UT, dataset$UT)] <- cluster_data$summary
  } else {
    cluster_data <- dataset %>% filter(X_C == cluster, summary != '')
    cluster_data$text <- paste(cluster_data$TI, cluster_data$AB, sep = ' ')
  }

  
  # Generate the bulk text
  print('get bulk text')
  print(glue('Total selected papers for this cluster: {nrow(cluster_data)}'))
  my_texts <- list()
  for (i in c(1:min(10,nrow(cluster_data)))) {
    my_texts[i] <- glue('##### {cluster_data$text[[i]]}')
  }
  my_texts <- paste(my_texts, collapse = ' ')
  my_texts <- substr(my_texts, 1, (3500 * 4))
  
  # Get the topic of the cluster
  print('Get cluster topic')
  prompt_desc <- prompt_cluster_description(topic = MAIN_TOPIC, 
                                       topic_description = MAIN_TOPIC_DESCRIPTION,
                                       cluster_text = my_texts)
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      cluster_description <- ask_claude(system_prompt = prompt_desc$system,
                                        user_prompt = prompt_desc$user,
                                        temperature = 0.2)
      cluster_completed <- TRUE
      print(cluster_description)
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
      prompt <- prompt_cluster_name(topic = MAIN_TOPIC, 
                                    topic_description = MAIN_TOPIC_DESCRIPTION,
                                    cluster_description = cluster_description)
      cluster_name <- ask_claude(system_prompt = prompt$system, 
                                 user_prompt = prompt$user,
                                 max_tokens = 60,
                                 temperature = 0.3)
      cluster_completed <- TRUE
      print(cluster_name)
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
#rcs_merged$cluster_name[rcs_merged$cluster_code == 99] <- 'Others'

rcs_merged$detailed_description <- rcs_merged$description 

for (cluster in list_of_clusters) {
  print('=================================================================')
  print(glue('cluster: {cluster}'))
  
  # Get the topic of the cluster
  print('Get enhanced description')
  cluster_completed <- FALSE
  while(!cluster_completed) {
    tmp <- tryCatch({
      prompt_enh <- prompt_cluster_description_enhanced(topic = MAIN_TOPIC,
                                                    cluster_description = rcs_merged$detailed_description[rcs_merged$cluster_code == cluster])
      
      print(prompt_enh$user)
      cluster_description <- ask_claude(system_prompt = prompt_enh$system,
                                        user_prompt = prompt_enh$user,
                                        model = 'claude-3-opus-20240229',
                                        temperature = 0.1)
      print(cluster_description)
      cluster_completed <- TRUE
    }, 
    error = function(err){
      message(glue('Error getting topic enhanced description of cluster {cluster}. Trying again'))
      message(err)
    })
  }
  rcs_merged$description[which(rcs_merged$cluster_code == cluster)] <- cluster_description
}

figure_caption <- "Figure."

test <- rcs_merged[,c("cluster","cluster_code","cluster_name", "description")]
write.csv(test, file='Q293_claude_names.csv')
getwd()

