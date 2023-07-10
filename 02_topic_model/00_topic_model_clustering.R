# 20210812 Last updated


####################################################
# Execute
####################################################

# Text Preparation
# source("02_topic_model/01_topic_model.R")
# Use this for a faster and cleaner (but not verified) implementation
source("02_topic_model/01_1_clean_corpus.R")
# source("01_2a_get_vocab_default.R") # OR
source("02_topic_model/01_2b_get_vocab_with_tfidf_threshold.R")
source("02_topic_model/01_3_lda_parameters.R")

# Select number of topics
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# STOP
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Automatically select number of topics K
# K <- 31
# source("02_topic_model/02_choose_number_of_topics.R")
# source("02_topic_model/02_choose_number_of_topics_ldatunning.R")
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Compute topic model, assign topics to news, and create LDAvis visualization
source("02_topic_model/03_topic_model_viz_and_assignation.R")

# Save the image before report computation
save.image(paste(input_folder, "//", analysis_metadata$query_id, "//", analysis_metadata$project_name, "_environ_before_reports.rdata", sep = ""))
