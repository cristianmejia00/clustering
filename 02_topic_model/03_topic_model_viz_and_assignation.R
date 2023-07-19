# 20160315
# Topic Model 3/3
# The topic model.

#######################################################
# Run the topic model
#######################################################
print("code: 03_topic_model_viz_and_assignation.R")
print("compute lda")
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(
  documents = documents,
  K = tmo$K,
  vocab = vocab,
  num.iterations = tmo$G,
  alpha = tmo$alpha,
  eta = tmo$eta,
  initial = NULL,
  burnin = 0,
  compute.log.likelihood = TRUE
)
t2 <- Sys.time()
t2 - t1

# For 11515 vocab, +1500 docs, 49topics, G=5000 iter, takes 2.5 hours
# The visualizer automatically reorder topics from the largest
# To stay with the output of LDA we can add reorder.topics = FALSE in the parameters
######################################################
theta <- t(apply(fit$document_sums + tmo$alpha, 2, function(x) x / sum(x))) # DxK, document topic matrix
phi <- t(apply(t(fit$topics) + tmo$eta, 2, function(x) x / sum(x))) # KxW, topic word matrix


#####################################################
# Topic Assignation
print("topic assignation")
# find the TOPIC with the largest probability of each article
topic_assignation <- sapply(1:nrow(theta), function(x) {
  which.max(theta[x, ])
})

# Reassign so the topics are sorted from the largest.
topic_reorder_vector <- c(1:tmo$K)
names(topic_reorder_vector) <- table(topic_assignation) %>%
  order(decreasing = TRUE) %>%
  as.character()
topic_assignation_ordered <- topic_reorder_vector[as.character(topic_assignation)]
topic_assignation_ordered %>% table()


#####################################################
# Reorder the tables
theta <- theta[, table(topic_assignation) %>% order(decreasing = TRUE)]
phi <- phi[table(topic_assignation) %>% order(decreasing = TRUE), ]

# Write the topic model table outputs
if (tmo$fullReports) {
  write.csv(phi, file = PROJECTkxw, row.names = TRUE)
  write.csv(theta, file = PROJECTdxk, row.names = TRUE)
}

#####################################################
# Visualization trough LDAVIS
# create the JSON object to feed the visualization:
model_parameters <- list(
  phi = phi,
  theta = theta,
  doc.length = doc.length,
  vocab = vocab,
  term.frequency = term.frequency
)

json <- createJSON(
  phi = model_parameters$phi,
  theta = model_parameters$theta,
  doc.length = model_parameters$doc.length,
  vocab = model_parameters$vocab,
  term.frequency = model_parameters$term.frequency
)

PROJECTviz <- paste("C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics\\", analysis_metadata$query_id, "\\tm_viz", sep = "")
serVis(json, out.dir = PROJECTviz, open.browser = FALSE) # TRUE to open in the console
#####################################################

# find the SCORE of the most probable topic
value_assignation <- sapply(1:nrow(theta), function(x) {
  max(theta[x, ])
})

# find the topics with the highest score in each news
# to do so we find the outliers of each news
related_topics <- sapply(1:nrow(theta), function(x) {
  topic_probs <- theta[x, ]
  names(topic_probs) <- c(1:ncol(theta))
  sorted_topics <- sort(topic_probs, decreasing = TRUE)
  bp <- boxplot(sorted_topics, plot = FALSE)
  largest_topics <- bp$out
  largest_topics <- paste(names(largest_topics), collapse = "; ")
  return(largest_topics)
}) %>% unlist()


# Add the topics
myDataCorrect$X_C <- topic_assignation_ordered
myDataCorrect$cluster_code <- topic_assignation_ordered
myDataCorrect$X_E <- value_assignation
myDataCorrect$related_topics <- related_topics
