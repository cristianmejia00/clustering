# 20220924

# Evolution network sankey diagram
# https://plotly.com/r/sankey-diagram/

# INPUT:
# The clustering environ or at least:
# - g1 the network
# - myDataCorrect
# - settings$params$seed

# Output
# - A Sankey diagram in the viewer. We saved it manually.

############################################################
## Params for Sankey

# time partitions
time_partitions <- c(2000, 2010, 2015, 2022)

# Use the X_C from fukan system for the last solution
use_fukan <- TRUE

# Trimming
# Retain branches that contain __% data from predecessor
trmm <- 0.2


############################################################
# Set seed
set.seed(settings$params$seed) # or 100

# Add years to network
V(g1)$PY <- myDataCorrect$PY[match(V(g1)$name, as.character(myDataCorrect$X_N))]

#  Sort myDataCorrect to have the same order as g1
myDataCorrect_g1 <- myDataCorrect[match(V(g1)$name, as.character(myDataCorrect$X_N)), ]

# verify order
all(myDataCorrect_g1$X_N == V(g1)$name)

#####################################################################################
# Aux function
getLouvain <- function(a_graph) {
  com <- multilevel.community(a_graph)
  m_com <- membership(com)
  id_com <- sort(unique(m_com))
  ordered <- as.numeric(names(sort(table(m_com), decreasing = TRUE)))
  repl <- sapply(m_com, function(x) {
    which(ordered == x)
  })
  names(repl) <- names(m_com)
  return(repl)
}

#####################################################################################
# Get time-windows data OPTION 1
# Clustering every time window. This Sankey shows the different topic structures over time
sk_data <- lapply(c(1:length(time_partitions)), function(i) {
  if (use_fukan && i == length(time_partitions)) {
    tmp_df <- data.frame(
      "X_N" = myDataCorrect_g1$X_N,
      "X_C" = myDataCorrect_g1$X_C,
      "PY" = myDataCorrect_g1$PY
    )
    # larger_clusters <- sum(cumsum(prop.table(table(tmp_df$X_C))) <= 0.9)
    # tmp_df$X_C[tmp_df$X_C > larger_clusters] <- larger_clusters + 1
    return(tmp_df)
  } else {
    tmp <- induced_subgraph(g1, which(V(g1)$PY < time_partitions[i]))
    tmp_cls <- clusters(tmp)
    giant_id <- which.max(tmp_cls$csize)
    vert_ids <- V(tmp)[tmp_cls$membership == giant_id]
    tmp <- induced_subgraph(tmp, vert_ids)
    V(tmp)$cluster <- getLouvain(tmp)
    tmp_df <- data.frame(
      "X_N" = as.numeric(V(tmp)$name),
      "X_C" = as.numeric(V(tmp)$cluster),
      "PY" = as.numeric(V(tmp)$PY)
    )
    larger_clusters <- sum(cumsum(prop.table(table(tmp_df$X_C))) <= 0.9)
    tmp_df$X_C[tmp_df$X_C > larger_clusters] <- larger_clusters + 1
    return(tmp_df)
  }
})


#####################################################################################
# Get time-windows data OPTION 2
# Use the latest clustering solution in every time window.This Sankey ranks each of the current clusters over time
#
# sk_data <- lapply(c(1:length(time_partitions)), function(i) {
#     myDataCorrect_tmp <- myDataCorrect_g1[myDataCorrect_g1$PY < time_partitions[i],]
#     tmp_df <- data.frame("X_N" = myDataCorrect_tmp$X_N,
#                          "X_C" = myDataCorrect_tmp$X_C,
#                          "PY" = myDataCorrect_tmp$PY)
#     return(tmp_df)
# })

#####################################################################################
# Plotly Sankey Data Preparation

# Labels and indices of boxes
sk_labels <- c()
sk_labels_x <- c()
sk_labels_y <- c()
for (i in c(1:length(sk_data))) {
  labels <- table(sk_data[[i]]$X_C) %>% names()
  labels_ <- paste(as.character(i), labels, sep = "_")
  sk_labels <- c(sk_labels, labels_)
  sk_labels_x <- c(sk_labels_x, rep(i, length(labels)))
  sk_labels_y <- c(sk_labels_y, as.numeric(labels))
}


# Calculates steps (flow from one level to the other)
steps <- lapply(c(2:length(sk_data)), function(i) {
  xx <- i - 1
  yy <- i
  step <- table(sk_data[[xx]]$X_C, sk_data[[yy]]$X_C[sk_data[[yy]]$X_N %in% sk_data[[xx]]$X_N]) %>% reshape2::melt()
  step <- step[step[, 3] != 0, ]
  step[, 1] <- paste(as.character(xx), step[, 1], sep = "_")
  step[, 2] <- paste(as.character(yy), step[, 2], sep = "_")
  step[, 4] <- step[, 1] %>% match(sk_labels)
  step[, 5] <- step[, 2] %>% match(sk_labels)
  return(step)
}) %>%
  rbindlist() %>%
  as.data.frame()
steps <- steps[order(steps[, 5], steps[, 4]), ]


## Special trims
# Remove branches with ten papers or more
# steps[,3][steps[,3] <= 10] <- 0

# Retain branches with the 20% of articles or more
test <- tapply(
  steps[, 3] %>% unlist() %>% unname(),
  steps[, 4] %>% unlist() %>% unname(),
  sum
)
test_value <- sapply(c(1:nrow(steps)), function(x) {
  enum <- steps[[3]][x]
  denom <- test[steps[[4]][x]]
  return(enum / denom)
})
steps[, 3][test_value <= trmm] <- 0 # or 0.2

#####################################################################################
## Plot

# Compute the longest of the timeframes
separator <- steps[steps[[3]] > 0, ]
separator <- unique(c(separator[[1]], separator[[2]]))
separator <- sapply(separator, function(x) {
  strsplit(x, "_")[[1]][1]
}) %>% table()
separator <- max(separator)

# Plot
fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = sk_labels,
    color = rep("blue", length(sk_labels)),
    x = ((sk_labels_x - 0.8) / 4),
    y = (sk_labels_y - 0.8) / separator,
    pad = 20,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = steps[, 4] %>% unlist() %>% unname() - 1,
    target = steps[, 5] %>% unlist() %>% unname() - 1,
    value =  steps[, 3] %>% unlist() %>% unname()
  )
)
fig

fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 10
  )
)
fig

steps[, 1] %>%
  unlist() %>%
  unname()
