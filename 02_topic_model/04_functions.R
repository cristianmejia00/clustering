# 20160117
# There is no output in this code. But is necesary for the others.

# this is a extension code for the file topic_models.R
# this should be used only when topic_models_1.R was run correctly and completedly.
# And still open in the console.

# Basically this code is the main structure of the visualization tool
# from there we are going to extract the words from the topics.

# this codes comes from the authors of the LDAvis package
# https://github.com/cpsievert/LDAvis/blob/master/R/createJSON.R
# https://github.com/cpsievert/LDAvis/blob/master/inst/htmljs/ldavis.js

# Input params:
R = 100 #The number of words per topic
lambda.step = 0.01 #Dont change
mds.method = jsPCA 
# cluster
plot.opts = list(xlab = "PC1", ylab = "PC2") 
reorder.topics = TRUE

# Set the values of a few summary statistics of the corpus and model:
dp <- dim(phi)  # should be K x W
dt <- dim(theta)  # should be D x K

N <- sum(doc.length)  # number of tokens in the data
W <- length(vocab)  # number of terms in the vocab
D <- length(doc.length)  # number of documents in the data
K <- dt[2]  # number of topics in the model

# check that certain input dimensions match
if (dp[1] != K) stop("Number of rows of phi does not match 
                     number of columns of theta; both should be equal to the number of topics 
                     in the model.")  
if (D != dt[1]) stop("Length of doc.length not equal 
                     to the number of rows in theta; both should be equal to the number of 
                     documents in the data.")
if (dp[2] != W) stop("Number of terms in vocabulary does 
                     not match the number of columns of phi (where each row of phi is a
                     probability distribution of terms for a given topic).")
if (length(term.frequency) != W) stop("Length of term.frequency 
                                      not equal to the number of terms in the vocabulary.")
if (any(nchar(vocab) == 0)) stop("One or more terms in the vocabulary
                                 has zero characters -- all terms must have at least one character.")

# check that conditional distributions are normalized:
phi.test <- all.equal(rowSums(phi), rep(1, K), check.attributes = FALSE)
theta.test <- all.equal(rowSums(theta), rep(1, dt[1]), 
                        check.attributes = FALSE)
if (!isTRUE(phi.test)) stop("Rows of phi don't all sum to 1.")
if (!isTRUE(theta.test)) stop("Rows of theta don't all sum to 1.")

# compute counts of tokens across K topics (length-K vector):
# (this determines the areas of the default topic circles when no term is 
# highlighted)
topic.frequency <- colSums(theta * doc.length)
topic.proportion <- topic.frequency/sum(topic.frequency)

# re-order the K topics in order of decreasing proportion:
if(reorder.topics) {
  o <- order(topic.proportion, decreasing = TRUE)
} else {
  o <- seq_along(topic.proportion)
}

phi <- phi[o, ]
theta <- theta[, o]
topic.frequency <- topic.frequency[o]
topic.proportion <- topic.proportion[o]

# compute intertopic distances using the specified multidimensional
# scaling method:
mds.res <- mds.method(phi)
if (is.matrix(mds.res)) {
  colnames(mds.res) <- c("x", "y")
} else if (is.data.frame(mds.res)) {
  names(mds.res) <- c("x", "y")
} else {
  warning("Result of mds.method should be a matrix or data.frame.")
}  
mds.df <- data.frame(mds.res, topics = seq_len(K), Freq = topic.proportion*100, 
                     cluster = 1, stringsAsFactors = FALSE)
# note: cluster (should?) be deprecated soon.

# token counts for each term-topic combination (widths of red bars)
term.topic.frequency <- phi * topic.frequency  

# compute term frequencies as column sums of term.topic.frequency
# we actually won't use the user-supplied term.frequency vector.
# the term frequencies won't match the user-supplied frequencies exactly
# this is a work-around to solve the bug described in Issue #32 on github:
# https://github.com/cpsievert/LDAvis/issues/32
term.frequency <- colSums(term.topic.frequency)
stopifnot(all(term.frequency > 0))

# marginal distribution over terms (width of blue bars)
term.proportion <- term.frequency/sum(term.frequency)

# Old code to adjust term frequencies. Deprecated for now
# adjust to match term frequencies exactly (get rid of rounding error)
#err <- as.numeric(term.frequency/colSums(term.topic.frequency))
# http://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector
#term.topic.frequency <- sweep(term.topic.frequency, MARGIN=2, err, `*`)

# Most operations on phi after this point are across topics
# R has better facilities for column-wise operations
phi <- t(phi)

# compute the distinctiveness and saliency of the terms:
# this determines the R terms that are displayed when no topic is selected
topic.given.term <- phi/rowSums(phi)  # (W x K)
kernel <- topic.given.term * log(sweep(topic.given.term, MARGIN=2, 
                                       topic.proportion, `/`))
distinctiveness <- rowSums(kernel)
saliency <- term.proportion * distinctiveness

# Order the terms for the "default" view by decreasing saliency:
default.terms <- vocab[order(saliency, decreasing = TRUE)][1:R]
counts <- as.integer(term.frequency[match(default.terms, vocab)])
Rs <- rev(seq_len(R))
default <- data.frame(Term = default.terms, logprob = Rs, loglift = Rs, 
                      Freq = counts, Total = counts, Category = "Default", 
                      stringsAsFactors = FALSE)
topic_seq <- rep(seq_len(K), each = R)
category <- paste0("Topic", topic_seq)
lift <- phi/term.proportion

# Collect R most relevant terms for each topic/lambda combination
# Note that relevance is re-computed in the browser, so we only need
# to send each possible term/topic combination to the browser
find_relevance <- function(i) {
  relevance <- i*log(phi) + (1 - i)*log(lift)
  idx <- apply(relevance, 2, 
               function(x) order(x, decreasing = TRUE)[seq_len(R)])
  # for matrices, we pick out elements by their row/column index
  indices <- cbind(c(idx), topic_seq)
  data.frame(Term = vocab[idx], Category = category,
             logprob = round(log(phi[indices]), 4),
             loglift = round(log(lift[indices]), 4),
             relev = round(i*log(phi[indices]) + (1 - i)*log(lift[indices]), 4), #I added this line
             stringsAsFactors = FALSE)
}

