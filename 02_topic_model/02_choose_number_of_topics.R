# 20160315
# Topic Model 2/3
# Automatically select the best number of topics
# Calculate best number of topics by the maximum likelihood of the harmonic means
# As it is calculated in Griffiths and Steyvers
# If the number of topics is already decided, jump to Topic Model 3/3.

# This code can be used either after Topic_Model_1_with_tfidf_threshold.R

################################
# Select the range to extract the best amount of topics
min_topics = 20
max_topics = 25
pase = 1
###############################

# harmonic mean function
harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# generate numerous topic models with different numbers of topics
sequ <- seq(min_topics, max_topics, pase)
fitted_many <- lapply(sequ, function(k) lda.collapsed.gibbs.sampler(documents = documents, K = k, vocab = vocab, 
                                                                    num.iterations = G, alpha = alpha, 
                                                                    eta = eta, initial = NULL, burnin = 0,
                                                                    compute.log.likelihood = TRUE))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L$log.likelihoods[1,470:500])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
plot(sequ, hm_many, type = "l")

# compute optimum number of topics
# The following throws the best number of topics
K <- sequ[which.max(hm_many)]
K

########################
#=======================
# NOTES

# Here we use the lda package. (Instead of the original topicmodels package used in stackoverflow)
# http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity/21394092#21394092
# http://epub.wu.ac.at/3558/1/main.pdf
