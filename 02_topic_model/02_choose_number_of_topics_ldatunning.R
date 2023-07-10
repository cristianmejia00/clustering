# 20180104
# Estimate number of topics

# This code does not consider the results of Topic_Model_1_with_tfidf_threshold.R, 
# and computes the Maimum Likelihood based on all text.

######################
# Prepare for LDATunning
tidytext <- Corpus(VectorSource(myText)) %>% DocumentTermMatrix

a <- Sys.time()
result <- FindTopicsNumber(
  tidytext,
  topics =  seq(from = 20, to = 200, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009"),#, "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 100),
  mc.cores = 2L,
  verbose = TRUE
)

b <- Sys.time()
c <- b - a
c

# See chart
FindTopicsNumber_plot(result)

# Save the image
png(PROJECTmaxliklihood)
FindTopicsNumber_plot(result)
dev.off()

# Update K
result[which.max(result$Griffiths2004),]
K <- result[which.max(result$Griffiths2004),]$topics
