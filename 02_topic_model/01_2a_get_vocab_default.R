# 20170523 Topic Models
# Topic Model 1/3
# Model and data preparation
# We prepare the text that is going to be used in the model. We also choose key parameters.

###########################################################
###########################################################
# default implementation: Use get the `vocab` based on unigrams in the corpus
# Except for those appearing in less than 5 documents and stopwords

###########################################################
###########################################################

# remove terms that are stopwords or occur fewer than 5 times:
del <- names(term.table) %in% myStopWords | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)
vocab[1:100] # top 100 terms
