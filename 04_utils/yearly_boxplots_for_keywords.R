# 20221126 

# Creates a figure of the 20 most recent keywords based on the publication years of the 
# documents they appear in. As sorted boxplots. 

# For this file to work we need a `dataset` object. Either directly from WOS of from a clustering analysis or from fukan system
# And a "map" file from a keyword cooccurrence network from VOSviewer
# --- Actually no. we just need a list of keywords we want to analyse. 


###############################################
# Year boxplots Plots:
###############################################
# Boxplots sorted from the most recent to oldest
# Based on medians; ties are broken with the mean

library("reshape2")
library("ggplot2")

keywords_vos_map$label

# Get years per keyword
text_vector <- paste(dataset$TI, dataset$AB, dataset$DE, dataset$ID, sep = ". ") %>% tolower()
keywords_years_df <- lapply(keywords_vos_map$label, function(x) {
  tmp <- dataset[grepl(x,text_vector), c("UT", "PY")]
  tmp$X_C <- x
  return(tmp)
}) %>% rbind.fill()
keywords_years_df$PY <- as.numeric(keywords_years_df$PY)
keywords_years_df$UT <- NULL

# Break ties 
PY_compound_mean <- tapply(keywords_years_df$PY, keywords_years_df$X_C, mean, na.rm=TRUE)
PY_compound_median <- tapply(keywords_years_df$PY, keywords_years_df$X_C, median, na.rm = TRUE)

# Plots
PY_long <- keywords_years_df[,c("X_C", "PY")]
PY_long$X_C <- as.character(PY_long$X_C)
PY_long$X_C <-factor(PY_long$X_C, levels = keywords_vos_map$label[order(PY_compound_median, PY_compound_mean)])

# Filter. We plot only 20 most recent
keywords_recent <- keywords_vos_map$label[order(PY_compound_median, PY_compound_mean)]
keywords_recent <- keywords_recent[(nrow(keywords_vos_map) - 19):nrow(keywords_vos_map)]
PY_long <- PY_long[PY_long$X_C %in% keywords_recent,]


bp <- ggplot(PY_long, aes(x = X_C, y = PY)) + geom_boxplot(fill = "deepskyblue3") + xlab("Keywords") + ylab("Ave. Year")
bp + coord_flip()
K <- length(unique(keywords_years_df$X_C))
if (K > 20) {
  bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1))
}
bp

ggsave(file.path(output_folder_reports, "keywords_PY_boxplot.jpg"))
getwd()
