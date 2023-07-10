
###############################################
# Year boxplots Plots:
###############################################
# Boxplots sorted from the most recent to oldest
# Based on medians; ties are broken with the mean

library("reshape2")
library("ggplot2")

PY_compound_mean <- tapply(myDataCorrect$PY, myDataCorrect$X_C, mean, na.rm=TRUE)
PY_compound_median <- tapply(myDataCorrect$PY, myDataCorrect$X_C, median, na.rm = TRUE)


# Plots
PY_long <- myDataCorrect[,c("X_C", "PY")]
PY_long$X_C <- as.character(PY_long$X_C)
PY_long$X_C <-factor(PY_long$X_C, levels = as.character(order(PY_compound_median, PY_compound_mean)))

bp <- ggplot(PY_long, aes(x = X_C, y = PY)) + geom_boxplot(width = 0.7, fill = "deepskyblue3") + xlab("Cluster") + ylab("Ave. Year")
#bp + coord_flip()
K <- length(unique(myDataCorrect$X_C))
if (K > 20) {
  bp <- bp + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1))
}
bp

ggsave(file.path(output_folder_level, "clusters_PY_boxplot.jpg"))
