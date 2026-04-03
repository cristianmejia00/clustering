colnames(myDataCorrect)
colnames(dataset)

myDataCorrect$text <- paste(myDataCorrect$TI, myDataCorrect$AB) %>% tolower()
myDataCorrect$has_manufact <- grepl("manufact", myDataCorrect$text)
myDataCorrect$has_sust_manufact <- grepl("sustainable manufact", myDataCorrect$text)

clr <- table(myDataCorrect$level0[myDataCorrect$has_manufact]) %>% sort()
sclr <- table(myDataCorrect$subcluster_label1[myDataCorrect$has_manufact]) %>% sort()

clr_sust <- table(myDataCorrect$level0[myDataCorrect$has_sust_manufact]) %>% sort()
sclr_sust <- table(myDataCorrect$subcluster_label1[myDataCorrect$has_sust_manufact]) %>% sort()


#barplot(table(myDataCorrect$level0[myDataCorrect$has_manufact]) %>% sort(decreasing = TRUE))
#barplot(table(myDataCorrect$subcluster_label1[myDataCorrect$has_manufact]) %>% sort(decreasing = TRUE))

getwd()
# Convert table to data frame
df <- as.data.frame(sclr_sust)
names(df) <- c("Cluster", "Papers")

ggplot(df, aes(x = Papers, y = Cluster)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Concentration of Sustainable Manufacturing-related research",
       x = "Cluster",
       y = "Papers") +
  theme(axis.text.y = element_text(size = 10))


myDataCorrect %>% filter(has_manufact, subcluster_label1 == "2-5---") %>% slice_max(n=15, order_by= Z9) %>% pull(TI)

?slice_max
