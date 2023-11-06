#20170311

# This code adds 2 columns to the mission.facet.all extracted from the Fukan System
# One column correspond to the Community of each paper according to other algorithm
# Mainly Louvain.
# The other column is the degree of the paper in the network (This is the "same" of _D, and is added for verification purposes,
# This is, that column "degree" and "_D" must be the same for each paper. However, small variance may be 
# observed due the algorithm way of counting. A variation of +- 2 is normal)

# Input files:
# Mission.facet.all 
# mission.pairs

# Output:
# The same mission.facet.all with louvain column (community), and degree column

#####################################################################################
if(settings$cno$algor == "louvain") {com <- multilevel.community(g1)}
if(settings$cno$algor == "infomap") {com <- cluster_infomap(g1)}

m_com = membership(com)
id_com = sort(unique(m_com))
ordered = as.numeric(names(sort(table(m_com), decreasing = TRUE)))
repl <- sapply(m_com, function(x) {which(ordered == x)})
names(repl) <- names(m_com)
m_com <- repl

#Verify the clusters are ordered from the largest
plot(table(m_com))

#plot(table(dataset$"_C")) #Compare with original Newman Solution
table(m_com)

#Get degrees of each vertex
degrees <- degree(g1, V(g1))

#Order vector of communities and vector of degrees as they appear in the dataset
vertex <- as.numeric(names(V(g1)))
ids <- dataset$"X_N"
communities <-m_com[order(match(vertex, ids))]
degrees <- degrees[order(match(vertex, ids))]

#Add the to the dataset
dataset$community <- communities
dataset$degrees <- degrees

# Change names
dataset$fukan_c <- dataset$X_C
dataset$X_C <- dataset$community

table(dataset$community)
table(dataset$X_C)

# Get orphans
dataset_louvain <- dataset # backup
dataset_wos <- dataset # reload from saved object
orphans <- dataset_wos[!(dataset_wos$UT %in% dataset_louvain$UT),]
dataset <- dataset_louvain

# Let's check the orphans top titles
orphans$TI[order(orphans$Z9, decreasing = TRUE)][1:10]


# Let's check the clusters' top titles
for (i in c(1:max(dataset$X_C))) {
  print('===================================================')
  print(as.character(i))
  titles <- dataset %>% filter(X_C == i) %>% dplyr::arrange(desc(X_D)) %>% top_n(10, X_D)
  print(titles$TI)
}


