## Metadata
analysis_metadata <- list(
  input_folder = "C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics",
  theme = 'future scenario',
  project_name = "Q249-future-scenario",
  query_id = "Q249", #This is the Folder name. Equivalent to Fukan's dataset
  project_id = "001", #Equivalent to Fukan's analysis (i.e. the order inside dataset)
  date = "2023-07-13",
  project_description = "Citation network of future scenario research",
  created_by = "cristianmejia00@gmail.com",
  notes = "NA",
  fukan_url = "NA",
  query = "",
  downloaded_documents = ""
)

## General Parameters
params <- list(
  type_of_dataset = "papers", # "papers", "patents" or "news" 
  unit_of_analysis = "cluster", #topic, cluster, facet, firm, country, institution, author, etc.
  type_of_analysis = "citation_network", #"topic_model" or "citation_network" 
  dataset_source = "wos", #wos, derwent, factiva (dimensions = wos)
  seed = 100 # The seed for random initialization. Needed for reproducibility
)

########################################################### for 00_clustering.R
## Citation network options
cno <- list(
  # Shall we use the network file from Fukan System (i.e. mission.pairs.tsv is available) 
  # if False, we create our own network file from scratch.
  using_mission_pairs_from_fukan = TRUE,
  
  # Shall we use the initial clustering solution from Fukan System?
  # If TRUE, we use the column "_C" in mission.facet.all, and hence we can use the figure from Fukan System
  # If FALSE, we compute a new "_C" column based on the algorithm of choice.
  using_initial_column_C_from_fukan = TRUE,
  
  # Either...
  # - Proportion of articles that determines the number of level0 clusters (<1)(e.g. #largest clusters contain 90%, 0.9, of articles )
  # - Number of cluster to consider from the Fukan System solution (1+)
  threshold = 27,
  
  # Cluster scope
  scope = "all", #"all" OR "cl99" OR "cl_99"
  
  # Report for up to this level
  recursive_level = 0, #if (vcount(g1) < 30000) {0} else {1} #0,1,2, OR 3
  
  ### Options for clustering or recursive clustering.
  ### The following options are useful for any of these conditions
  ### - We want recursive clustering
  ### - we want clustering at level0, either because:
  ###   - we don't want to use Fukan System X_C
  ###   - We have a WOS dataset without X_C
  
  #Top max clusters to analyze per iteration
  max_clusters = 100,
  
  # Subcluster only if having this or more
  size_limit = 500, 
  
  # Include cluster having collecting a minimum of __ articles
  size_lower_limit = 0,
  
  # When recursive clustering there is a label "-0" that might be annoying. TRUE to remove it. However, Excel will think they are dates.
  remove_zero = FALSE,
  
  # Algorithm to use
  algor = "louvain" #"louvain", OR "newman" OR "infomap"
)

## Topic Model options
tmo <- list(
  #Select the number of topics
  K = 86, #select "0" zero to automatically detect the topics
  
  # Gibbs sampling parameter
  G = 500, #iterations
  alpha = 0.02,
  eta = 0.02,
  
  # More options
  useStemming = TRUE,
  fullReports = FALSE, #for KXD and WXK
  
  # Select the level of relevance
  # 1 = Words ordered based on simple frequency within the topic
  # 0 = Words ordered based on how unique they are to the topic
  # 0.6 is recommended
  relevance_value = 1
)

# add-ons
addons <- list(
  "include_orphans" = "NO", #NO, 99, 999
  "sentiment_analysis" = FALSE,
  "page_rank" = FALSE,
  "eigen_centrality" = FALSE,
  "closeness_centrality" = FALSE,
  "betweeness_centrality" = FALSE
)



########################################################### for 00_reports.R
## Reporting
rp <- list(
  top_documents = 0, #0 means ALL # Select the number of top documents to show in the article report
  top_items = 20, ##0 means ALL # Select the number of top `documents`field`` to show in the clusters report
  text_columns =  c("TI", "AB"),  # Column(s) with text contents to merge and analyze
  categorical_long_reports = c("AU", "WC", "SO", "Countries", "Institutions", "DE", "sentiment_factor"), #Columns in the dataset for long-form summary. These are also used for RCS.
  categorical_simple_wide_reports = c("PY", "sentiment_factor"), #Columns in the dataset without ';' for matrix type summary
  categorical_multi_wide_reports = c("WC", "Countries", "Institutions"), #Columns in the dataset with ';' for matrix type summary
  numerical_reports = c("PY", "Z9", "sentiment", "score"), #Numeric columns in the dataset for summary (min, max, mean, median, sd)
  column_labels = c('Countries' = 'Countries',
                    'SO' = 'Journals',
                    'Institutions' = 'Institutions',
                    'AU' = 'Authors',
                    'WC' = 'Categories',
                    'DE' = 'Author Keywords',
                    'sentiment_factor' = 'Sentiment',
                    'PY' = 'Publication Years',
                    'Z9' = 'Citations',
                    'score' = 'Score',
                    'sentiment' = 'Sentiment') #Column labels are used to format RCS columns and charts' labels
)

# Activate stopwords
article_StopWords <- c("analysis", "paper", "na", "say", "will", "can", "article", "use", "press", "release",
                       "all", "rights", "reserved", "elsevier", "scopus", "doi", "int", "ieee", "cover", "story",
                       #Stemmed
                       "use", "structur", "result", "method", "system", "effect", "studi", "measur", "model", "show", "high",
                       "observ", "increas", "also", "propos","two", "base", "investig", "properti", "process", "differ", "obtain",
                       "found", "chang")

# patent_StopWords <- c("patent", "claim", "device", "data", "module", "network", "control" , 
#                   "base","method", "methods","terminal", "information", 
#                   "connect", "connects", "connection", "communication", "internet", "things", "thing")
# 
news_Stopwords <- c("said", "country", "year", "according", "people", "work", "say", "says", "said",
                    "need", "one", "number", "well", "part", "end", "report","support", "per", "cent", "percent",
                    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "billion", "million", "thousand",
                    "million","time", "living", "make","including", "however", "reached","provide","expected","day",
                    "set", "important", "come", "many", "made", "way", "take", "total", "want", "com", "now", "like", "able", "get",
                    "order", "continue", "aim", "since", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday","week",
                    "noted", "see", "addition", "put", "present", "month", "received", "taken",
                    "january", "february", "march", "april", "may", "june", "july", "august", "september", "november", "october", "december",
                    "timescontent", "especially", "know", "look", "give", "consider", "much", "asked", "lot", "less",
                    "yesterday", "tomorrow", "publish", "reprint", "yet", "ago")

myStopWords <- c(article_StopWords, news_Stopwords)
