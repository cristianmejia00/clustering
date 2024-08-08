# input_folder -> bibliometrics_folder
# project_name -> project_folder
# project_id -> analysis_folder

settings <- list()

## Metadata
settings$analysis_metadata <- list(
  # Directory path
  bibliometrics_folder = "C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics",#"/Users/cristian/Library/CloudStorage/OneDrive-Personal/Documentos/03-bibliometrics",#
  project_folder = "Q301",
  analysis_folder = "001", # Equivalent to Fukan's analysis (i.e. the order inside dataset)

  # Query and data
  query = 'TS=("pay*" NEAR/2 "ecosystem* service*") OR TS=("pay*" NEAR/2 "environment* service*")',
  query_id = "Q301", # This is the Folder name. Equivalent to Fukan's dataset
  fukan_url = "Not apply. Compute directly",
  downloaded_documents = "2605",

  # project
  project_name = "Payment for Ecosystem Service",
  project_description = "Citation Network of Payment for Ecosystem Service",
  date = "2024-08-08",
  created_by = "cristianmejia00@gmail.com",
  notes = "updates to Q293"
)

## General Parameters
settings$params <- list(
  type_of_dataset = "papers", # "papers", "patents" or "news"
  unit_of_analysis = "cluster", # topic, cluster, facet, firm, country, institution, author, etc.
  type_of_analysis = "citation_network", # "topic_model" or "citation_network"
  dataset_source = "wos", # wos, derwent, factiva (dimensions = wos)
  recursive_level = 1,   # Reports will be generated to this level. Topic Models are always 0.
  seed = 100 # The seed for random initialization. Needed for reproducibility
)

########################################################### for 00_clustering.R
## Citation network options
if (settings$params$type_of_analysis == "citation_network") {
  settings$cno <- list(
    # Shall we use the network file from Fukan System (i.e. mission.pairs.tsv is available)
    # if False, we create our own network file from scratch.
    using_mission_pairs_from_fukan = FALSE,

    # Shall we use the initial clustering solution from Fukan System?
    # If TRUE, we use the column "_C" in mission.facet.all, and hence we can use the figure from Fukan System
    # If FALSE, we compute a new "_C" column based on the algorithm of choice.
    using_initial_column_C_from_fukan = FALSE,

    # Either...
    # - Proportion of articles that determines the number of level0 clusters (<1)(e.g. #largest clusters contain 90%, 0.9, of articles )
    # - Number of cluster to consider from the Fukan System solution (1+)
    threshold = 0.97,
    
    # Top max clusters to analyze per iteration
    max_clusters = 1000, #When clustering level 0 has more than 100 clusters pick a large number

    # Cluster scope
    scope = "all", # "all" OR "cl99" OR "cl_99"

    ### Options for clustering or recursive clustering.
    ### The following options are useful for any of these conditions
    ### - We want recursive clustering
    ### - we want clustering at level0, either because:
    ###   - we don't want to use Fukan System X_C
    ###   - We have a WOS dataset without X_C

    # Subcluster only if having this or more
    size_limit = 350,

    # Include cluster having collecting a minimum of __ articles
    size_lower_limit = 70,

    # When recursive clustering there is a label "-0" that might be annoying. TRUE to remove it. However, Excel will think they are dates.
    remove_zero = FALSE,

    # Algorithm to use
    algor = "louvain" # "louvain", OR "newman" OR "infomap"
  )
}


## Topic Model options
if (settings$params$type_of_analysis == "topic_model") {
  settings$tmo <- list(
    # Shall we use the initial clustering solution included in the dataset?
    # If TRUE, we use the column "X_C" in dataset, this column was previously computed 
    # using a Topic Model algorithm, usually HDBScan or Kmeans from Python, or LDA from my other codes
    # If FALSE, we compute a new "X_C" column based on the algorithm of choice. Usually LDA.
    using_initial_column_x_C = TRUE
    
    # # The `using_initial_column_x_C = TRUE`, then all the following 
    # # are NOT necessary, because we do not compute the topic model here.
    # # Select the number of topics
    # K = 86, # select "0" zero to automatically detect the topics
    # 
    # # Gibbs sampling parameter
    # G = 500, # iterations
    # alpha = 0.02,
    # eta = 0.02,
    # 
    # # More options
    # useStemming = TRUE,
    # fullReports = FALSE, # for KXD and WXK
    # 
    # # Select the level of relevance
    # # 1 = Words ordered based on simple frequency within the topic
    # # 0 = Words ordered based on how unique they are to the topic
    # # 0.6 is recommended
    # relevance_value = 1
  )
}


# add-ons
settings$addons <- list(
  "include_orphans" = "NO", # NO, 99, 999
  # Sentiment analysis is computed outside R, with Python
  # The dataset must contain the columns:
  # - `sentiment` NUMERIC. with a score between -1 and 1
  # - `sentiment_factor` STRING ENUM[positive, neutral, negative] with the sentiment label
  "sentiment_analysis" = FALSE,
  # These are possible if we provide a network file. 
  "page_rank" = FALSE,
  "eigen_centrality" = FALSE,
  "closeness_centrality" = FALSE,
  "betweeness_centrality" = FALSE
)

########################################################### 
## For LLM
settings$llm <- list(
  "theme" =  "patent analysis of agriculture machinery",
  "description" = "the mechanical structures and devices used in farming or other agriculture. There are many types of such equipment, from hand tools and power tools to tractors and the countless kinds of farm implements that they tow or operate.",
  "compute" = c("old_paper_summaries", "representative_docs_summaries", "cluster_title", "cluster_description", "cluster_enhanced_description")
  )

########################################################### for 00_reports.R
## Reporting
settings$rp <- list(
  most_recent_year = 2024, # This is needed so the charts do not plot 2024, or future years where data is incomplete
  top_documents = 0, # 0 means ALL # Select the number of top documents to show in the article report
  top_items = 20, ## 0 means ALL # Select the number of top `documents`field`` to show in the clusters report
  text_columns = c("TI", "AB"), # Column(s) with text contents to merge and analyze
  article_report_columns = c('X_C','cluster_code','AU','PY','DI','TI','AB','Z9','X_E','DE','SO','WC','Countries','UT', 'sentiment', 'sentiment_factor'),
  categorical_long_reports = c("AU", "WC", "SO", "Countries", "Institutions", "DE", "sentiment_factor", "ID", "issues"), # Columns in the dataset for long-form summary. These are also used for RCS.
  categorical_simple_wide_reports = c("PY", "sentiment_factor"), # Columns in the dataset without ';' for matrix type summary
  categorical_multi_wide_reports = c("WC", "Countries", "Institutions", "issues"), # Columns in the dataset with ';' for matrix type summary
  numerical_reports = c("PY", "Z9", "sentiment", "score"), # Numeric columns in the dataset for summary (min, max, mean, median, sd)
  methods = c("Data collection from WOS", "Created citation network", "Extracted Maximum Component", "Clustering using the Louvain method", "Cluster description")
#c('Data collection from Factiva',
              #'Embeddings',
              #'UMAP',
              #'HDBScan')
  # Need to capture the parameters of the topic model
  #c("Data collection from WOS", "Created citation network", "Extracted Maximum Component", "Clustering using the Louvain method", "Cluster description")
)

# Column labels are used to format RCS columns and charts' labels
if (settings$params$dataset_source == 'wos') {
  settings$rp$column_labels <- c(
    "X_C" = "Cluster",
    "TI" = "Title",
    "AB" = "Abstract",
    "AU" = "Authors",
    "PY" = "Publication Years",
    "X_E" = "Degree", 
    "SO" = "Journals",
    "Countries" = "Countries",
    "Institutions" = "Institutions",
    "DI" = "DOI",
    "WC" = "Categories",
    "DE" = "Author Keywords",
    "ID" = "WOS Keywords",
    "Z9" = "Citations",
    "score" = "Score",
    "sentiment" = "Sentiment score",
    "sentiment_factor" = "Sentiment",
    "UT" = "ID"
  )
}

if (settings$params$dataset_source == 'derwent') {
  settings$rp$column_labels <- c(
    "X_C" = "Cluster",
    "TI" = "Title",
    "AB" = "Abstract",
    "AU" = "Inventors",
    "PY" = "Publication Years",
    "X_E" = "Degree", 
    "SO" = "Firms",
    "Countries" = "Countries",
    "Institutions" = "Asignees",
    "DI" = "DOI",
    "WC" = "IPC",
    "DE" = "Author Keywords",
    "Z9" = "Citations",
    "score" = "Score",
    #"sentiment" = "Sentiment score",
    #"sentiment_factor" = "Sentiment",
    "UT" = "Patent Number"
  )
}

if (settings$params$dataset_source == 'factiva') {
  settings$rp$column_labels <- c(
    "X_C" = "Cluster",
    "TI" = "Headline",
    "AB" = "Main paragraph",
    "X_E" = "Score", 
    "PY" = "Publication Years",
    "SO" = "Newspapers",
    "AU" = "Factiva Types",
    "Countries" = "Regions",
    "Institutions" = "Entities",
    #"WC" = "Categories",
    "DE" = "Categories",
    "ID" = "Entities",
    "score" = "Score",
    "sentiment" = "Sentiment score",
    "sentiment_factor" = "Sentiment",
    "UT" = "ID",
    "issues" = "Issues",
    "Keywords" = "Keywords"
  )
}

# Activate stopwords
settings$stopwords <- list()
settings$stopwords$article_StopWords <- c(
  "analysis", "paper", "na", "say", "will", "can", "article", "use", "press", "release",
  "all", "rights", "reserved", "elsevier", "scopus", "doi", "int", "ieee", "cover", "story",
  # Stemmed
  "use", "structur", "result", "method", "system", "effect", "studi", "measur", "model", "show", "high",
  "observ", "increas", "also", "propos", "two", "base", "investig", "properti", "process", "differ", "obtain",
  "found", "chang"
)

# patent_StopWords <- c("patent", "claim", "device", "data", "module", "network", "control" ,
#                   "base","method", "methods","terminal", "information",
#                   "connect", "connects", "connection", "communication", "internet", "things", "thing")
#
settings$stopwords$news_Stopwords <- c(
  "said", "country", "year", "according", "people", "work", "say", "says", "said",
  "need", "one", "number", "well", "part", "end", "report", "support", "per", "cent", "percent",
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "billion", "million", "thousand",
  "million", "time", "living", "make", "including", "however", "reached", "provide", "expected", "day",
  "set", "important", "come", "many", "made", "way", "take", "total", "want", "com", "now", "like", "able", "get",
  "order", "continue", "aim", "since", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "week",
  "noted", "see", "addition", "put", "present", "month", "received", "taken",
  "january", "february", "march", "april", "may", "june", "july", "august", "september", "november", "october", "december",
  "timescontent", "especially", "know", "look", "give", "consider", "much", "asked", "lot", "less",
  "yesterday", "tomorrow", "publish", "reprint", "yet", "ago"
)

settings$stopwords$myStopWords <- c(
  settings$stopwords$article_StopWords,
  settings$stopwords$news_Stopwords
)

