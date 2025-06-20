# Analysis Settings file
# This is a directive settings file
# We use it as input
# Settings file

settings <- list()

## Metadata
settings$metadata <- list(
  # Directory path
  bibliometrics_folder = "/Users/cristian/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive", # "C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics",#
  project_folder = "Q336_derwent_fukuhara",
  filtered_folder = "f01",
  analysis_id = "a01_cn__f01_dc__c01_lv"#"a01_tm__f01_e01__km01" #"a01_cn__f01_dc__c01_lv" #
)


## General Parameters
settings$params <- list(
  recursive_level = 3, # Reports will be generated to this level. 0 means clusters, 1 subclusters, 2 subclusters of subclusters
  dataset_source = "wos",
  unit_of_analysis = "cluster", # topic, cluster, facet, firm, country, institution, author, etc.
  type_of_analysis = "citation_network", # "topic_model", "citation_network", or "both"
  seed = 100 # The seed for random initialization. Needed for reproducibility
)


########################################################### for 00_clustering.R
## Citation network options
if (settings$params$type_of_analysis %in% c("citation_network", "both")) {
  settings$cno <- list(
    # Shall we use the network file from Fukan System (i.e. mission.pairs.tsv is available)
    # if False, we create our own network file from scratch.
    using_mission_pairs_from_fukan = FALSE,

    # Shall we use the initial clustering solution from Fukan System?
    # If TRUE, we use the column "_C" in mission.facet.all, and hence we can use the figure from Fukan System
    # If FALSE, we compute a new "_C" column based on the algorithm of choice.
    using_initial_column_C_from_fukan = FALSE,

    # Type of network
    network_type = "direct_citation",

    # The component subsetting
    # By default we take the largets comp
    # 'all' = everything
    # 'top' = The top n components by size
    # 'component' = Only the selected component
    # 'min_vertices' = The min size of vertex in comp to be considered. For example value = 2, will remove all floating isolated nodes.
    component = list(
      strategy = "top",
      value = 1
    ),

    # Clustering
    clustering = list(
      algorithm = "louvain" # "louvain", OR "newman" OR "infomap"
    ),

    # Thresholding.
    # i.e. Creation of the `dataset_minimal`. Recursive clustering and aggregation of small clusters

    thresholding = list(
      # Either...
      # - Proportion of articles that determines the number of level0 clusters (<1)(e.g. #largest clusters contain 90%, 0.9, of articles )
      # - Number of cluster to consider from the Fukan System solution (1+)
      threshold = 0.90,

      # Top max clusters to analyze per iteration
      max_clusters = 1000, # When clustering level 0 has more than 100 clusters pick a large number

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
      size_lower_limit = 45,

      # When recursive clustering there is a label "-0" that might be annoying. TRUE to remove it. However, Excel will think they are dates.
      remove_zero = FALSE
    )
  )
}


## Topic Model options
if (settings$params$type_of_analysis %in% c("topic_model", "both")) {
  settings$tmo <- list(
    # embeds to use within filtered folder
    embeds_folder = 'e01',
    
    # The integer column with the Year of publication
    year_column = "PY",

    # The number of topics to get. Use 0 to infer the topics with HDBScan
    n_topics = 62,

    # The minimum size for a topic
    min_topic_size = 5)
}

## add-ons
settings$addons <- list(
  "include_orphans" = "NO", # NO, 99, 999
  # Sentiment analysis is computed outside R, with Python
  # The dataset must contain the columns:
  # - `sentiment` NUMERIC. with a score between -1 and 1
  # - `sentiment_factor` STRING ENUM[positive, neutral, negative] with the sentiment label
  "sentiment_analysis" = FALSE
)

###########################################################
## For LLM
settings$llm <- list(
  "theme" = "Sustainability",
  "description"  = "academic research about Sustainability",
  "compute" = c("old_paper_summaries", "representative_docs_summaries", "cluster_title", "cluster_description", "cluster_enhanced_description")
)

########################################################### for 00_reports.R
## Reporting
settings$rp <- list(
  most_recent_year = 2025, # This is needed so the charts do not plot 2024, or future years where data is incomplete
  top_documents = 0, # 0 means ALL # Select the number of top documents to show in the article report
  top_items = 20, ## 0 means ALL # Select the number of top `documents`field`` to show in the clusters report
  text_columns = c("TI", "AB"), # Column(s) with text contents to merge and analyze
  article_report_columns = list("X_C", "cluster_code", "AU", "PY", "DI", "TI", "AB", "Z9", "X_E", "DE", "SO", "WC", 
                                "Countries", "UT", "sentiment", "sentiment_factor"
                                # 'ethnicities', 'race', 'is_japanese', 'nationality', 'gender',
                                # 'nationality_processed', 'ethnicities_total_elements',
                                # 'ethnicities_unique_elements', 'ethnicities_variety_ratio',
                                # 'ethnicities_hhi', 'ethnicities_hhi_normalized',
                                # 'ethnicities_shannon_entropy', 'ethnicities_shannon_max',
                                # 'ethnicities_shannon_evenness', 'ethnicities_simpson_diversity',
                                # 'ethnicities_simpson_dominance', 'ethnicities_berger_parker_dominance',
                                # 'ethnicities_effective_species_shannon',
                                # 'ethnicities_effective_species_simpson', 'ethnicities_gini_coefficient',
                                # 'ethnicities_coefficient_of_variation', 'race_total_elements',
                                # 'race_unique_elements', 'race_variety_ratio', 'race_hhi',
                                # 'race_hhi_normalized', 'race_shannon_entropy', 'race_shannon_max',
                                # 'race_shannon_evenness', 'race_simpson_diversity',
                                # 'race_simpson_dominance', 'race_berger_parker_dominance',
                                # 'race_effective_species_shannon', 'race_effective_species_simpson',
                                # 'race_gini_coefficient', 'race_coefficient_of_variation',
                                # 'nationality_total_elements', 'nationality_unique_elements',
                                # 'nationality_variety_ratio', 'nationality_hhi',
                                # 'nationality_hhi_normalized', 'nationality_shannon_entropy',
                                # 'nationality_shannon_max', 'nationality_shannon_evenness',
                                # 'nationality_simpson_diversity', 'nationality_simpson_dominance',
                                # 'nationality_berger_parker_dominance',
                                # 'nationality_effective_species_shannon',
                                # 'nationality_effective_species_simpson', 'nationality_gini_coefficient',
                                # 'nationality_coefficient_of_variation', 'is_japanese_total_elements',
                                # 'is_japanese_unique_elements', 'is_japanese_variety_ratio',
                                # 'is_japanese_hhi', 'is_japanese_hhi_normalized',
                                # 'is_japanese_shannon_entropy', 'is_japanese_shannon_max',
                                # 'is_japanese_shannon_evenness', 'is_japanese_simpson_diversity',
                                # 'is_japanese_simpson_dominance', 'is_japanese_berger_parker_dominance',
                                # 'is_japanese_effective_species_shannon',
                                # 'is_japanese_effective_species_simpson', 'is_japanese_gini_coefficient',
                                # 'is_japanese_coefficient_of_variation', 'gender_total_elements',
                                # 'gender_unique_elements', 'gender_variety_ratio', 'gender_hhi',
                                # 'gender_hhi_normalized', 'gender_shannon_entropy', 'gender_shannon_max',
                                # 'gender_shannon_evenness', 'gender_simpson_diversity',
                                # 'gender_simpson_dominance', 'gender_berger_parker_dominance',
                                # 'gender_effective_species_shannon', 'gender_effective_species_simpson',
                                # 'gender_gini_coefficient', 'gender_coefficient_of_variation',
                                # 'Z9_rank_normalized',
                                # 'institutions_total_elements',
                                # 'institutions_unique_elements',
                                # 'institutions_variety_ratio',
                                # 'institutions_hhi',
                                # 'institutions_hhi_normalized',
                                # 'institutions_shannon_entropy',
                                # 'institutions_shannon_max',
                                # 'institutions_shannon_evenness',
                                # 'institutions_simpson_diversity',
                                # 'institutions_simpson_dominance',
                                # 'institutions_berger_parker_dominance',
                                # 'institutions_effective_species_shannon',
                                # 'institutions_effective_species_simpson',
                                # 'institutions_gini_coefficient',
                                # 'institutions_coefficient_of_variation',
                                # 'countries_total_elements',
                                # 'countries_unique_elements',
                                # 'countries_variety_ratio',
                                # 'countries_hhi',
                                # 'countries_hhi_normalized',
                                # 'countries_shannon_entropy',
                                # 'countries_shannon_max',
                                # 'countries_shannon_evenness',
                                # 'countries_simpson_diversity',
                                # 'countries_simpson_dominance',
                                # 'countries_berger_parker_dominance',
                                # 'countries_effective_species_shannon',
                                # 'countries_effective_species_simpson',
                                # 'countries_gini_coefficient',
                                # 'countries_coefficient_of_variation'
                                ),
  categorical_long_reports = list("AU", "WC", "SO", "Countries", "Institutions", "DE", "sentiment_factor", "ID", "issues",
                                  'ethnicities', 'race', 'is_japanese', 'nationality', 'gender'), # Columns in the dataset for long-form summary. These are also used for RCS.
  categorical_simple_wide_reports = list("PY", "sentiment_factor"), # Columns in the dataset without ';' for matrix type summary
  categorical_multi_wide_reports = list("WC", "Countries", "Institutions", "issues"), # Columns in the dataset with ';' for matrix type summary
  numerical_reports = list("PY", "Z9", "sentiment", "score")
                           # 'nationality_processed', 'ethnicities_total_elements',
                           # 'ethnicities_unique_elements', 'ethnicities_variety_ratio',
                           # 'ethnicities_hhi', 'ethnicities_hhi_normalized',
                           # 'ethnicities_shannon_entropy', 'ethnicities_shannon_max',
                           # 'ethnicities_shannon_evenness', 'ethnicities_simpson_diversity',
                           # 'ethnicities_simpson_dominance', 'ethnicities_berger_parker_dominance',
                           # 'ethnicities_effective_species_shannon',
                           # 'ethnicities_effective_species_simpson', 'ethnicities_gini_coefficient',
                           # 'ethnicities_coefficient_of_variation', 'race_total_elements',
                           # 'race_unique_elements', 'race_variety_ratio', 'race_hhi',
                           # 'race_hhi_normalized', 'race_shannon_entropy', 'race_shannon_max',
                           # 'race_shannon_evenness', 'race_simpson_diversity',
                           # 'race_simpson_dominance', 'race_berger_parker_dominance',
                           # 'race_effective_species_shannon', 'race_effective_species_simpson',
                           # 'race_gini_coefficient', 'race_coefficient_of_variation',
                           # 'nationality_total_elements', 'nationality_unique_elements',
                           # 'nationality_variety_ratio', 'nationality_hhi',
                           # 'nationality_hhi_normalized', 'nationality_shannon_entropy',
                           # 'nationality_shannon_max', 'nationality_shannon_evenness',
                           # 'nationality_simpson_diversity', 'nationality_simpson_dominance',
                           # 'nationality_berger_parker_dominance',
                           # 'nationality_effective_species_shannon',
                           # 'nationality_effective_species_simpson', 'nationality_gini_coefficient',
                           # 'nationality_coefficient_of_variation', 'is_japanese_total_elements',
                           # 'is_japanese_unique_elements', 'is_japanese_variety_ratio',
                           # 'is_japanese_hhi', 'is_japanese_hhi_normalized',
                           # 'is_japanese_shannon_entropy', 'is_japanese_shannon_max',
                           # 'is_japanese_shannon_evenness', 'is_japanese_simpson_diversity',
                           # 'is_japanese_simpson_dominance', 'is_japanese_berger_parker_dominance',
                           # 'is_japanese_effective_species_shannon',
                           # 'is_japanese_effective_species_simpson', 'is_japanese_gini_coefficient',
                           # 'is_japanese_coefficient_of_variation', 'gender_total_elements',
                           # 'gender_unique_elements', 'gender_variety_ratio', 'gender_hhi',
                           # 'gender_hhi_normalized', 'gender_shannon_entropy', 'gender_shannon_max',
                           # 'gender_shannon_evenness', 'gender_simpson_diversity',
                           # 'gender_simpson_dominance', 'gender_berger_parker_dominance',
                           # 'gender_effective_species_shannon', 'gender_effective_species_simpson',
                           # 'gender_gini_coefficient', 'gender_coefficient_of_variation','Z9_rank_normalized',
                           # 'institutions_total_elements',
                           # 'institutions_unique_elements',
                           # 'institutions_variety_ratio',
                           # 'institutions_hhi',
                           # 'institutions_hhi_normalized',
                           # 'institutions_shannon_entropy',
                           # 'institutions_shannon_max',
                           # 'institutions_shannon_evenness',
                           # 'institutions_simpson_diversity',
                           # 'institutions_simpson_dominance',
                           # 'institutions_berger_parker_dominance',
                           # 'institutions_effective_species_shannon',
                           # 'institutions_effective_species_simpson',
                           # 'institutions_gini_coefficient',
                           # 'institutions_coefficient_of_variation',
                           # 'countries_total_elements',
                           # 'countries_unique_elements',
                           # 'countries_variety_ratio',
                           # 'countries_hhi',
                           # 'countries_hhi_normalized',
                           # 'countries_shannon_entropy',
                           # 'countries_shannon_max',
                           # 'countries_shannon_evenness',
                           # 'countries_simpson_diversity',
                           # 'countries_simpson_dominance',
                           # 'countries_berger_parker_dominance',
                           # 'countries_effective_species_shannon',
                           # 'countries_effective_species_simpson',
                           # 'countries_gini_coefficient',
                           # 'countries_coefficient_of_variation') # Numeric columns in the dataset for summary (min, max, mean, median, sd)
  # methods = c("Data collection from WOS", "Created citation network", "Extracted Maximum Component", "Clustering using the Louvain method", "Cluster description")
)

# Column labels are used to format RCS columns and charts' labels
if (settings$params$dataset_source == "wos") {
  settings$rp$column_labels <- list(
    "X_C" = "Cluster Index",
    "cluster_code" = "Cluster Code",
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

if (settings$params$dataset_source == "derwent") {
  settings$rp$column_labels <- list(
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
    # "sentiment" = "Sentiment score",
    # "sentiment_factor" = "Sentiment",
    "UT" = "Patent Number"
  )
}

if (settings$params$dataset_source == "factiva") {
  settings$rp$column_labels <- list(
    "X_C" = "Cluster",
    "TI" = "Headline",
    "AB" = "Main paragraph",
    "X_E" = "Score",
    "PY" = "Publication Years",
    "SO" = "Newspapers",
    "AU" = "Factiva Types",
    "Countries" = "Regions",
    "Institutions" = "Entities",
    # "WC" = "Categories",
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
settings$stopwords$article_StopWords <- list(
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
settings$stopwords$news_Stopwords <- list(
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

settings$stopwords$myStopWords <- list(
  settings$stopwords$article_StopWords,
  settings$stopwords$news_Stopwords
) %>% unlist()



###############################################################################
# In the case of the analysis settings we must create the directory first.
ana_lysis_results_folder_path <- file.path(
  settings$metadata$bibliometrics_folder,
  settings$metadata$project_folder,
  settings$metadata$analysis_id
)
dir.create(ana_lysis_results_folder_path)

# Save the analysis directive
settings_file_path <- file.path(
  ana_lysis_results_folder_path,
  paste("settings_analysis_directive_",
    format(Sys.time(), "%Y-%m-%d-%H-%M"),
    ".json",
    sep = ""
  )
)

# Save readable settings
writeLines(
  RJSONIO::toJSON(settings, pretty = TRUE, auto_unbox = TRUE),
  settings_file_path
)

# Print to console
settings_file_path
