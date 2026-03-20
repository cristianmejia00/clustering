# Settings file
# Update the settings and run the file. 
# This creates directory in the GDrive bibliometrics folder and JSON file inside it
# With the directive for creating the dataset.

settings <- list()

## Dataset Metadata
settings$metadata <- list(
  # Raw files path (# As downloaded in WoS, etc.)
  raw_input_directory = "/Users/cristian/Library/CloudStorage/OneDrive-Personal/Documentos/imacros/downloads",
  raw_input_folder_name = "Q344 - hydrogen economy",
  
  # Directory path
  bibliometrics_directory = "/Users/cristian/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive",#"C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics",#
  project_folder = "Q344 - hydrogen economy",
  
  # Analysis ID (the date + number is the label of this analysis)
  date_id = "2026-02-27",#format(Sys.Date(), "%Y-%m-%d"),
  
  # Query and data
  query = 'Q344 - hydrogen economy',
  type_of_dataset = "papers", # "papers", "patents" or "news"
  dataset_source = "wos", # wos, derwent, factiva (dimensions = wos)
  
  # project
  dataset_name = "Q344 - hydrogen economy",
  dataset_file_name_suffix = "Q344 ", #suffix is used for file names
  dataset_description = "",
  created_by = "cristianmejia00@gmail.com",
  notes = "Project texts from Q344 - hydrogen economy"
)

## Dataset General Parameters
settings$filtering = list(
  "f01" = list(
    rows_filter = list(
      removed_duplicated_UT = TRUE,
      most_recent_year = format(Sys.Date(), "%Y")
    ),
    columns_filter = list(
      columns_selected = c("PT", "AU", "TI", "SO", "LA", "DT", "DE", "ID", "AB", "C1", "OI", "AF", "OA",
                           "RP", "FU", "FX", "CR", "NR", "TC", "Z9", "U1", "U2", "PU", "SN", "J9",
                           "JI", "PY", "VL", "IS", "BP", "EP", "AR", "DI", "PG", "WC", "SC","UT",
                           'is_science_diplomacy', 'is_bibliometrics', 'policy_dimension',
                           'policy_dimension_reasoning', 'framework_2025_categories_main',
                           'framework_2025_categories_detailed', 'framework_2025_reasoning',
                           'article_type', 'article_type_reasoning', 'main_domain',
                           'geographic_focus', 'geographic_focus_reasoning', 'power_dynamic',
                           'power_dynamic_reasoning', 'diplomacy_goal', 'diplomacy_goal_reasoning',
                           'key_actors', 'key_actors_reasoning', 'global_commons',
                           'global_commons_reasoning', 'geopolitical_context',
                           'geopolitical_context_reasoning', 'outcome', 'technology_type', 'notes', 'model')
    )
  )
)

# Embed Parameters
settings$embeds <- list(
  # If copute embeds
  get_embeds = TRUE,
  
  # The filtered label
  from_filtered_dataset = "f01",
  
  # Embeds parameters
  "e01" = list(
    # The text columns to combine to form the corpus
    text_columns = c("TI", "AB"),
    
    # Text preparation
    to_lowercase = FALSE,
    remove_stopwords = FALSE,
    remove_numbers = FALSE,
    remove_symbols = FALSE,
    stemming = FALSE,
    lemmatization = FALSE,
    
    # Column to use as the ID of the embeds. It can be a concatenation of multiple columns
    id_column = c("UT"),
    
    # The huggingface ID of the embed model
    transformer_model = "all-MiniLM-L6-v2",
    
    # Comments
    notes = ""
  )
)


settings$network <- list(
  # If compute network
  get_network = TRUE,
  
  # The filtered label
  from_filtered_dataset = "f01",
  
  # Type of network
  network_type = "direct_citation", # "direct_citation", "bibliographic_coupling", "co-citation"
  
  # notes
  notes = ""
)



###############################################################################
project_folder_path <- file.path(settings$metadata$bibliometrics_directory, 
                                  settings$metadata$project_folder)
dir.create(project_folder_path, showWarnings = FALSE)

settings_file_path = file.path(settings$metadata$bibliometrics_directory, 
                               settings$metadata$project_folder,
                               paste("settings_dataset_directive_",
                                     format(Sys.time(), "%Y-%m-%d-%H-%M"),
                                     ".json",
                                     sep = ""))

# Save readable settings
writeLines(RJSONIO::toJSON(settings, pretty=TRUE, auto_unbox=TRUE), 
           settings_file_path)

# Print to console
settings_file_path
