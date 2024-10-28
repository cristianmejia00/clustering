# Settings file

settings <- list()

## Dataset Metadata
settings$metadata <- list(
  # Raw files path (# As downloaded in WoS, etc.)
  raw_input_directory = "/Users/cristian/Library/CloudStorage/OneDrive-Personal/Documentos/imacros/downloads",
  raw_input_folder_name = "Q311_innovativeness/innovativeness framework",
  
  # Directory path
  bibliometrics_directory = "/Users/cristian/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive",#"C:\\Users\\crist\\OneDrive\\Documentos\\03-bibliometrics",#
  dataset_folder = "Q311_innovativeness",
  
  # Analysis ID (the date + number is the label of this analysis)
  date_id = "2024-10-24",#format(Sys.Date(), "%Y-%m-%d"),
  
  # Query and data
  query = 'Yousif - innovativeness framework',
  type_of_dataset = "papers", # "papers", "patents" or "news"
  dataset_source = "wos", # wos, derwent, factiva (dimensions = wos)
  
  # project
  dataset_name = "Innovativeness Framework",
  dataset_file_name_suffix = "innovs", #suffix is used for file names
  dataset_description = "",
  created_by = "cristianmejia00@gmail.com",
  notes = "Yousif project"
)

## Dataset General Parameters
settings$filtering = list(
  "f001" = list(
    rows_filter = list(
      removed_duplicated_UT = TRUE,
      most_recent_year = format(Sys.Date(), "%Y")
    ),
    columns_filter = list(
      columns_selected = c("PT", "AU", "TI", "SO", "LA", "DT", "DE", "ID", "AB", "C1", "OI", "AF", "OA",
                           "RP", "FU", "FX", "CR", "NR", "TC", "Z9", "U1", "U2", "PU", "SN", "J9",
                           "JI", "PY", "VL", "IS", "BP", "EP", "AR", "DI", "PG", "WC", "SC","UT")
    )
  )
)

# Embed Parameters
settings$embeds <- list(
  # If copute embeds
  get_embeds = TRUE,
  
  # The filtered label
  from_filtered_dataset = "f001",
  
  # Embeds parameters
  "e001" = list(
    # The text columns to combine to form the corpus
    text_columns = c("TI", "AB"),
    
    # Text preparation
    to_lowercase = TRUE,
    remove_stopwords = TRUE,
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
  from_filtered_dataset = "f001",
  
  # Type of network
  network_type = "direct_citation", # "direct_citation", "bibliographic_coupling", "co-citation"
  
  # notes
  notes = ""
)



###############################################################################
settings_file_path = file.path(settings$metadata$bibliometrics_directory, 
                               settings$metadata$dataset_folder,
                               paste("settings_dataset_directive_",
                                     format(Sys.time(), "%Y-%m-%d-%H-%M"),
                                     ".json",
                                     sep = ""))

# Save readable settings
writeLines(RJSONIO::toJSON(settings, pretty=TRUE, auto_unbox=TRUE), 
           settings_file_path)

# Print to console
settings_file_path
