# 20230717

# Create bibliography

# Inputs
dataset <- dataset[!duplicated(dataset$UT),]


# Create the keys for citation analysis
citation_keys <- sapply(c(1:nrow(dataset)), function(x) {
  if (dataset$AU[x] != '') {
    first_au <- gsub(',|;', '', dataset$AU[x]) %>% strsplit(' ') %>% unlist() %>% tolower() %>% .[[1]] 
  } else {
    first_au <- 'anon'
  }
  first_kwd <- tolower(dataset$TI[x]) %>% gsub('^the |^a |^an ', '', .) %>% gsub(' of | the | a | an | from | to | in | on ', ' ', .) %>% strsplit(' ') %>% unlist() %>% .[c(1:2)] %>% paste(collapse = '-')
  paste(first_au, as.character(dataset$PY[x]), first_kwd, sep = '-')
})

citation_keys[duplicated(citation_keys)] <- paste(citation_keys[duplicated(citation_keys)], c(1:length(citation_keys[duplicated(citation_keys)])), collapse = '')
dataset$citation_key <- citation_keys


