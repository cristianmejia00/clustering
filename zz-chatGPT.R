# 20230715

# Using OpenAI in R.

# Globals

# The topic used to infer the query
MAIN_TOPIC <- 'Future Scenarios'

# It means that you know about ...
MAIN_TOPIC_DESCRIPTION <- 'technological forecasting and social change, foresight, and roadmapping in the context of management and innovation.'



# Libraries
library(reticulate)
library(glue)
#library(simplemarkdown)
#install.packages('Rtools')
#install.packages('simplemarkdown')

# One time operation to generate a python env
#reticulate::conda_create(envname = 'openai_env', packages = 'openai', python_version = '3.9')

# Activate enviroment
reticulate::use_condaenv('openai_env')

# import Openai Python library
openai <- reticulate::import('openai')

colnames(rcs_merged)
write.csv(rcs_merged[,c(1,4,5,7,9,10,17,18,20,24,27,28,37,38)], file = 'rcs_merged.csv', row.names = FALSE)
# Attach key.
# In VSCode create a file `openai.key`
# Is only one line with the OpenAi key.
# `credentials/openai.key` was added to .gitignore so is not comitted to the repo.
openai$api_key <- readr::read_file('05_assets/credentials/openai.key')


# utils
ask_gpt <- function(prompt, temperature = 0.1, max_tokens = 500, n = 1) {
  response <- openai$ChatCompletion$create(
    model = 'gpt-3.5-turbo-0613', #'gpt-4-0613'
    messages = prompt,
    temperature = temperature,
    max_tokens = as.integer(max_tokens),
    n = as.integer(n)
  )
}


###################################
###################################
# Article summarization
###################################
prompt_summarize_a_paper <- list(
  list(
    'role' = 'system',
    'content' = 'You are a researcher with a great record of publications and that understands what good academic writing is.
                 Your writing style is that of authors in reputable journals like Nature or Science.
                 Your answers are concise.
                 Your research of expertice is on <<{MAIN_TOPIC}>>, meaning that you know about {MAIN_TOPIC_DESCRIPTION}' 
  ),
  list(
    'role' = 'user',
    'content' = glue('Summarize in one sentence of less than 50 words, and focussing on "{MAIN_TOPIC}" the following text: {cluster_data$text[1]}')
  )
)

###################################
###################################
# Cluster name and summarization
###################################
cluster_data <- subset(dataset, X_C == 15, select = c('TI','AB','AU','PY','UT','Z9','X_E'))
if (nrow(cluster_data) > 5) {
  selected_papers <- c(
    # Most connected
    cluster_data$UT[order(cluster_data$X_E, decreasing = TRUE)][1:5],
    # Most cited
    cluster_data$UT[order(cluster_data$Z9, decreasing = TRUE)][1:5],
    # Newest most connected (X_E is preferred over Z9 because most of paper wont have citations)
    cluster_data$UT[order(cluster_data$PY, cluster_data$X_E, decreasing = TRUE)][1:5]
  ) %>% unique()
  # Only reatain selected papers
  cluster_data <- cluster_data[cluster_data$UT %in% selected_papers,]
}
cluster_data$text <- paste(cluster_data$TI, cluster_data$AB, sep = ' ')

my_texts <- list()
for (i in c(1:10)) {
  my_texts[[i]] <- glue('##### {cluster_data$text[[i]]}')
}
my_texts <- paste(my_texts, collapse = ' ')






prompt_cluster_description <- list(
  list(
    'role' = 'system',
    'content' = 'You are a policy consultant with expertise on <<{MAIN_TOPIC}>>, meaning that you know about {MAIN_TOPIC_DESCRIPTION}
    You will be given multiple texts (a.k.a cluster) separated by #####. Your task is to read the texts and find their common topic. Ideally, the common topic should be framed in the context of <<{MAIN_TOPIC}>>.
    To find the topic name you will follow the next steps:

    Step 1: You understand the main topic of each text
    Step 2: You find common keywords across the texts relevant to <<{MAIN_TOPIC}>>
    Step 3: You summarize all the text in a single paragraph
    Step 4: You conclude by giving a name for the common topic shared by the articles. 
    
    Your answers are concise.' 
  ),
  list(
    'role' = 'user',
    'content' = glue('What is the main topic of the following cluster: {my_texts}')
  )
)


cluster_description <- ask_gpt(prompt_cluster_description)
cluster_description <- cluster_description$choices[[1]]$message$content
cluster_description

prompt_cluster_name <- list(
  list(
    'role' = 'system',
    'content' = 'You are a policy consultant with expertise on <<{MAIN_TOPIC}>>, meaning that you know about {MAIN_TOPIC_DESCRIPTION}
    You will be given the decription of a cluster of documents. 
    Either extract the cluster name given in the description, or give a short name based on what you read. 
    Your answers are concise.' 
  ),
  list(
    'role' = 'user',
    'content' = glue('What is the name of the following cluster?: {cluster_description}')
  )
)

cluster_name <- ask_gpt(prompt_cluster_name, max_tokens = 50)
cluster_name$choices[[1]]$message$content



###################################
###################################
# Cluster figure caption
###################################
prompt_figure_caption <- list(
  list(
    'role' = 'system',
    'content' = 'You are a researcher with a great record of publications and that understands what good academic writing is.
                 Your writing style is that of authors in reputable journals like Nature or Science.
                 Your answers are concise.' 
  ),
  list(
    'role' = 'user',
    'content' = 'Please provide some bullet points with good practices for figure captions based on standard practices in scientific writing.'
  ),
  list(
    'role' = 'assistant',
    'content' = "I can provide some general guidelines for writing figure captions based on standard practices in scientific writing:
      
1. **Caption Length:** Keep the caption concise but descriptive enough to understand the figure without reading the main text. Typically, a few sentences should be sufficient.
2. **Start with a Title:** The caption should start with a brief title that explains what the figure is about.
3. **Description:** After the title, you should provide a detailed explanation of what the figure is displaying. Include necessary details such as the experiment's conditions, what each part of the figure indicates (e.g., axes of graphs), and the significance of the results.
4. **Avoid Redundancy:** The caption should not just repeat information that's already in the text. Instead, it should complement the text and provide extra information to help readers understand the figure.
5. **Key Details:** Any symbols, abbreviations, and units of measurement used in the figure should be clearly defined in the caption.
6. **Statistical Information:** If your figure includes any statistical data, make sure to include information about the statistical tests used, the number of observations, and any relevant significance values.
7. **Avoid Interpretation:** Try not to include any conclusions or interpretations in the figure caption. The role of the caption is to describe, while interpretation of the data should be left for the main body of the text.
8. **Referencing the Figure:** When you refer to the figure in your text, make sure to use the same terminology as in the caption. For example, if your caption refers to panels A, B, and C, your text should use the same terms.
9. **Readability:** Make sure your caption is easily understandable and accessible to the readership of the journal. Avoid overly technical jargon where possible and consider the background knowledge of your audience.
10. **Proofread:** Finally, make sure to proofread your caption carefully. Errors in your caption can confuse readers and potentially undermine the credibility of your work."
  ),
  list(
    'role' = 'user',
    'content' = glue("Please write concisely a figure caption explaining the following:
-	The figure represents the citation network of {MAIN_TOPIC}
-	Each cluster is represented with different colors.
-	Clusters are labeled from the largest by the number of documents.
-	Clusters that are near each other tend to be topically related and those apart are topically divergent.")
  )
)

figure_caption <- ask_gpt(prompt_figure_caption)
figure_caption$choices[[1]]$message$content
