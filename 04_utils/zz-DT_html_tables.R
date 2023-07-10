# 20220712

# To complement the DASH dashboard using Colab
# Here we create the DT tables that will be loaded in the DASHboard 

# Dashboard: https://colab.research.google.com/drive/1DhGJ-3Qe_LyCMHir7dX2wbEgfx5xwrYM#scrollTo=kQpT1sMUKigh

# INPUT: An clustering environment. In particular the myDataCorrect table
# OUTPUT: 
# - the dataset_keys.csv
# - DT html widgets one per cluster and one for all. 

colnames(myDataCorrect)

dataset_keys <- myDataCorrect[,c("UT", "X_C", "X_E","PY", "Z9")]
write.csv(dataset_keys, file="dataset_keys.csv")

library(tools)
library(DT)
library(htmlwidgets)
# ###############
# template = '
# <details>
#   <summary>
#     <span>
#       <strong>--TITLE--</strong>
#     </span>
#     <a href="--URL--" target="_blank">🔗</a><br>
#     <span style="color:gray;font-size:0.8em;">
#       --AUTHORS--; <em>--JOURNAL--</em>; --YEAR--
#     </span>
#   </summary>
#   <span><strong>Countries: </strong>--COUNTRIES--</span><br>
#   <span><strong>Affiliations: </strong>--AFFILIATIONS--</span><br>
#   <span><strong>Abstract: </strong>--ABSTRACT--</span>
# </details>
# '

# If we use the full template we will create false URL for articles without DOI
# hence, we split and check for DOI existance
template_first = '
<details>
  <summary>
    <span>
      <strong>--TITLE--</strong>
    </span>'
template_link = '<a href="--URL--" target="_blank">🔗</a>'
template_last = '
    <br>
    <span style="color:gray;font-size:0.8em;">
      --AUTHORS--; <em>--JOURNAL--</em>; --YEAR--
    </span>
  </summary>
  <span><strong>Countries: </strong>--COUNTRIES--</span><br>
  <span><strong>Affiliations: </strong>--AFFILIATIONS--</span><br>
  <span><strong>Abstract: </strong>--ABSTRACT--</span>
</details>
'


myDataCorrect["metadata"] <- sapply(c(1:nrow(myDataCorrect)), function(x) {
  tmp_first <- gsub("--TITLE--",  toTitleCase(myDataCorrect$TI[x]), template_first)
  if( myDataCorrect$DI[x] != "" ) {
    tmp_link <- gsub("--URL--", paste("https://doi.org/", myDataCorrect$DI[x], sep = ""), template_link)
    tmp_first <- paste(tmp_first, tmp_link, sep = "")
  }
  tmp_last <- gsub("--AUTHORS--", myDataCorrect$AU[x], template_last)
  tmp_last <- gsub("--JOURNAL--", toupper(myDataCorrect$SO[x]), tmp_last)
  tmp_last <- gsub("--YEAR--", myDataCorrect$PY[x], tmp_last)
  #tmp_last <- gsub("--COUNTRIES--", myDataCorrect$TI[x], tmp_last)
  #tmp_last <- gsub("--AFFILIATIONS--", myDataCorrect$TI[x], tmp_last)
  tmp_last <- gsub("--ABSTRACT--", myDataCorrect$AB[x], tmp_last)
  return(paste(tmp_first, tmp_last, sep = ""))
})

colnames(myDataCorrect)





myDataCorrect <- dataset[c(1:500),]
# Sort the dataset based on the desired column, usually degree X_E
myDataCorrect <- myDataCorrect[order(myDataCorrect$X_E, decreasing = TRUE),]
data_subset <- myDataCorrect[,c("metadata", "PY", "X_C", "X_E", "Z9")]
write.csv(data_subset, file="table_html.csv", row.names = FALSE)

# Save the articles tables
dir.create("assets")
for( ii in c(0:15) ) {
  if ( ii == 0 ) {
    data_subset <- myDataCorrect[,c("metadata", "PY", "X_C", "X_E", "Z9")]
  } else {
    data_subset <- myDataCorrect[myDataCorrect$X_C == ii, c("metadata", "PY", "X_C", "X_E", "Z9")]
  }
  html_table <- datatable(data_subset, 
                          rownames = FALSE, 
                          escape = FALSE, 
                          colnames=c("Article", "Year", "Cluster", "Degree", "Cites"),
                          width = '100%', 
                          options = list(scrollX = TRUE))
  saveWidget(html_table, file = paste('assets/articles_', as.character(ii), '.html', sep = ''))
}


