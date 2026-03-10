library(readr)
library(dplyr)

# Load files
projects_text <- read_delim("~/Desktop/data/projects_text.txt", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

team_meta_full <- read_delim("~/Desktop/data/team_meta_full.tsv", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)

# Merge by team ID
df <- merge(projects_text,
            team_meta_full,
            by = "TeamID",
            all.x = TRUE,
            all.y = FALSE)

# Column mapping 
colnames(df)

dff <- df %>% dplyr::rename(
  'UT' = TeamID,
  'TI' = ProjectTitle,
  'AB' = ProjectAbstract,
  'PY' = Year.x,
  'AU' = Team.x,
  'WC' = Topics,
  'DI' = WikiURL,
  'ID' = Track,
  'Countries' = Country,
  'Institutions' = InstitutionName,
) %>% mutate(
  'CR' = '',
  'C1' = ''
)

# Save as input file
write.table(dff, file = 'igem.txt', sep = '\t', row.names = FALSE)
