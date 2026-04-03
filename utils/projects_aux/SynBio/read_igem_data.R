library(readr)
library(dplyr)

# Load files
projects_text <- read_delim("/Users/cristian/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My\ Drive/SynBio/igem_teams/team_project_descriptions.tsv", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

team_meta_full <- read_delim("/Users/cristian/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My\ Drive/SynBio/igem_teams/team_meta_full.tsv", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)

table(team_meta_full$Status)

# Work with accepted teams
team_meta_full <- team_meta_full %>% filter(Status == 'accepted')

# Merge by team ID
df <- merge(team_meta_full,
            projects_text,
            by = "TeamID",
            all.x = TRUE,
            all.y = FALSE)

# Get teams with descriptions
df <- df %>% filter(!is.na(ProjectAbstract))


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
