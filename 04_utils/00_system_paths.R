# Find system and root 
if (Sys.info()["sysname"] == "Windows") {
  print("You are running R on a Windows machine")
  root_path_from <- "C:\\Users\\crist\\OneDrive\\Documentos"
  root_path_to <- ""
} else if (Sys.info()["sysname"] == "Darwin") {
  print("You are running R on a Mac machine")
  root_path_from <- "/Users/cristian/Library/CloudStorage/OneDrive-Personal/Documentos"
  root_path_to   <- "/Users/cristian/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive"
} else {
  print("You are running R on a different operating system")
}