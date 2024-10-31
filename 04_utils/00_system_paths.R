# Find system and root
if (Sys.info()["sysname"] == "Windows") {
  print("You are running R on a Windows machine")
  raw_data_folder_path <- "C:\\Users\\crist\\OneDrive\\Documentos"
  bibliometrics_folder_path <- ""
} else if (Sys.info()["sysname"] == "Darwin") {
  print("You are running R on a Mac machine")
  raw_data_folder_path <- "/Users/cristian/Library/CloudStorage/OneDrive-Personal/Documentos"
  bibliometrics_folder_path <- "/Users/cristian/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/Bibliometrics_Drive"
} else {
  print("You are running R on a different operating system")
}
