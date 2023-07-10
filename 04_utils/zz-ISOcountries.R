dataset$Countries <- getCountries(dataset$C1)
dataset$IsoCountries <- getIsoCountries(dataset$Countries, iso_search) 


isos <- TopSomething(dataset, coll="IsoCountries", top = 200) %>% data.frame()
colnames(isos) <- c("Iso", "Documents")
isos <- isos[!is.na(isos$Iso),]
isos$Country <- names(iso_search)[match(isos$Iso, unname(iso_search))] %>% toupper()
write.csv(isos, file="isos.csv", row.names = FALSE)

