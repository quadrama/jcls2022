path_to_corpus <- "../data"
fileList1 <- paste0(path_to_corpus, "/round-1/csv/", list.files(path=paste0(path_to_corpus, "/round-1/csv"), pattern=".csv"))
fileList2 <- paste0(path_to_corpus, "/round-2/V2/csv/", list.files(path=paste0(path_to_corpus, "/round-2/V2/csv"), pattern=".csv"))
fileList <- c(fileList1, fileList2)
corpus <- sapply(fileList, read.csv)
