#!/usr/bin/env Rscript

library(DramaAnalysis)
library(tidyverse)
library(knitr)
library(kableExtra)

source("./load-corpus.R")

# Get DramaAnalysis information
source("./load-dramaanalysis-data.R")

transfers <- Reduce(rbind, lapply(seq(corpus),
                                  function(i) {
                                    match_idx <- regexec("^\\s*transfer\\s*\\(\\s*([^,]+?),\\s*(\\[.+?\\]|.+?),\\s*(.+\\(.+\\)|.+?\\(.+?\\(.+\\)\\))\\s*(.*,\\s*.*?)?\\s*\\)\\s*$", 
                                                         corpus[[i]]$entityLabel)
                                    row_matches <- grepl("^\\s*transfer\\s*\\(\\s*([^,]+?),\\s*(\\[.+?\\]|.+?),\\s*(.+\\(.+\\)|.+?\\(.+?\\(.+\\)\\))\\s*(.*,\\s*.*?)?\\s*\\)\\s*$", 
                                                         corpus[[i]]$entityLabel)
                                    matches <- regmatches(corpus[[i]]$entityLabel, match_idx)
                                    matches <- as.data.frame(do.call(rbind, matches))
                                    matches <- cbind(corpus[[i]]$begin[row_matches], matches)
                                    colnames(matches) <- c("begin", "transfer", "source", "target", "relation", "attribute")
                                    matches$filename <- names(corpus)[[i]]
                                    matches$dramaname <- gsub("_.*" ,"", tools::file_path_sans_ext(basename(names(corpus)[[i]])))
                                    matches
                                  }))
match_idx <- regexec("(.+)\\s*\\(\\s*(.+?),\\s*(.+?)\\s*\\)", transfers$relation)
matches <- regmatches(transfers$relation, match_idx)
matches <- lapply(matches, function(x) {if (length(x)==4) {return(x)} else {return(c(NA, NA, NA, NA))}})
relations <- do.call(rbind, matches)
colnames(relations) <- c("relation", "relation_name", "char1", "char2")
transfers <- cbind(transfers, relations[,c("relation_name", "char1", "char2")])
transfers$annotator <- ifelse(grepl("JH|02", transfers$filename), "02", "01")
transfers <- merge(transfers, id_map, by.x = "dramaname", by.y = "gdc_dramanames")

# Top relations per annotation
top_rels <- transfers %>% group_by(annotator, relation_name) %>% 
  summarize(rel_count = n()) %>% 
  arrange(desc(rel_count), .by_group = TRUE) %>%
  top_n(n=10)
top_rels_1 <- top_rels %>% pivot_wider(id_cols = c("relation_name"), names_from = "annotator", values_from = "rel_count") %>%
  rename(Relation = relation_name) %>%
  mutate(Relation1 = Relation) %>%
  mutate(Relation2 = Relation) %>%
  select(Relation1, `01`)
top_rels_2 <- top_rels %>% pivot_wider(id_cols = c("relation_name"), names_from = "annotator", values_from = "rel_count") %>%
  rename(Relation = relation_name) %>%
  mutate(Relation1 = Relation) %>%
  mutate(Relation2 = Relation) %>%
  select(Relation2, `02`)
top_rels_1 <- arrange(top_rels_1, desc(`01`))
top_rels_2 <- arrange(top_rels_2, desc(`02`))
top_rels <- cbind(top_rels_1, top_rels_2)
save_kable(top_rels %>% mutate(Relation1 = paste0("\\texttt{", Relation1, "}"),
                               Relation2 = paste0("\\texttt{", Relation2, "}")) %>%
             mutate(Relation1 = gsub("_", "\\_", Relation1, fixed = TRUE),
                    Relation2 = gsub("_", "\\_", Relation2, fixed = TRUE)) %>%
  kable(format = "latex", col.names = c("Relation", "Count", "Relation", "Count"),
        booktabs = TRUE, escape = FALSE,
        linesep = "") %>%
    add_header_above(c("Annotation 1" = 2, "Annotation 2" = 2)),
  file = "top_rel.tex")


## Statistical analysis of corpus
length(unique((transfers$gdc_ids))) # Number of texts
nrow(transfers) # Number of transfers
transfers %>% ungroup() %>% group_by(annotator) %>% summarize(n()) # Number of transfers per annotator
transfers %>% ungroup() %>% group_by(filename) %>% summarize(n=n()) %>% summarize(mean=mean(n), sd=sd(n)) # Average number of transfers per drama

# Number of characters that transfer knowledge compared to overall number of characters in play
count_all_char <- nrow(da_dramas$characters[, c("drama", "figure_id")])
count_all_char ## Count of all characters in the plays
count_all_female <- da_dramas$characters[, c("drama", "figure_id", "Gender")] %>% filter(Gender == "FEMALE") %>% nrow()
count_all_male <- da_dramas$characters[, c("drama", "figure_id", "Gender")] %>% filter(Gender == "MALE") %>% nrow()
transfer_char_source <- unique(
  merge(transfers[,c("gdc_ids", "source")], da_dramas$characters[,c("drama", "figure_id")], 
        by.x = c("gdc_ids", "source"),
        by.y = c("drama", "figure_id"))[, c("gdc_ids", "source")]
)
transfer_female_source <- merge(transfers[,c("gdc_ids", "source")], da_dramas$characters[,c("drama", "figure_id", "Gender")], 
                                by.x = c("gdc_ids", "source"),
                                by.y = c("drama", "figure_id"))[, c("gdc_ids", "source", "Gender")] %>% filter(Gender == "FEMALE") %>% unique()
transfer_male_source <- merge(transfers[,c("gdc_ids", "source")], da_dramas$characters[,c("drama", "figure_id", "Gender")], 
                                by.x = c("gdc_ids", "source"),
                                by.y = c("drama", "figure_id"))[, c("gdc_ids", "source", "Gender")] %>% filter(Gender == "MALE") %>% unique()
count_transfer_char_source <- nrow(transfer_char_source) ## Count of characters that appear in a play and transfer knowledge
count_transfer_female_source <- nrow(transfer_female_source)
count_transfer_male_source <- nrow(transfer_male_source)
count_transfer_char_source/count_all_char
count_transfer_female_source/count_all_female
count_transfer_male_source/count_all_male

# Number of characters that receive knowledge compared to overall number of characters in play
transfers_expanded <- transfers
transfers_expanded$target <- transfers_expanded$target %>% gsub("\\[", "", .) %>% gsub("\\]", "", .)
transfers_expanded$target <- transfers_expanded$target %>% gsub("\\(", "", .) %>% gsub("\\)", "", .)
transfers_expanded <- separate_rows(transfers_expanded, target, sep = "\\s*,\\s*")
transfer_char_target <- unique(
  merge(transfers_expanded[,c("gdc_ids", "target")], da_dramas$characters[,c("drama", "figure_id")], 
        by.x = c("gdc_ids", "target"),
        by.y = c("drama", "figure_id"))[, c("gdc_ids", "target")]
)
transfer_female_target <- unique(
  merge(transfers_expanded[,c("gdc_ids", "target")], da_dramas$characters[,c("drama", "figure_id", "Gender")], 
        by.x = c("gdc_ids", "target"),
        by.y = c("drama", "figure_id"))[, c("gdc_ids", "target", "Gender")] %>% filter(Gender == "FEMALE")
)
transfer_male_target <- unique(
  merge(transfers_expanded[,c("gdc_ids", "target")], da_dramas$characters[,c("drama", "figure_id", "Gender")], 
        by.x = c("gdc_ids", "target"),
        by.y = c("drama", "figure_id"))[, c("gdc_ids", "target", "Gender")] %>% filter(Gender == "MALE")
)
count_transfer_char_target <- nrow(transfer_char_target) ## Count of characters that appear in a play and receive knowledge
count_transfer_female_target <- nrow(transfer_female_target)
count_transfer_male_target <- nrow(transfer_female_target)
count_transfer_char_target/count_all_char
count_transfer_female_target/count_all_female
count_transfer_male_target/count_all_male

# How many characters are part of a knowledge transfer?
transfer_char <- unique(rbind(rename(transfer_char_source, char = source), rename(transfer_char_target, char = target))) ## Count of characters that appear either in target or source
transfer_female <- unique(rbind(rename(transfer_female_source, char = source), rename(transfer_female_target, char = target)))
transfer_male <- unique(rbind(rename(transfer_male_source, char = source), rename(transfer_male_target, char = target)))
count_transfer_char <- nrow(transfer_char)
count_transfer_female <- nrow(transfer_female)
count_transfer_male <- nrow(transfer_male)
count_transfer_char/count_all_char
count_transfer_female/count_all_female
count_transfer_male/count_all_male

# How often does source transfer a relation about themselves?
transfers[(transfers$source == transfers$char1) | (transfers$source == transfers$char2),] %>% nrow()
transfers[(transfers$source == transfers$char1) | (transfers$source == transfers$char2),] %>% nrow() / nrow(transfers)
# How often does target get to know something about themselves?
transfers[(transfers$target == transfers$char1) | (transfers$target == transfers$char2),] %>% nrow()
transfers[(transfers$target == transfers$char1) | (transfers$target == transfers$char2),] %>% nrow() / nrow(transfers)
