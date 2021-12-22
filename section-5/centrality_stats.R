#!/usr/bin/env Rscript

library(tidyverse)
library(DramaAnalysis)
library(igraph)
library(knitr)
library(kableExtra)
library(rstatix)

source("./load-corpus.R")
source("./load-dramaanalysis-data.R")

combine_mean_sd <- function(x, round_factor = 2, ...) {
  if (sd(x, ...) == 0) {
    paste0(round(mean(x, ...), digits = round_factor))
  } else {
    paste0(round(mean(x, ...), digits = round_factor), "\\pm", round(sd(x, ...), digits = round_factor))
  }
}

get_centralities <- function(corpus, removeAudience = FALSE) {
  centralities <- Reduce(rbind, lapply(seq(corpus), function(i) {
    d <- corpus[[i]]
    match_idx <- regexec("^\\s*transfer\\s*\\(\\s*([^,]+?),\\s*(\\[.+?\\]|.+?),\\s*(?:.+\\(.+\\)|.+?\\(.+?\\(.+\\)\\))\\s*(?:.*,\\s*.*?)?\\s*\\)\\s*$", 
                         d$entityLabel)
    matches <- regmatches(d$entityLabel, match_idx)
    matches <- as.data.frame(do.call(rbind, matches))
    colnames(matches) <- c("transfer", "source", "target")
    matches$target <- matches$target %>% gsub("\\[", "", .) %>% gsub("\\]", "", .)
    matches$target <- matches$target %>% gsub("\\(", "", .) %>% gsub("\\)", "", .)
    matches <- separate_rows(matches, target, sep = "\\s*,\\s*")
    # Remove parsing mistakes
    matches <- matches[!grepl("knowledge", matches$target, fixed = TRUE),]
    matches <- matches[!grepl("transfer", matches$target, fixed = TRUE),]
    matches <- matches[!grepl("identity", matches$target, fixed = TRUE),]
    matches <- matches[!grepl("parent_of", matches$target, fixed = TRUE),]
    if (removeAudience == TRUE) {
      # Remove audience
      matches <- matches[(matches$target != "audience") & (matches$source != "audience"),]
      matches <- matches[(matches$target != "auidence") & (matches$source != "auidence"),] # Typo
      if (nrow(matches) == 0) {
        return()
      }
    }
    transfer_count <- matches %>% select(source, target) %>% 
      group_by(source, target) %>% 
      mutate(Count = n()) %>% unique() %>% na.omit()
    edges <- transfer_count
    colnames(edges) <- c("from", "to", "weight")
    vertices <- unique(c(edges$to, edges$from))
    g <- graph_from_data_frame(edges, directed = TRUE, vertices = vertices)
    degreeAll <- igraph::degree(graph = g, mode = "total", normalized = TRUE)
    degreeIn <- igraph::degree(graph = g, mode = "in", normalized = TRUE)
    degreeOut <- igraph::degree(graph = g, mode = "out", normalized = TRUE)
    strengthAll <- igraph::strength(graph = g, mode = "all")
    strengthIn <- igraph::strength(graph = g, mode = "in")
    strengthOut <- igraph::strength(graph = g, mode = "out")
    betweenness <- igraph::betweenness(graph = g, directed = TRUE, normalized = TRUE)
    closenessAll <- igraph::closeness(graph = g, mode = "all", normalized = TRUE)
    closenessIn <- igraph::closeness(graph = g, mode = "in", normalized = TRUE)
    closenessOut <- igraph::closeness(graph = g, mode = "out", normalized = TRUE)
    eigenCentrality <- igraph::eigen_centrality(graph = g, directed = TRUE)$vector
    centrality <- as.data.frame(t(rbind(degreeAll, degreeIn, degreeOut, strengthAll, strengthIn, strengthOut,
                                        betweenness, closenessAll, closenessIn, closenessOut, eigenCentrality)))
    centrality <- rownames_to_column(centrality, var = "character")
    centrality$filename <- tools::file_path_sans_ext(basename(names(corpus)[[i]]))
    centrality$dramaname <- gsub("_.*" ,"", tools::file_path_sans_ext(basename(names(corpus)[[i]])))
    centrality <- centrality %>% select(filename, everything())
    centrality
  }))
  centralities
}
centralities <- get_centralities(corpus, removeAudience = FALSE)
centralities$annotator <- ifelse(grepl("02", centralities$filename), "02", "01")
centralities

# Merge with gender information for characters and get centralities per gender
centralities <- merge(centralities, id_map, by.x = "dramaname", by.y = "gdc_dramanames")
centralities <- merge(centralities, da_dramas$characters[,c("drama", "figure_id", "Gender")], 
                      by.x = c("gdc_ids", "character"), 
                      by.y = c("drama", "figure_id"))
centralities <- merge(centralities, characterStatistics(da_dramas)[,c("drama", "character", "tokens")], 
                      by.x = c("gdc_ids", "character"), by.y = c("drama", "character"))
gender_centralities <- centralities %>% filter(!is.na(Gender)) %>% 
  filter(Gender != "UNKNOWN") %>% 
  ungroup() %>% group_by(Gender, annotator) %>%
  mutate(count = n()) %>%
  summarize_if(is.numeric, combine_mean_sd, na.rm = TRUE) %>%
  mutate(gender = tolower(Gender)) %>%
  ungroup() %>%
  select(annotator, gender, count, tokens, strengthAll, strengthIn, strengthOut, betweenness) %>%
  mutate(tokens = sapply(strsplit(tokens, split = "\\pm", fixed = TRUE), 
                         function(x) {paste(round(as.numeric(x)), collapse = "\\pm")}))
gender_centralities <- gender_centralities[order(gender_centralities$annotator),]
save_kable(gender_centralities %>%
             select(-annotator) %>%
             kable(format = "latex", booktabs = TRUE, digits = 2, 
                   col.names = c("{Gender}", "{Count}", "{Tokens}", "{Strength}", "{In-Strength}", "{Out-Strength}", "{Betweenness}"),
                   escape = FALSE,
                   align = c("l", "S[table-format=3.0]", "r", "r", "r", "r", "r")) %>%
             pack_rows("Annotation 1", 1, 2) %>%
             pack_rows("Annotation 2", 3, 4),
           file = "centrality_gender.tex")
