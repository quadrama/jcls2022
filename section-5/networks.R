#!/usr/bin/env Rscript

library(tidyverse)
library(DramaAnalysis)
library(igraph)
library(reshape2)

source("./load-corpus.R")

plot_networks <- function(corpus, mode = "all", removeAudience = TRUE, save = FALSE) {
  lapply(seq(corpus), function(i) {
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
    plot_name <- tools::file_path_sans_ext(basename(names(corpus)[[i]]))
    if (save) {
      png(filename = paste0("plots/", "network_", plot_name, "_", mode, ".png"), 
          width = 8, height = 8, units = 'in', res = 300)
    }
    plot(g,
         edge.label = E(g)$weight,
         edge.width = 1,
         edge.arrow.size = 0.5,
         vertex.size = igraph::strength(g, mode = mode)*2,
         vertex.label.dist=1,
         vertex.color = adjustcolor("SkyBlue2", alpha.f = .5),
         vertex.label.color = adjustcolor("black", alpha.f = .8),
         edge.curved=0.4,
         main = ifelse(save, "", plot_name)
         )
    if (save) {
      dev.off()
    }
  })
}
plot_networks(corpus, mode = "all", removeAudience = TRUE, save = TRUE)
#plot_networks(corpus, mode = "in", removeAudience = TRUE, save = FALSE)
#plot_networks(corpus, mode = "out", removeAudience = TRUE, save = FALSE)