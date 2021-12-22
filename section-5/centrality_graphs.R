#!/usr/bin/env Rscript

library(DramaAnalysis)
library(tidyverse)
library(igraph)
library(ggplot2)

source("./load-corpus.R")

plot_centrality_curve <- function(corpus, N = 5, cumulative = TRUE,
                                  removeAudience = FALSE,
                                  removeAllZero = TRUE,
                                  centrality_fun = igraph::degree, ...) {
  if (cumulative) {
    cumulative_str <- "_cumulative"
  } else {
    cumulative_str <- ""
  }
  lapply(seq(corpus),
         function(i) {
           match_idx <- regexec("^\\s*transfer\\s*\\(\\s*([^,]+?),\\s*(\\[.+?\\]|.+?),\\s*(?:.+\\(.+\\)|.+?\\(.+?\\(.+\\)\\))\\s*(?:.*,\\s*.*?)?\\s*\\)\\s*$", 
                                corpus[[i]]$entityLabel)
           row_matches <- grepl("^\\s*transfer\\s*\\(\\s*([^,]+?),\\s*(\\[.+?\\]|.+?),\\s*(?:.+\\(.+\\)|.+?\\(.+?\\(.+\\)\\))\\s*(?:.*,\\s*.*?)?\\s*\\)\\s*$", 
                                corpus[[i]]$entityLabel)
           matches <- regmatches(corpus[[i]]$entityLabel, match_idx)
           matches <- as.data.frame(do.call(rbind, matches))
           matches <- cbind(corpus[[i]]$begin[row_matches], matches)
           colnames(matches) <- c("begin", "transfer", "source", "target")
           matches$target <- matches$target %>% gsub("\\[", "", .) %>% gsub("\\]", "", .)
           matches$target <- matches$target %>% gsub("\\(", "", .) %>% gsub("\\)", "", .)
           matches <- separate_rows(matches, target, sep = "\\s*,\\s*")
           # Remove parsing mistakes
           matches <- matches[!grepl("knowledge", matches$target, fixed = TRUE),]
           matches <- matches[!grepl("transfer", matches$target, fixed = TRUE),]
           matches <- matches[!grepl("identity", matches$target, fixed = TRUE),]
           matches <- matches[!grepl("parent_of", matches$target, fixed = TRUE),]
           if (removeAudience) {
             # Remove audience
             matches <- matches[(matches$target != "audience") & (matches$source != "audience"),]
             matches <- matches[(matches$target != "auidence") & (matches$source != "auidence"),] # Typo
             if (nrow(matches) == 0) {
               return()
             }
           }
           matches <- matches[order(matches$begin),]
           bins <- split(matches, cut(matches$begin, N, labels = 1:N))
           if (cumulative) {
             bins.copy <- bins
             last_bin <- bins[[1]]
             for (j in seq(bins)) {
               if (j > 1) {
                 last_bin <- rbind(last_bin, bins[[j]])
                 bins.copy[[j]] <- last_bin
               }
             }
             bins <- bins.copy
           }
           centralities <- Reduce(rbind, lapply(seq(bins), function(i) {
             b <- bins[[i]]
             transfer_count <- b %>% select(source, target) %>% 
               group_by(source, target) %>% 
               mutate(Count = n()) %>% unique() %>% na.omit()
             if (nrow(transfer_count) == 0) {
               return(NA)
             }
             edges <- transfer_count
             colnames(edges) <- c("from", "to", "weight")
             vertices <- unique(c(edges$to, edges$from))
             g <- graph_from_data_frame(edges, directed = TRUE, vertices = vertices)
             centrality <- centrality_fun$fun(graph = g, normalize = TRUE, 
                                              directed = TRUE)
             centrality <- as.data.frame(centrality)
             centrality <- rownames_to_column(centrality, var = "character")
             centrality$position <- i
             centrality
           }))
           centralities <- na.omit(centralities)
           centralities$position <- as.numeric(centralities$position)
           levels(centralities$position) <- unique(centralities$position)
           # Fill in missing entities per bin and assign a centrality value of 0
           centralities <- centralities %>% complete(position, nesting(character), fill = list(centrality = 0))
           if (removeAllZero) {
             centralities <- centralities %>% ungroup() %>% group_by(character) %>% filter(any(centrality !=0))
           }
           if (nrow(centralities) == 0) {
             return(NA)
           }
           plot_name <- tools::file_path_sans_ext(basename(names(corpus)[[i]]))
           gg <- ggplot(centralities, aes(x = position, y = centrality, group = character)) +
             geom_line() +
             facet_wrap(. ~ character, ncol = 3) +
             #ggtitle(plot_name) +
             xlab("Position") +
             ylab(stringr::str_to_title(centrality_fun$name)) +
             theme_bw()
           print(paste0("plots/", "centrality-curve_", plot_name, "_", 
                        centrality_fun$name, cumulative_str, ".png"))
           ggsave(filename = paste0("plots/", "centrality-curve_", plot_name, "_", 
                                    centrality_fun$name, cumulative_str, ".png"), 
                  plot = gg,
                  dpi = 300)
           gg
         }
  )
}
# plots_degree <- plot_centrality_curve(corpus = corpus, 
#                                       N = 10, 
#                                       cumulative = TRUE,
#                                       removeAudience = TRUE,
#                                       removeAllZero = TRUE,
#                                       centrality_fun = list(name = "degree", fun = igraph::degree), 
#                                       normalize = TRUE)
# plots_indegree <- plot_centrality_curve(corpus = corpus, 
#                                         N = 10, 
#                                         cumulative = TRUE,
#                                         removeAudience = TRUE,
#                                         removeAllZero = TRUE,
#                                         centrality_fun = list(name = "in-degree", fun = igraph::degree),
#                                         mode = "in",
#                                         normalize = TRUE)
# plots_outdegree <- plot_centrality_curve(corpus = corpus, 
#                                          N = 10, 
#                                          cumulative = TRUE,
#                                          removeAudience = TRUE,
#                                          removeAllZero = TRUE,
#                                          centrality_fun = list(name = "out-degree", fun = igraph::degree), 
#                                          mode = "out", 
#                                          normalize = TRUE)
# plots_strength <- plot_centrality_curve(corpus = corpus, 
#                                         N = 10, 
#                                         cumulative = TRUE,
#                                         removeAudience = TRUE,
#                                         removeAllZero = TRUE,
#                                         centrality_fun = list(name = "strength", fun = igraph::strength))
# plots_instrength <- plot_centrality_curve(corpus = corpus, 
#                                           N = 10, 
#                                           cumulative = TRUE,
#                                           removeAudience = TRUE,
#                                           removeAllZero = TRUE,
#                                           centrality_fun = list(name = "in-strength", fun = igraph::strength), 
#                                           mode = "in")
# plots_outstrength <- plot_centrality_curve(corpus = corpus, 
#                                            N = 10, 
#                                            cumulative = TRUE,
#                                            removeAudience = TRUE,
#                                            removeAllZero = TRUE,
#                                            centrality_fun = list(name = "out-strength", fun = igraph::strength), 
#                                            mode = "out")
plots_betweenness <- plot_centrality_curve(corpus = corpus, 
                                           N = 10, 
                                           cumulative = TRUE,
                                           removeAudience = TRUE,
                                           removeAllZero = TRUE,
                                           centrality_fun = list(name = "betweenness", fun = igraph::betweenness), 
                                           normalize = TRUE, 
                                           directed = TRUE)
