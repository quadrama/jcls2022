library(DramaAnalysis)
library(tidyverse)
library(igraph)
library(ggplot2)

source("./load-corpus.R")

plot_centrality_curve <- function(corpus, N = 5, cumulative = TRUE,
                                  removeAudience = FALSE,
                                  centrality_fun = list(name = "strength", fun = igraph::strength), ...) {
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
           if (removeAudience == TRUE) {
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
             centrality_in <- centrality_fun$fun(graph = g, mode = "in")
             centrality_in <- as.data.frame(centrality_in)
             centrality_in <- rownames_to_column(centrality_in, var = "character")
             centrality_in$position <- i
             centrality_out <- centrality_fun$fun(graph = g, mode = "out")
             centrality_out <- as.data.frame(centrality_out)
             centrality_out <- rownames_to_column(centrality_out, var = "character")
             centrality_out$position <- i
             merge(centrality_in, centrality_out)
           }))
           centralities <- na.omit(centralities)
           centralities$position <- as.numeric(centralities$position)
           # Fill in missing entities per bin and assign a centrality value of 0
           centralities <- centralities %>% complete(position, nesting(character), fill = list(centrality_in = 0, centrality_out = 0))
           centralities <- pivot_longer(centralities, cols = c("centrality_in", "centrality_out"), names_to = "centrality")
           centralities$centrality <- gsub("centrality_in", "in-strength", centralities$centrality, fixed = TRUE)
           centralities$centrality <- gsub("centrality_out", "out-strength", centralities$centrality, fixed = TRUE)
           plot_name <- tools::file_path_sans_ext(basename(names(corpus)[[i]]))
           gg <- ggplot(centralities, aes(x = position, y = value, colour = centrality)) +
             geom_line() +
             facet_wrap(. ~ character, ncol = 4) +
             #ggtitle(plot_name) +
             xlab("Position") +
             ylab(stringr::str_to_title(centrality_fun$name)) +
             guides(colour=guide_legend(title="Legend")) +
             theme_bw() +
             theme(legend.position="top")
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
plots_strength <- plot_centrality_curve(corpus = corpus, N = 10, cumulative = TRUE,
                                        centrality_fun = list(name = "strength", fun = igraph::strength),
                                        removeAudience = TRUE)
