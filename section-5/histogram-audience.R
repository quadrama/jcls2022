#!/usr/bin/env Rscript

library(tidyverse)
library(DramaAnalysis)

round_any <- function(x, accuracy, f=round){f(x/accuracy)*accuracy}

source("load-corpus.R")
corpus <- bind_rows(corpus, .id = "file")
corpus <- corpus %>% group_by(file) %>% mutate(percentage=round_any(begin/max(begin)*100, 1))
match_idx <- regexec("^\\s*transfer\\s*\\(\\s*([^,]+?),\\s*(\\[.+?\\]|.+?),\\s*(?:.+\\(.+\\)|.+?\\(.+?\\(.+\\)\\))\\s*(?:.*,\\s*.*?)?\\s*\\)\\s*$", 
                     corpus$entityLabel)
matches <- regmatches(corpus$entityLabel, match_idx)
matches <- lapply(matches, function(x) {if (length(x) == 0) {c(NA, NA, NA)} else {x}})
matches <- as.data.frame(do.call(rbind, matches))
colnames(matches) <- c("entityLabel", "source", "target")
corpus <- cbind(corpus, matches[, c("source", "target")])
corpus <- corpus %>% filter(!grepl("konsens", file))

corpus <- corpus %>% ungroup() %>% mutate(bins = round(as.numeric(sapply(as.character(percentage %>% OneR::bin(nbins = 30, method = "content")), function(x) {median(as.numeric((unlist(regmatches(x, gregexpr("([[:digit:]](\\.[[:digit:]])?)+", x))))))}))))
corpus$bins <- round_any(corpus$bins, accuracy = 5)
corpus.combined.audience <- rbind(corpus %>% filter(target == "audience") %>% mutate(Target = "Only audience"),
                                  corpus %>% filter(grepl("\\[.*audience.*\\]", target)) %>% mutate(Target = "Audience + characters"))
corpus.combined.characters <- rbind(corpus %>% filter(!grepl("audience", target, fixed = TRUE)) %>% mutate(Target = "Only characters"),
                                    corpus %>% filter(grepl("\\[.*audience.*\\]", target)) %>% mutate(Target = "Characters + audience"))
barwidth <- 2
gg_hist_bins30_stack_dodged <- ggplot() + 
  geom_bar(data = corpus.combined.audience %>% 
             group_by(bins, Target) %>% summarise(Count = n()), 
           mapping = aes(x=bins, y=Count, fill=Target),
           stat = "identity", position = "stack", alpha = 1, width = barwidth) +
  geom_bar(data = corpus.combined.characters %>%
      group_by(bins, Target) %>% summarise(Count = n()),
    mapping = aes(x=bins + barwidth, y=Count, fill=Target), 
    stat = "identity", position = "stack", alpha = 1, width = barwidth) +
  scale_fill_manual(values=c(adjustcolor("SkyBlue2"),
                             adjustcolor(qd.colors[2]),
                             adjustcolor("red"),
                             adjustcolor(qd.colors[10], red.f = .7)),
                    limits = c("Audience + characters", "Only audience", "Characters + audience", "Only characters")) +
  xlab("Position in play in percent") +
  ylab("Annotation count") +
  theme_bw() +
  theme(legend.position = c(.65, .85))
ggsave(filename = "./plots/histogram_audience_stack_dodged.pdf", plot = gg_hist_bins30_stack_dodged, dpi = 300)
gg_hist_bins30_stack_dodged

# Number and percentage of annotations per chunk
corpus %>% group_by(file) %>% mutate(chunk=round_any(begin/max(begin)*100, 4.9)) %>% 
  ungroup() %>% group_by(chunk) %>% filter(grepl(".*audience.*", target)) %>% summarise(N = n()) %>% mutate(percentage=(N/sum(N)*100))
