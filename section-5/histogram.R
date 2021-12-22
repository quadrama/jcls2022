#!/usr/bin/env Rscript

library(tidyverse)
library(DramaAnalysis)

round_any <- function(x, accuracy, f=round){f(x/accuracy)*accuracy}

source("./load-corpus.R")
corpus <- bind_rows(corpus, .id = "file")
corpus <- corpus %>% group_by(file) %>% mutate(percentage=round_any(begin/max(begin)*100, 1))

gg_hist_bins30 <- ggplot(corpus, aes(x=percentage)) + 
  geom_histogram(colour="black", fill="grey", bins = 30) +
  #geom_histogram(aes(y=..density..), colour="black", fill="grey", bins = 30) +
  #geom_density(alpha=.2, fill=qd.colors[8]) +
  #geom_vline(aes(xintercept=mean(percentage)), color=qd.colors[2], linetype="dashed", size=1) +
  xlab("Position in Drama in Percent") +
  ylab("Annotation Count") +
  theme_bw()
ggsave(filename = "./histogram.png", plot = gg_hist_bins30, dpi = 300)
gg_hist_bins30