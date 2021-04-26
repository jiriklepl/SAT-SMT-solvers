#!/usr/bin/Rscript

library('tidyverse')

adjacency <- read_csv("out/adjacency.csv", col_names=c("variables", "id", "time", "decisions", "derivations")) %>%
    group_by(variables) %>% summarize(time = mean(time), decisions = mean(decisions), derivations = mean(derivations))
watched <- read_csv("out/watched.csv", col_names=c("variables", "id", "time", "decisions", "derivations")) %>%
    group_by(variables) %>% summarize(time = mean(time), decisions = mean(decisions), derivations = mean(derivations)) %>% mutate(variables=1)

adjacency / watched
