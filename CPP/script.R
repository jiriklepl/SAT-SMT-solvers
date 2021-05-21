#!/usr/bin/Rscript

library('tidyverse')

options(tibble.print_max = Inf)

col_names <- c("type", "variables", "id", "sat", "time", "decisions", "derivations")

data <- do.call("rbind", lapply(list.files("out", full.names=TRUE), function(x) return (read_csv(x, col_names=col_names)  %>%
    group_by(type, variables) %>% summarize(.groups="keep", name=x, time = mean(time), decisions = mean(decisions), derivations = mean(derivations)))))

data %>% arrange(type, variables, time)
