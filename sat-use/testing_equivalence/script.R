#!/usr/bin/env Rscript

library("tidyverse")

short <- read_csv("out/short.csv", col_names=c("variables1", "id1", "variables2", "id2", "time", "impl"))
long <- read_csv("out/long.csv", col_names=c("variables1", "id1", "variables2", "id2", "time", "impl"))
long_eq <- read_csv("out/long_eq.csv", col_names=c("variables1", "id1", "variables2", "id2", "time", "impl"))

short   <- short   %>% group_by(variables1, variables2) %>% summarize(variables1=1, variables2=1, time=mean(time))
long    <- long    %>% group_by(variables1, variables2) %>% summarize(time=mean(time))
long_eq <- long_eq %>% group_by(variables1, variables2) %>% summarize(time=mean(time))

print(long/short)
print(long_eq/short)
