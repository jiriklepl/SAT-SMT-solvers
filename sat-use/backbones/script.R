#!/usr/bin/Rscript

library('tidyverse')

cadibones <- read_delim("out/cadibones.csv", col_names=c("clauses", "backbones", "time", "calls"), delim=" ")
glucobones <- read_delim("out/glucobones.csv", col_names=c("clauses", "backbones", "time", "calls"), delim=" ")
lglbones <- read_delim("out/lglbones.csv", col_names=c("clauses", "backbones", "time", "calls"), delim=" ")
minibones <- read_delim("out/minibones.csv", col_names=c("clauses", "backbones", "time", "calls"), delim=" ")

for (i in (1:5)*20-10) {
    transform <- function(x) x %>% filter(backbones==i) %>% group_by(clauses) %>% summarize(time=mean(time), calls=mean(calls))
    print_data = function(data) {

    }
    cadicat <- cadibones %>% transform
    minisat <- minibones %>% transform
    glucose <- glucobones %>% transform
    lingeling <- lglbones %>% transform

    data <- cbind("x"=cadicat$clauses, "minisat"=minisat$time, "cadicat"=cadicat$time, "glucose"=glucose$time, "lingeling"=lingeling$time) %>% as_tibble

    pdf("/dev/null")
    data %>%
        ggplot(aes(x=x)) +
        geom_line(aes(y=minisat, color="minisat")) +
        geom_line(aes(y=glucose, color="glucose")) +
        geom_line(aes(y=lingeling, color="lingeling")) +
        geom_line(aes(y=cadicat, color="cadicat")) +
        ggtitle(paste("Using SAT solvers on \"backbones\" program;", 100, "variables,", i, "backbones")) +
        xlab("clauses") +
        ylab("time [ms]") +
    ggsave(paste("times_", i, ".png", sep=""))
    dev.off();

    data <- cbind("x"=cadicat$clauses, "minisat"=minisat$calls, "cadicat"=cadicat$calls, "glucose"=glucose$calls, "lingeling"=lingeling$calls) %>% as_tibble

    pdf("/dev/null")
    data %>%
        ggplot(aes(x=x)) +
        geom_line(aes(y=minisat, color="minisat")) +
        geom_line(aes(y=glucose, color="glucose")) +
        geom_line(aes(y=lingeling, color="lingeling")) +
        geom_line(aes(y=cadicat, color="cadicat")) +
        ggtitle(paste("Using SAT solvers on \"backbones\" program;", 100, "variables,", i, "backbones")) +
        xlab("clauses") +
        ylab("calls") +
    ggsave(paste("calls_", i, ".png", sep=""))
    dev.off();
}
