library(tidyverse)

minisat <- read_csv("minisat.out.txt", col_names="ns")
glucose <- read_csv("glucose.out.txt", col_names="ns")
lingeling <- read_csv("lingeling.out.txt", col_names="ns")

data <- cbind("x"=1:100, "minisat"=minisat$ns, "glucose"=glucose$ns, "lingeling"=lingeling$ns) %>% as_tibble

pdf("/dev/null")
data %>%
    ggplot(aes(x=x)) +
    geom_line(aes(y=minisat, color="minisat")) +
    geom_line(aes(y=glucose, color="glucose")) +
    geom_line(aes(y=lingeling, color="lingeling")) +
    ggtitle("Comparison of SAT solvers on the \"N Queens Puzzle\"") +
    xlab("n") +
    ylab("time [ns]") +
ggsave("plot.png")
