#include "watched.hpp"

int main() {
    Cnf<watch_tag> cnf;
    WatchedList watched_list;
    watched_list.watched_at.resize(3 + 1);

    {
        std::vector<lit_t> clause{1,2,3};
        cnf.learn(clause, watched_list);
    }
    {
        std::vector<lit_t> clause{1,2,3};
        cnf.learn(clause, watched_list);
    }
    {
        std::vector<lit_t> clause{1,2,3};
        cnf.learn(clause, watched_list);
    }
    {
        std::vector<lit_t> clause{1,2,3};
        cnf.learn(clause, watched_list);
    }
    {
        std::vector<lit_t> clause{1,2,3};
        cnf.learn(clause, watched_list);
    }
}
