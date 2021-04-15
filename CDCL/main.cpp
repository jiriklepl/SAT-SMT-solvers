#include <vector>
#include <cassert>
#include <iostream>
#include <unordered_set>
#include <fstream>
#include <cstdint>

#include "satlib.hpp"
#include "dimacs.hpp"

using lit_t = std::int32_t;

struct watch_tag;
struct nowatch_tag;

struct dpll_tag;
struct cdcl_tag;

template<typename> class Clause;
template<typename> class Cnf;
struct Assignment;
struct AdjacencyList;
struct WatchedList;
template<typename> class Solver;

template<>
class Clause<watch_tag> {
public:
    Clause() noexcept = default;
    Clause(lit_t *_start, lit_t *_end, std::vector<std::unordered_set<Clause*>> &watched_at) noexcept
        : start(_start), end(_end), w1(_start), w2(_end - 1), satisfied(0) {
            assert(start != nullptr);
            assert(end != nullptr);
            assert(w1 != w2);

            watched_at[std::abs(*w1)].emplace(this);
            watched_at[std::abs(*w2)].emplace(this);
        }
    bool update(const std::vector<lit_t> &variables, std::vector<std::unordered_set<Clause*>> &watched_at, lit_t variable) {
        auto tmp = w2;
        while (true) {
            ++w2;
            if (w2 == end) w2 = start;
            if (w2 == tmp) break;
            else if (variables[std::abs(*w2)] == 0) {
                if (w2 == w1) {
                    continue;
                } else {
                    assert(variables[std::abs(*w1)] == 0);
                    watched_at[variable].erase(this);
                    watched_at[std::abs(*w2)].emplace(this);
                    break;
                }
            } else if (variables[std::abs(*w2)] * *w2 > 0) {
                w2 = tmp;
                return true;
            }
        }

        assert(w1 != w2);
        assert(watched_at[std::abs(*w1)].find(this) != watched_at[std::abs(*w1)].end());
        assert(watched_at[std::abs(*w2)].find(this) != watched_at[std::abs(*w2)].end());
        assert((variables[std::abs(*w1)] == 0) || (variables[std::abs(*w2)] != 0));

        return false;
    }

    lit_t get_watch() const { return *w1; }
    lit_t snd_watch() const { return *w2; }
    void assure_watch(lit_t variable) { 
        if (std::abs(*w2) != variable)
            std::swap(w1, w2);
        assert(std::abs(*w2) == variable);
    }
    bool is_unit(const std::vector<lit_t> &variables) const {
        return variables[std::abs(*w1)] == 0 && variables[std::abs(*w2)] != 0;
    }
    bool is_empty(const std::vector<lit_t> &variables) const {
        if (variables[std::abs(*w1)] != 0) {
            assert(variables[std::abs(*w2)] != 0);
            return true;
        }
        return false;
    }
private:
    lit_t* start;
    lit_t* end;
    lit_t* w1;
    lit_t* w2;
public:
    lit_t satisfied;
};

template<typename parent, typename V>
struct iterator_impl {
    friend parent;
    V* where;

    constexpr auto operator<=>(const iterator_impl&) const noexcept = default;
    iterator_impl &operator++() noexcept { ++where; return *this; }
    constexpr iterator_impl operator+(std::size_t i) const noexcept { return iterator_impl(where + i); }
    constexpr std::ptrdiff_t operator-(const iterator_impl &it) const noexcept { return (std::ptrdiff_t)(where - it.where); }
    V &operator*() noexcept { return *where; }
private:
    constexpr iterator_impl() noexcept = default;
    constexpr iterator_impl(V *where) noexcept : where(where) {}
};

class memory_handle {
public:
    memory_handle() noexcept = default;
    memory_handle(lit_t *start, lit_t *end) : start_(start), end_(end) {
        assert(end >= start);
    }

    using iterator = iterator_impl<memory_handle, lit_t>;
    using const_iterator = iterator_impl<memory_handle, const lit_t>;

    constexpr iterator begin() noexcept { return iterator(start_); }
    constexpr iterator end() noexcept { return iterator(end_); }

    constexpr const_iterator begin() const noexcept { return const_iterator(start_); }
    constexpr const_iterator end() const noexcept { return const_iterator(end_); }

    const_iterator find(lit_t l) const noexcept {
        auto it = begin();
        for (; it != end(); ++it)
            if (*it == l) break;
        return it;
    }

    iterator find(lit_t l) noexcept {
        auto it = begin();
        for (; it != end(); ++it)
            if (*it == l) break;
        return it;
    }

    void remove(iterator it) noexcept {
        assert(it >= begin() && it < end());
        if (it != --end_)
            std::swap(*it, *end_);
    }

    void restore() noexcept {
        ++end_;
    }

    constexpr std::size_t length() const noexcept {
        return end() - begin();
    }

    constexpr bool is_unit() const noexcept {
        return length() == 1;
    }

    constexpr bool is_empty() const noexcept {
        return length() == 0;
    }

private:
    lit_t* start_;
    lit_t* end_;
};

template<>
class Clause<nowatch_tag> : public memory_handle {
public:
    Clause() noexcept = default;
    Clause(lit_t *start, lit_t *end) : memory_handle(start, end) {}
};

class Adjacency : public memory_handle {
public:
    Adjacency() noexcept = default;
    Adjacency(lit_t *start, lit_t *end) : memory_handle(start, end) {}
};

template<>
class Cnf<nowatch_tag> {
public:
    std::vector<Clause<nowatch_tag>*> units;
    std::vector<Clause<nowatch_tag>> clauses;
    std::vector<lit_t> literals;
    bool contra;
};

template<>
class Cnf<watch_tag> {
public:
    std::vector<Clause<watch_tag>*> units;
    std::vector<Clause<watch_tag>*> satisfied;
    std::vector<Clause<watch_tag>> clauses;
    std::vector<lit_t> literals;
    bool contra;
};

struct Assignment {
    /* list of variable assignments
     * 0 = unassigned or free
     * +d = assigned true at decision lvl d
     * -d = assigned false at decision lvl d
     */
    std::vector<lit_t> variables;

    // list of assigned variables
    std::vector<std::size_t> assigned;

    // set of unassigned variables
    std::unordered_set<std::size_t> unassigned;
};

struct AdjacencyList {
    std::vector<Adjacency> adjacency;
    std::vector<lit_t> adjacency_data;
};

struct WatchedList {
    std::vector<std::unordered_set<Clause<watch_tag>*>> watched_at;
};

template<>
class Solver<watch_tag> {
public:
    Cnf<watch_tag> cnf;
    Assignment assign;
    WatchedList wch;
    bool unit_propag(lit_t d) {
        for (;!cnf.contra && !cnf.units.empty();) {
            auto clause = *cnf.units.back();
            cnf.units.pop_back();
            if (clause.satisfied > 0)
                continue;
            auto l = clause.get_watch();

            assert(l != 0);

            if (l > 0) {
                update(l, d, true);
            } else {
                update(-l, d, false);
            }
        }

        return !cnf.contra;
    }

    void update(lit_t variable, lit_t d, bool is_true) {
        assign.assigned.push_back(variable);
        assign.unassigned.erase(variable);
        assign.variables[variable] = is_true ? d : -d;
        auto copy = wch.watched_at[variable];
        for (auto &&clause : copy) {
            if (clause->satisfied > 0)
                continue;
            clause->assure_watch(variable);
            bool pos = (clause->snd_watch() > 0) ? is_true : !is_true;

            if (pos || clause->update(assign.variables, wch.watched_at, variable)) {
                assert(clause->satisfied == 0);
                cnf.satisfied.push_back(clause);
                clause->satisfied = d;
                continue;
            }
            if (clause->is_unit(assign.variables))
                cnf.units.emplace_back(clause);
            else if (clause->is_empty(assign.variables)) {
                cnf.contra = true;
                break;
            }
        }
    }

    bool solve(lit_t d) {
        if (!unit_propag(d)) {
            return false;
        }

        if (assign.unassigned.empty())
            return true;

        auto var = assign.unassigned.begin();
        auto val = *var;

        update(val, d + 1, true);

        if (solve(d + 1))
            return true;
        else rollback(d);

        update(val, d + 1, false);

        if (solve(d + 1))
            return true;
        else rollback(d);

        return false;
    }

    void rollback(std::size_t d) {
        for (; !assign.assigned.empty(); assign.assigned.pop_back()) {
            auto var = assign.assigned.back();
            assert(var != 0);
            auto &&val = assign.variables[var];
            assert(val != 0);
            if (std::abs(val) <= d)
                break;

            val = 0;
            assign.unassigned.emplace(var);
        }

        for (; !cnf.satisfied.empty(); cnf.satisfied.pop_back()) {
            auto var = cnf.satisfied.back();
            assert(var != nullptr);
            auto &&val = var->satisfied;
            assert(val != 0);
            if (std::abs(val) <= d)
                break;

            val = 0;
        }

        cnf.units.clear();
        cnf.contra = false;
    }

    void set(std::vector<std::vector<lit_t>> &list) {
        assign.assigned. clear();
        assign.unassigned.clear();
        assign.variables.clear();
        wch.watched_at.clear();
        cnf.contra = false;
        std::size_t total_size = 0;

        for (auto &&c : list) {
            total_size += c.size();
        }

        cnf.literals.resize(total_size);
        cnf.clauses.resize(list.size() + 1);

        std::size_t l_counter = 0;
        std::size_t c_counter = 0;
        for (auto &&c : list) {
            std::size_t clause_begin = l_counter;
            ++c_counter;

            for (auto &&l : c) {
                cnf.literals[l_counter++] = l;
                std::size_t var = (l > 0) ? l : -l;
                if (var >= wch.watched_at.size()) {
                    wch.watched_at.resize(2 * var);
                    assign.variables.resize(2 * var);
                }

                assign.unassigned.emplace(var);
            }

            new (cnf.clauses.data() + c_counter) Clause<watch_tag>(cnf.literals.data() + clause_begin, cnf.literals.data() + l_counter, wch.watched_at);

            if (clause_begin == l_counter)
                exit(20);
        }
    }
};

template<>
class Solver<nowatch_tag> {
public:
    Cnf<nowatch_tag> cnf;
    Assignment assign;
    AdjacencyList adj;
    void update(lit_t variable, bool is_true) {
        if (is_true) {
            for (auto &&a : adj.adjacency[variable]) {
                if (a > 0) {
                    auto &&clause = cnf.clauses[a];
                    clause.remove(clause.find(variable));
                    for (auto &&l : clause) {
                        if (l > 0)
                            adj.adjacency[l].remove(adj.adjacency[l].find(a));
                        else
                            adj.adjacency[-l].remove(adj.adjacency[-l].find(-a));
                    }
                } else {
                    auto &&clause = cnf.clauses[-a];
                    clause.remove(clause.find(-variable));
                    if (clause.is_unit()) cnf.units.push_back(&clause);
                    if (clause.is_empty()) cnf.contra = true;
                }
            }
        } else {
            for (auto &&a : adj.adjacency[variable]) {
                if (a > 0) {
                    auto &&clause = cnf.clauses[a];
                    clause.remove(clause.find(variable));
                    if (clause.is_unit()) cnf.units.push_back(&clause);
                    if (clause.is_empty()) cnf.contra = true;
                } else {
                    auto &&clause = cnf.clauses[-a];
                    clause.remove(clause.find(-variable));
                    for (auto &&l : clause) {
                        if (l > 0)
                            adj.adjacency[l].remove(adj.adjacency[l].find(-a));
                        else
                            adj.adjacency[-l].remove(adj.adjacency[-l].find(a));
                    }
                }
            }
        }
    }

    void restore(std::size_t variable) {
        assert(variable != 0);
        for (auto &&a : adj.adjacency[variable]) {
            assert(a != 0);
            const auto c = (a > 0) ? a : - a;
            auto &&clause = cnf.clauses[c];
            if ((assign.variables[variable] > 0 && a > 0) || (assign.variables[variable] < 0 && a < 0))
            for (auto &&l : clause) {
                assert(l != 0);
                adj.adjacency[std::abs(l)].restore();
            }

            assert(std::abs(*clause.end()) == variable);
            clause.restore();
        }
    }

    void rollback(std::size_t d) {
        for (; !assign.assigned.empty(); assign.assigned.pop_back()) {
            auto var = assign.assigned.back();
            assert(var != 0);
            auto &&val = assign.variables[var];
            if (std::abs(val) == d)
                break;

            if (val != 0)
                restore(var);
            val = 0;
            assign.unassigned.emplace(var);
        }

        cnf.units.clear();
        cnf.contra = false;
    }

    bool unit_propag(lit_t d) {
        for (;!cnf.contra && !cnf.units.empty();) {
            auto clause = *cnf.units.back();
            cnf.units.pop_back();
            if (clause.is_empty())
                continue;
            auto l = *clause.begin();

            if (l > 0) {
                assign.assigned.push_back(l);
                assign.unassigned.erase(l);
                assign.variables[l] = d;
                update(l, true);
            } else {
                assign.assigned.push_back(-l);
                assign.unassigned.erase(-l);
                assign.variables[-l] = -d;
                update(-l, false);
            }
        }

        return !cnf.contra;
    }

    bool solve(std::size_t d) {
        if (!unit_propag(d))
            return false;

        assert(!cnf.contra);
        assert(cnf.units.empty());

        if (assign.unassigned.empty())
            return true;

        auto var = assign.unassigned.begin();
        auto val = *var;
        assign.unassigned.erase(var);
        std::size_t score[] = {0, 0};
        for (auto &&c : adj.adjacency[val])
            ++score[(c > 0) ? 0 : 1];

        if (score[0] + score[1] == 0) {
            assign.assigned.push_back(val);
            assign.variables[val] = 0;
            return solve(d);
        }

        if (score[0] > 0) {
            assign.assigned.push_back(val);
            assign.variables[val] = d + 1;
            update(val, true);

            if (solve(d + 1)) {
                return true;
            } else {
                rollback(d);

                if (score[1] > 0)
                    assign.unassigned.erase(val);
            }
        }

        if (score[1] > 0) {
            assign.assigned.push_back(val);
            assign.variables[val] = -(d + 1);
            update(val, false);

            if (solve(d + 1))
                return true;
        }

        return false;
    }

    void set(std::vector<std::vector<lit_t>> &list) {
        assign.assigned. clear();
        assign.unassigned.clear();
        assign.variables.clear();
        adj.adjacency.clear();
        cnf.contra = false;
        std::size_t total_size = 0;

        for (auto &&c : list) {
            total_size += c.size();
        }

        cnf.literals.resize(total_size);
        cnf.clauses.resize(list.size() + 1);

        std::vector<std::unordered_set<lit_t>> adjacency;

        std::size_t l_counter = 0;
        std::size_t c_counter = 0;
        for (auto &&c : list) {
            std::size_t clause_begin = l_counter;
            ++c_counter;

            for (auto &&l : c) {
                cnf.literals[l_counter++] = l;
                std::size_t var = (l > 0) ? l : -l;
                if (var >= adjacency.size()) {
                    adjacency.resize(2 * var);
                    assign.variables.resize(2 * var);
                }

                assign.unassigned.emplace(var);

                auto new_it = adjacency[var].emplace((l > 0) ? c_counter : -(lit_t)c_counter);
                if (new_it.second) {
                    auto it = adjacency[var].find((l < 0) ? c_counter : -(lit_t)c_counter);
                    if (it != adjacency[var].end()) {
                        adjacency[var].erase(it);
                        adjacency[var].erase(new_it.first);

                        --c_counter;
                        l_counter  = clause_begin;
                        goto skip_clause;
                    }
                } else {
                    --l_counter;
                }
            }

            cnf.clauses[c_counter] = Clause<nowatch_tag>(cnf.literals.data() + clause_begin, cnf.literals.data() + l_counter);
            skip_clause:;
        }

        total_size = 0;

        for (auto &&adj_var : adjacency)
            total_size += adj_var.size();

        adj.adjacency_data.resize(total_size);
        std::size_t adj_count = 0;

        for (auto &&adj_var : adjacency) {
            auto adj_beg = adj_count;

            for (auto &&clause : adj_var) {
                adj.adjacency_data[adj_count++] = clause;
            }

            adj.adjacency.emplace_back(adj.adjacency_data.data() + adj_beg, adj.adjacency_data.data() + adj_count);
        }
    }
};

int main(int argc, const char *argv[])
{
    Solver<watch_tag> solver;

    {
        std::unique_ptr<dimacs_parser> parser;
        if (argc == 2) {
            auto file = std::ifstream(argv[1]);
            parser = dimacs_parse(file);
        }
        else {
            parser = dimacs_parse(std::cin);
        }
        solver.set(parser->cnf);
    }

    std::cout << (solver.solve(0) ? "s SATISFIABLE" : "s UNSATISFIABLE") << std::endl;

    for (auto &&a : solver.assign.assigned) {
        std::cout << a << ": " << (solver.assign.variables[a] > 0 ?  "true" : solver.assign.variables[a] < 0 ? "false" : "free") << std::endl;
    }
}