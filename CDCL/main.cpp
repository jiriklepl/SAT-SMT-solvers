#include <vector>
#include <cassert>
#include <iostream>
#include <unordered_set>
#include <fstream>
#include <cstdint>

#include "satlib.hpp"

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
    lit_t* start;
    lit_t* end;
    lit_t* w1;
    lit_t* w2;
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

template<>
class Clause<nowatch_tag> {
public:
    Clause() noexcept = default;
    Clause(lit_t *start, lit_t *end) : start_(start), end_(end) {
        assert(end > start);
    }

    using iterator = iterator_impl<Clause, lit_t>;
    using const_iterator = iterator_impl<Clause, const lit_t>;

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
class Cnf<nowatch_tag> {
public:
    std::vector<Clause<nowatch_tag>*> units;
    std::vector<Clause<nowatch_tag>> clauses;
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
    std::vector<std::unordered_set<lit_t>> adjacency;
};

struct WatchedList {
    std::vector<std::vector<std::size_t>> watched_at;
};

template<>
class Solver<dpll_tag> {
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
                        if (l > 0) {
                          #if (DEBUG || !NDEBUG)
                            std::size_t n = 
                          #endif
                            adj.adjacency[l].erase(a);
                            assert(n == 1);
                        } else {
                          #if (DEBUG || !NDEBUG)
                            std::size_t n = 
                          #endif
                            adj.adjacency[-l].erase(-a);
                            assert(n == 1);
                        }
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
                        if (l > 0) {
                          #if (DEBUG || !NDEBUG)
                            std::size_t n = 
                          #endif
                            adj.adjacency[l].erase(-a);
                            assert(n == 1);
                        } else {
                          #if (DEBUG || !NDEBUG)
                            std::size_t n = 
                          #endif
                            adj.adjacency[-l].erase(a);
                            assert(n == 1);
                        }
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
            for (auto &&l : clause) {
                assert(l != 0);
                if (l > 0)
                    adj.adjacency[l].emplace(c);
                else
                    adj.adjacency[-l].emplace(-c);
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
                return;

            if (val != 0)
                restore(var);
            val = 0;
            assign.unassigned.emplace(var);
        }

        cnf.units.clear();
        cnf.contra = false;
    }

    bool unit_propag(lit_t d) {
        for (;!cnf.units.empty();) {
            Clause<nowatch_tag> *clause = cnf.units.back();
            cnf.units.pop_back();
            if (clause->is_empty())
                continue;
            auto l = *clause->begin();

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

            if (cnf.contra)
                return false;
        }

        return true;
    }

    bool solve(std::size_t d) {
        if (!unit_propag(d)) {
            cnf.units.clear();
            cnf.contra = false;
            return false;
        }

        if (assign.unassigned.empty()) {
            return !cnf.contra;
        }

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
                if (score[1] > 0) {
                    assign.unassigned.erase(assign.unassigned.find(val));
                }
            }
        }

        if (score[1] > 0) {
            assign.assigned.push_back(val);
            assign.variables[val] = -(d + 1);
            update(val, false);
            if (solve(d + 1)) {
                return true;
            } else {
                rollback(d);
            }
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

        std::size_t l_counter = 0;
        std::size_t c_counter = 0;
        for (auto &&c : list) {
            std::size_t clause_begin = l_counter;
            ++c_counter;

            for (auto &&l : c) {
                cnf.literals[l_counter++] = l;
                std::size_t var = (l > 0) ? l : -l;
                if (var >= adj.adjacency.size()) {
                    adj.adjacency.resize(2 * var);
                    assign.variables.resize(2 * var);
                }

                assign.unassigned.emplace(var);

                auto new_it = adj.adjacency[var].emplace((l > 0) ? c_counter : -(lit_t)c_counter);
                if (new_it.second) {
                    auto it = adj.adjacency[var].find((l < 0) ? c_counter : -(lit_t)c_counter);
                    if (it != adj.adjacency[var].end()) {
                        adj.adjacency[var].erase(it);
                        adj.adjacency[var].erase(new_it.first);

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
    }
};

int main(int argc, const char *argv[])
{
    Solver<dpll_tag> solver;

    {
        std::unique_ptr<satlib_parser> parser;
        if (argc == 2) {
            auto file = std::ifstream(argv[1]);
            parser = satlib_parse(file);
        }
        else {
            parser = satlib_parse(std::cin);
        }
        solver.set(parser->cnf);
    }

    std::cout << (solver.solve(0) ? "s SATISFIABLE" : "s UNSATISFIABLE") << std::endl;

    for (auto &&a : solver.assign.assigned) {
        std::cout << a << ": " << (solver.assign.variables[a] > 0 ?  "true" : solver.assign.variables[a] < 0 ? "false" : "free") << std::endl;
    }
}