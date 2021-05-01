#ifndef ADJACENCY_HPP
#define ADJACENCY_HPP

#include <cassert>
#include <unordered_set>
#include <utility>
#include <vector>

#include "shared.hpp"

struct AdjacencyList;

template<>
class Clause<adjacency_tag> : public memory_handle {
public:
    Clause() noexcept = default;
    Clause(lit_t *start, lit_t *end) : memory_handle(start, end) {}
};

template<>
class Cnf<adjacency_tag> {
public:
    std::size_t index(const Clause<adjacency_tag> &clause) const {
        return &clause - &clauses.front();
    }

    std::vector<Clause<adjacency_tag>*> units;
    std::vector<Clause<adjacency_tag>> clauses;
    std::vector<lit_t> literals;
    bool contra;
};

class Adjacency : public memory_handle {
public:
    Adjacency() noexcept = default;
    Adjacency(lit_t *start, lit_t *end) : memory_handle(start, end) {}
};

struct AdjacencyList {
    std::vector<Adjacency> adjacency;
    std::vector<lit_t> adjacency_data;
};

class SolverAdjacency : public Solver {
public:
    bool solve() override {
        return solve(0);
    }

    // TODO: refactor this
    void set(std::vector<std::vector<lit_t>> &list) override {
        assign.assigned. clear();
        assign.unassigned.clear();
        assign.variables.clear();
        assign.antecedents.clear();

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
                var_t var = (l > 0) ? l : -l;
                if ((std::size_t)var >= adjacency.size()) {
                    adjacency.resize(2 * var);
                    assign.antecedents.resize(2 * var);

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

            cnf.clauses[c_counter] = Clause<adjacency_tag>(cnf.literals.data() + clause_begin, cnf.literals.data() + l_counter);
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

private:
    void update(var_t variable, var_t d, bool is_true, lit_t antecedent) {
        assign.assigned.push_back(variable);
        assign.unassigned.erase(variable);
        assign.variables[variable] = is_true ? d : -d;
        assign.antecedents[variable] = antecedent;
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
                    if (clause.is_unit()) {
                        cnf.units.push_back(&clause);
                    } else if (clause.is_empty()) {
                        cnf.contra = true;
                        assign.antecedents[0] = cnf.index(clause);
                        assign.variables[0] = d;
                    }
                }
            }
        } else {
            for (auto &&a : adj.adjacency[variable]) {
                if (a > 0) {
                    auto &&clause = cnf.clauses[a];
                    clause.remove(clause.find(variable));
                    if (clause.is_unit()) {
                        cnf.units.push_back(&clause);
                    } else if (clause.is_empty()) {
                        cnf.contra = true;
                        assign.antecedents[0] = cnf.index(clause);
                        assign.variables[0] = d;
                    }
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

    void restore(var_t variable) {
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

            assert((var_t)std::abs(*clause.end()) == variable);
            clause.restore();
        }
    }

    void rollback(var_t d) {
        for (; !assign.assigned.empty(); assign.assigned.pop_back()) {
            auto var = assign.assigned.back();
            assert(var != 0);
            auto &&val = assign.variables[var];
            if ((var_t)std::abs(val) == d)
                break;

            if (val != 0)
                restore(var);
            val = 0;
            assign.unassigned.emplace(var);
        }

        cnf.units.clear();
        cnf.contra = false;
    }

    bool unit_propag(var_t d) {
        for (;!cnf.contra && !cnf.units.empty();) {
            auto clause = *cnf.units.back();
            cnf.units.pop_back();
            if (clause.is_empty())
                continue;

            ++derived;
            auto l = *clause.begin();

            if (l > 0) {
                update(l, d, true, cnf.index(clause));
            } else {
                update(-l, d, false, cnf.index(clause));
            }
        }

        return !cnf.contra;
    }

    bool solve(var_t d) {
        if (!unit_propag(d))
            return false;

        assert(!cnf.contra);
        assert(cnf.units.empty());

        if (assign.unassigned.empty())
            return true;

        auto var = assign.unassigned.begin();
        auto val = *var;

        bool first = rand() % 2 == 0;

        ++decided;
        update(val, d + 1, first, 0);

        if (solve(d + 1))
            return true;
        else
            rollback(d);

        update(val, d + 1, !first, 0);

        return solve(d + 1);
    }

    Cnf<adjacency_tag> cnf;
    AdjacencyList adj;
};

#endif // ADJACENCY_HPP
