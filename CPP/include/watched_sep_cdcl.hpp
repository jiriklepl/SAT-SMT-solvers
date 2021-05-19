#ifndef WATCHED_SEP_CDCL_HPP
#define WATCHED_SEP_CDCL_HPP

#include "watched_sep.hpp"

class DeleteUseless {
    std::size_t cache_limit = 1000;
    std::vector<std::size_t> block_distances;

    // caches:
    std::vector<bool> distance_register;
    std::vector<std::size_t> distance_halfer;

public:
    void register_clause(const std::vector<lit_t> &variables, const Clause<watch_sep_tag> &clause) {
        std::size_t level = 0;

        distance_register.clear();
        distance_register.resize(std::abs(variables[clause.get_watch_var()]) + 1, false);

        for (auto [lit, _pos] : clause) {
            auto &&bit = distance_register[std::abs(variables[std::abs(lit)])];

            if (bit)
                continue;

            ++level;
            bit = true;
        }

        assert(distance_register.back());

        block_distances.emplace_back(level);
    }

    void delete_clauses(Cnf<watch_sep_tag> &cnf, WatchedSepList &wch, std::size_t original_clauses) {
        if (block_distances.size() < cache_limit)
            return;

        cache_limit *= 2; // FIXME: discuss with lectures

        std::size_t total = 0, current = 0, middle_dist = 0;
        distance_halfer.clear();

        std::size_t max_dist = 0;
        for (auto &&dist : block_distances) {
            if (dist > max_dist)
                max_dist = dist;
        }

        distance_halfer.resize(max_dist + 1, 0);

        for (auto &&dist : block_distances) {
            assert(dist < distance_halfer.size());

            ++total;
            ++distance_halfer[dist];
        }

        assert(distance_halfer.back() != 0);

        for (auto &&count : distance_halfer) {
            current += count;

            if (current * 2 >= total)
                break;

            ++middle_dist;
        }

        auto end = block_distances.end();
        for (auto it = block_distances.begin(); it != end;) {
            if (*it > middle_dist) {
                cnf.unlearn(it - block_distances.begin() + original_clauses -1, wch);

                if (it != --end)
                    std::swap(*it, *end);

                assert(&*end == &block_distances.back());

                block_distances.pop_back();
            } else {
                ++it;
            }
        }
    }
};

class SolverWatchedSepCDCL : public Solver {
    static constexpr int unit_run = 100;

public:
    SolverWatchedSepCDCL() : cnf(), wch(), luby(), till_restart(unit_run) {}

    bool solve() override {
        return solve(1);
    }

    // TODO: refactor this
    void set(std::vector<std::vector<lit_t>> &list) override {
        assign.assigned. clear();
        assign.unassigned.clear();
        assign.variables.clear();
        assign.antecedents.clear();

        wch.watched_at.clear();
        cnf.contra = false;

        std::size_t total_size = 0;

        for (auto &&c : list)
            total_size += c.size();

        cnf.clauses.resize(list.size() + 1);
        original_clauses = cnf.clauses.size();

        std::size_t c_counter = 0;
        for (auto &&c : list) {
            std::vector<LiteralHandle> literals;
            literals.reserve(c.size());
            ++c_counter;

            for (auto &&l : c) {
                literals.emplace_back(l, 0);
                var_t var = (l > 0) ? l : -l;
                if ((std::size_t)var >= wch.watched_at.size()) {
                    assign.variables.resize(2 * var);
                    assign.antecedents.resize(2 * var);

                    wch.watched_at.resize(2 * var);
                }

                assign.unassigned.emplace(var);
            }

            auto clause = new (cnf.clauses.data() + c_counter) Clause<watch_sep_tag>(std::move(literals), wch);

            if (clause->size() == 1) {
                cnf.units.emplace_back(clause);
            } else if (clause->size() == 0) {
                exit(20);
            }
        }
    }

private:
    bool unit_propag(var_t d) {
        for (; !cnf.contra && !cnf.units.empty(); cnf.units.pop_back()) {
            auto &&clause = *cnf.units.back();
            if (clause.satisfied > 0)
                continue;

            ++derived;
            auto l = clause.get_watch();

            assert(l != 0);
            assert(assign.variables[std::abs(l)] == 0);

            if (l > 0)
                update(l, d, true, (std::size_t)(&clause - &cnf.clauses.front()));
            else
                update(-l, d, false, (std::size_t)(&clause - &cnf.clauses.front()));
        }

        return !cnf.contra;
    }

    void update(var_t variable, var_t d, bool is_true, lit_t antecedent) {
        assign.assigned.emplace_back(variable);
        assign.unassigned.erase(variable);
        assign.variables[variable] = is_true ? d : -d;
        assign.antecedents[variable] = antecedent;
        auto end = wch.watched_at[variable].end();
        for (auto it = wch.watched_at[variable].begin(); it != end; ++it) {
            auto clause = *it;

            if (clause->satisfied > 0)
                continue;

            clause->assure_watch(variable);
            bool sat = (clause->snd_watch() > 0) ^ !is_true;

            if (sat || clause->update(assign.variables, wch)) {
                assert(clause->satisfied == 0);
                cnf.satisfied.emplace_back(clause);
                clause->satisfied = d;
                continue;
            }

            if (clause->is_unit(assign.variables)) {
                cnf.units.emplace_back(clause);
            } else if (clause->is_empty(assign.variables)) {
                cnf.contra = true;
                assign.antecedents[0] = cnf.index(*clause); // FIXME? maybe **it
                assign.variables[0] = d; // contradiction is always true!
                break;
            }

            assert(clause->get_watch_var() != variable);
            if (clause->snd_watch_var() != variable) {
                --it;
                --end;
            }
        }
    }

    void delete_clauses() {
        delete_useless.delete_clauses(cnf, wch, original_clauses);

        for (auto it = cnf.clauses.begin() + 1; it != cnf.clauses.end(); ++it)
            if (it->is_unit(assign.variables))
                cnf.units.push_back(&*it);
    }

    bool solve(var_t d) {
        do {
            while (!unit_propag(d)) {
                if (d == 1)
                    return false;

                auto [new_clause, a] = clause1uip<watch_sep_tag>(assign.antecedents, assign.variables, cnf.clauses, assign.assigned);
                auto &&clause = cnf.learn(std::move(new_clause), wch);
                delete_useless.register_clause(assign.variables, clause);

                if (till_restart-- > 0) {
                    rollback(d = a);
                    cnf.units.push_back(&clause);
                } else {
                    // RESTART
                    till_restart = unit_run * *++luby;
                    rollback(d = 1);
                    delete_clauses();
                }
            }

            if (assign.unassigned.empty())
                return true;

            auto val = *assign.unassigned.begin();

            ++decided;
            update(val, ++d, rand() % 2 == 0, 0);
        } while (true);
    }

    void rollback(var_t d) {
        for (; !assign.assigned.empty(); assign.assigned.pop_back()) {
            auto &&var = assign.assigned.back();
            assert(var != 0);
            auto &&val = assign.variables[var];
            assert(val != 0);
            if ((var_t)std::abs(val) <= d)
                break;

            val = 0;
            assign.unassigned.emplace(var);
        }

        for (; !cnf.satisfied.empty(); cnf.satisfied.pop_back()) {
            auto &&var = cnf.satisfied.back();
            assert(var != nullptr);
            auto &&val = var->satisfied;
            assert(val != 0);
            if ((var_t)std::abs(val) <= d)
                break;

            val = 0;
        }

        cnf.units.clear();
        cnf.contra = false;
    }

    Cnf<watch_sep_tag> cnf;
    std::size_t original_clauses;
    WatchedSepList wch;
    luby_generator<int> luby;
    DeleteUseless delete_useless;
    int till_restart;
};

#endif // WATCHED_SEP_CDCL_HPP
