#ifndef WATCHED_CDCL_HPP
#define WATCHED_CDCL_HPP

#include "watched.hpp"

class SolverWatchedCDCL : public Solver {
public:
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

        cnf.literals.resize(total_size);
        cnf.clauses.resize(list.size() + 1);

        std::size_t l_counter = 0;
        std::size_t c_counter = 0;
        for (auto &&c : list) {
            std::size_t clause_begin = l_counter;
            ++c_counter;

            for (auto &&l : c) {
                cnf.literals[l_counter++] = {l, 0};
                var_t var = (l > 0) ? l : -l;
                if ((std::size_t)var >= wch.watched_at.size()) {
                    assign.variables.resize(2 * var);
                    assign.antecedents.resize(2 * var);

                    wch.watched_at.resize(2 * var);
                }

                assign.unassigned.emplace(var);
            }

            new (cnf.clauses.data() + c_counter) Clause<watch_tag>(cnf.literals.data() + clause_begin, cnf.literals.data() + l_counter, wch);

            if (clause_begin == l_counter)
                exit(20);
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

    bool solve(var_t d) {
        do {
            while (!unit_propag(d)) {
                if (d == 1)
                    return false;
                auto [new_clause, a] = clause1uip<watch_tag>(assign.antecedents, assign.variables, cnf.clauses, assign.assigned);

                auto &&clause = cnf.learn(new_clause, wch);
                rollback(d = a);
                cnf.units.push_back(&clause);
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

    Cnf<watch_tag> cnf;
    WatchedList wch;
};

#endif // WATCHED_CDCL_HPP
