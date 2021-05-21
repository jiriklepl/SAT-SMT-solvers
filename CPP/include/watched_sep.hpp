#ifndef WATCHED_SEP_HPP
#define WATCHED_SEP_HPP

#include <array>
#include <cassert>
#include <vector>
#include <memory>

#include "shared.hpp"

struct WatchedSepList {
    std::vector<std::vector<Clause<watch_sep_tag>*>> watched_at;
};


struct LiteralHandle {
    constexpr LiteralHandle() noexcept = default;
    constexpr LiteralHandle(lit_t lit, var_t pos) noexcept
        : lit(lit), pos(pos)
    {}

    var_t var() const { return std::abs(lit); }

    lit_t lit;
    var_t pos;
};

template<>
class Clause<watch_sep_tag> {
    friend Cnf<watch_sep_tag>;

public:
    using value_t = LiteralHandle;

    Clause() noexcept = default;
    Clause(std::vector<LiteralHandle> &&_literals, WatchedSepList &watched_list) noexcept
        : literals(std::move(_literals)), w1(&literals.back()), w2(&literals.front()), satisfied(0), satisfied_at(0)
    {
        assert(literals.size() > 0);
        assert(literals.size() == 1 || w1 != w2);

        w1->pos = watched_list.watched_at[w1->var()].size();
        watched_list.watched_at[w1->var()].emplace_back(this);
        assert(watched_list.watched_at[w1->var()][w1->pos] == this);

        if (w2 == w1)
            return;

        w2->pos = watched_list.watched_at[w2->var()].size();
        watched_list.watched_at[w2->var()].emplace_back(this);
        assert(watched_list.watched_at[w2->var()][w2->pos] == this);
    }

    LiteralHandle *begin() noexcept { return &literals.front(); }
    const LiteralHandle *begin() const noexcept { return &literals.front(); }
    LiteralHandle *end() noexcept { return &*literals.end(); }
    const LiteralHandle *end() const noexcept { return &*literals.end(); }

    std::size_t size() const noexcept { return literals.size(); }

    void remove(LiteralHandle *lit_handle, WatchedSepList &watched_list) noexcept {

        var_t var  = lit_handle->var();

        auto &&var_item = watched_list.watched_at[var];
        auto &&this_ref = watched_list.watched_at[var][lit_handle->pos];

        assert(this_ref == this);

        if (&this_ref != &var_item.back()) {
            std::swap(this_ref, var_item.back());

            if(var == this_ref->w1->var()) {
                this_ref->w1->pos = lit_handle->pos;
            } else {
                assert(var == this_ref->w2->var());

                this_ref->w2->pos = lit_handle->pos;
            }

            assert(this_ref->size() == 1 || this_ref->w1 != this_ref->w2);
            assert(watched_list.watched_at[this_ref->w1->var()][this_ref->w1->pos] == this_ref);
            assert(watched_list.watched_at[this_ref->w2->var()][this_ref->w2->pos] == this_ref);
        }

        var_item.pop_back();
    }

    bool update(const std::vector<lit_t> &variables, WatchedSepList &watched_list) noexcept {
        if (variables[w1->var()] * w1->lit > 0)
            return true;

        assert(variables[w2->var()] * w2->lit < 0);

        auto tmp = w2;
        for (++w2;;++w2) {
            if (w2 == end()) w2 = begin();
            if (w2 == tmp) break;
            else if (variables[w2->var()] == 0) {
                if (w2 == w1)
                    continue;

                assert(variables[w1->var()] == 0);

                remove(tmp, watched_list);

                w2->pos = watched_list.watched_at[w2->var()].size();
                watched_list.watched_at[w2->var()].emplace_back(this);
                break;
            } else if (variables[w2->var()] * w2->lit > 0) {
                w2 = tmp;

                assert(size() == 1 || w1 != w2);
                assert(watched_list.watched_at[w1->var()][w1->pos] == this);
                assert(watched_list.watched_at[w2->var()][w2->pos] == this);
                return true;
            }
        }

        assert(size() == 1 || w1 != w2);
        assert(watched_list.watched_at[w1->var()][w1->pos] == this);
        assert(watched_list.watched_at[w2->var()][w2->pos] == this);
        assert((variables[w1->var()] == 0) || (variables[w2->var()] != 0));

        return false;
    }

    const lit_t &get_watch() const noexcept { return w1->lit; }
    var_t get_watch_var() const noexcept { return std::abs(w1->lit); }
    const lit_t &snd_watch() const noexcept { return w2->lit; }
    var_t snd_watch_var() const noexcept { return std::abs(w2->lit); }

    void assure_watch(var_t variable) noexcept {
        if (w2->var() != variable)
            std::swap(w1, w2);

        assert(w2->var() == variable);
    }

    bool is_unit(const std::vector<lit_t> &variables) const noexcept {
        return variables[w1->var()] == 0 && variables[w2->var()] != 0;
    }

    bool is_empty(const std::vector<lit_t> &variables) const noexcept {
        if (variables[w1->var()] != 0) {
            assert(variables[w2->var()] != 0);
            return true;
        }

        return false;
    }

private:
    std::vector<LiteralHandle> literals;
    LiteralHandle *w1, *w2;

public:
    lit_t satisfied, satisfied_at;
};

template<>
class Cnf<watch_sep_tag> {
public:
    std::size_t index(const Clause<watch_sep_tag> &clause) const {
        return &clause - &clauses.front();
    }

    Clause<watch_sep_tag> &learn(std::vector<LiteralHandle> &&literals, WatchedSepList &watched_list) {
        if (clauses.size() < clauses.capacity()) {
            return clauses.emplace_back(std::move(literals), watched_list);
        }

        auto orig = clauses.data();
        clauses.emplace_back();

        for (auto &&lit_watched : watched_list.watched_at) {
            for (auto &&clause : lit_watched)
                clause = clause - orig + clauses.data();
        }

        for (auto &&clause : satisfied)
            clause = clause - orig + clauses.data();

        return *new (&clauses.back()) Clause<watch_sep_tag>(std::move(literals), watched_list);
    }

    void unlearn(std::size_t index, WatchedSepList &watched_list) {
        Clause<watch_sep_tag> &clause = clauses[index];

        clause.remove(clause.w1, watched_list);

        if (clause.size() > 1) {
            assert(clause.w1 != clause.w2);

            clause.remove(clause.w2, watched_list);
        }

        if (&clause != &clauses.back()) {
            std::swap(clause, clauses.back());

            {
                auto var = clause.w1->var();

                assert(watched_list.watched_at[var][clause.w1->pos] == &clauses.back());

                watched_list.watched_at[var][clause.w1->pos] = &clause;
            }

            if (clause.size() > 1) {
                auto var = clause.w2->var();

                assert(watched_list.watched_at[var][clause.w2->pos] == &clauses.back());

                watched_list.watched_at[var][clause.w2->pos] = &clause;
            }

            if (clause.satisfied > 0) {
                assert(satisfied[clause.satisfied_at] == &clauses.back());

                satisfied[clause.satisfied_at] = &clause;
            }


            if (clauses.back().satisfied > 0) {
                assert(satisfied[clauses.back().satisfied_at] == &clause);

                auto &&entry = satisfied[clauses.back().satisfied_at];

                if (&entry != &satisfied.back()) {
                    assert((std::size_t)satisfied.back()->satisfied_at == satisfied.size() - 1);

                    satisfied.back()->satisfied_at = clauses.back().satisfied_at;

                    std::swap(entry, satisfied.back());

                    assert(satisfied[entry->satisfied_at] == entry);
                }

                satisfied.pop_back();
            }
        }

        clauses.pop_back();
    }

    std::vector<Clause<watch_sep_tag>*> units;
    std::vector<Clause<watch_sep_tag>*> satisfied;
    std::vector<Clause<watch_sep_tag>> clauses;
    bool contra;
};

class SolverWatchedSep : public Solver {
public:
    bool solve() override {
        return solve(1);
    }

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

                clause->satisfied = d;
                clause->satisfied_at = cnf.satisfied.size();
                cnf.satisfied.emplace_back(clause);

                continue;
            }

            if (clause->is_unit(assign.variables)) {
                cnf.units.emplace_back(clause);
            } else if (clause->is_empty(assign.variables)) {
                cnf.contra = true;

                assign.antecedents[0] = cnf.index(*clause);
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
        if (!unit_propag(d))
            return false;

        if (assign.unassigned.empty())
            return true;

        auto val = *assign.unassigned.begin();
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
    WatchedSepList wch;
};

#endif // WATCHED_SEP_HPP
