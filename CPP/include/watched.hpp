#ifndef WATCHED_HPP
#define WATCHED_HPP

#include <cassert>
#include <vector>

#include "shared.hpp"

struct WatchedList {
    std::vector<std::vector<Clause<watch_tag>*>> watched_at;
};

template<>
class Clause<watch_tag> {
    friend Cnf<watch_tag>;
public:
    using value_t = std::pair<lit_t, std::size_t>;
    using WatchHandle = value_t*;

    static var_t get_var(WatchHandle w) { return std::abs(w->first); }

    static constexpr lit_t &get_lit(WatchHandle &w) { return w->first; }
    static constexpr const lit_t &get_lit(const WatchHandle &w) { return w->first; }

    static constexpr std::size_t &get_pos(WatchHandle &w) { return w->second; }
    static constexpr const std::size_t &get_pos(const WatchHandle &w) { return w->second; }

    Clause() noexcept = default;
    Clause(WatchHandle _start, WatchHandle _end, WatchedList &watched_list) noexcept
        : start(_start), after(_end), w1(_end - 1), w2(_start), satisfied(0)
    {
        assert(start != nullptr);
        assert(after != nullptr);
        assert(after - start == 1 || w1 != w2);

        w1->second = watched_list.watched_at[get_var(w1)].size();
        watched_list.watched_at[get_var(w1)].emplace_back(this);

        w2->second = watched_list.watched_at[get_var(w2)].size();
        watched_list.watched_at[get_var(w2)].emplace_back(this);

        assert(watched_list.watched_at[get_var(w1)][get_pos(w1)] == this);
        assert(watched_list.watched_at[get_var(w2)][get_pos(w2)] == this);
    }

    value_t *begin() { return start; }
    const value_t *begin() const { return start; }
    value_t *end() { return after; }
    const value_t *end() const { return after; }

    std::size_t size() const { return after - start; }

    void remove(std::pair<lit_t, std::size_t> *lit_handle, WatchedList &watched_list) noexcept {

        var_t var  = get_var(lit_handle);

        auto &&var_item = watched_list.watched_at[var];
        auto &&this_ref = watched_list.watched_at[var][get_pos(lit_handle)];

        assert(this_ref == this);

        if (&this_ref != &var_item.back()) {
            std::swap(this_ref, var_item.back());
            if(var == get_var(this_ref->w1)) {
                get_pos(this_ref->w1) = get_pos(lit_handle);
            } else {
                assert(var == get_var(this_ref->w2));
                get_pos(this_ref->w2) = get_pos(lit_handle);
            }

            assert(size() == 1 || this_ref->w1 != this_ref->w2);
            assert(watched_list.watched_at[get_var(this_ref->w1)][get_pos(this_ref->w1)] == this_ref);
            assert(watched_list.watched_at[get_var(this_ref->w2)][get_pos(this_ref->w2)] == this_ref);
        }

        var_item.pop_back();
    }

    bool update(const std::vector<lit_t> &variables, WatchedList &watched_list) noexcept {
        if (variables[get_var(w1)] * get_lit(w1) > 0)
            return true;

        assert(variables[get_var(w2)] * get_lit(w2) < 0);

        auto tmp = w2;
        for (++w2;;++w2) {
            if (w2 == after) w2 = start;
            if (w2 == tmp) break;
            else if (variables[get_var(w2)] == 0) {
                if (w2 == w1)
                    continue;

                assert(variables[get_var(w1)] == 0);

                remove(tmp, watched_list);

                get_pos(w2) = watched_list.watched_at[get_var(w2)].size();
                watched_list.watched_at[get_var(w2)].emplace_back(this);
                break;
            } else if (variables[get_var(w2)] * get_lit(w2) > 0) {
                w2 = tmp;

                assert(size() == 1 || w1 != w2);
                assert(watched_list.watched_at[get_var(w1)][get_pos(w1)] == this);
                assert(watched_list.watched_at[get_var(w2)][get_pos(w2)] == this);
                return true;
            }
        }

        assert(size() == 1 || w1 != w2);
        assert(watched_list.watched_at[get_var(w1)][get_pos(w1)] == this);
        assert(watched_list.watched_at[get_var(w2)][get_pos(w2)] == this);
        assert((variables[get_var(w1)] == 0) || (variables[get_var(w2)] != 0));

        return false;
    }

    const lit_t &get_watch() const noexcept { return get_lit(w1); }
    var_t get_watch_var() const noexcept { return std::abs(get_lit(w1)); }
    const lit_t &snd_watch() const noexcept { return get_lit(w2); }
    var_t snd_watch_var() const noexcept { return std::abs(get_lit(w2)); }

    void assure_watch(var_t variable) noexcept {
        if (get_var(w2) != variable)
            std::swap(w1, w2);
        assert(get_var(w2) == variable);
    }

    bool is_unit(const std::vector<lit_t> &variables) const noexcept {
        return variables[get_var(w1)] == 0 && variables[get_var(w2)] != 0;
    }

    bool is_empty(const std::vector<lit_t> &variables) const noexcept {
        if (variables[get_var(w1)] != 0) {
            assert(variables[get_var(w2)] != 0);
            return true;
        }
        return false;
    }

private:
    WatchHandle start, after, w1, w2;

public:
    lit_t satisfied;
};

template<>
class Cnf<watch_tag> {
public:
    std::size_t index(const Clause<watch_tag> &clause) const {
        return &clause - &clauses.front();
    }

    Clause<watch_tag> &learn(std::vector<Clause<watch_tag>::value_t> &&clause, WatchedList &watched_list) {
        std::size_t end = literals.end() - literals.begin();

        if (literals.size() + clause.size() > literals.capacity()) {
            auto orig = literals.data();

            for (auto &&[lit, _pos] : clause)
                literals.emplace_back(lit, 0);

            for (auto &&clause : clauses) {
                clause.start = clause.start - orig + literals.data();
                clause.after = clause.after - orig + literals.data();
                clause.w1 = clause.w1 - orig + literals.data();
                clause.w2 = clause.w2 - orig + literals.data();
            }
        } else {
            for (auto &&[lit, _pos] : clause)
                literals.emplace_back(lit, 0);
        }

        if (clauses.size() < clauses.capacity()) {
            return clauses.emplace_back(literals.data() + end, &*literals.end(), watched_list);
        }

        auto orig = clauses.data();
        clauses.emplace_back();

        for (auto &&lit_watched : watched_list.watched_at) {
            for (auto &&clause : lit_watched)
                clause = clause - orig + clauses.data();
        }

        for (auto &&clause : satisfied)
            clause = clause - orig + clauses.data();

        return *new (&clauses.back()) Clause<watch_tag>(literals.data() + end, &*literals.end(), watched_list);
    }

    std::vector<Clause<watch_tag>*> units;
    std::vector<Clause<watch_tag>*> satisfied;
    std::vector<Clause<watch_tag>> clauses;
    std::vector<std::pair<lit_t, std::size_t>> literals;
    bool contra;
};

class SolverWatched : public Solver {
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

            Clause<watch_tag> *clause =
                new (cnf.clauses.data() + c_counter)
                Clause<watch_tag>(cnf.literals.data() + clause_begin, cnf.literals.data() + l_counter, wch);

            if (clause_begin + 1 == l_counter)
                cnf.units.emplace_back(clause);
            else if (clause_begin == l_counter)
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

        var_t value;

        {
            auto val = assign.unassigned.begin();
            std::uniform_int_distribution<> distrib(0, assign.unassigned.size() - 1);

            for (auto i = distrib(rng); i--;)
                ++val;

            value = *val;
        }


        bool first;

        {
            std::uniform_int_distribution<> distrib(0, 1);
            
            first = distrib(rng) == 0;
        }

        ++decided;
        update(value, d + 1, first, 0);

        if (solve(d + 1))
            return true;
        else
            rollback(d);

        update(value, d + 1, !first, 0);

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

    Cnf<watch_tag> cnf;
    WatchedList wch;
};

#endif // WATCHED_HPP
