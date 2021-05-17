#ifndef SHARED_HPP
#define SHARED_HPP

#include <cassert>
#include <compare>
#include <cstdint>
#include <utility>
#include <vector>
#include <unordered_set>

using lit_t = std::int32_t;
using var_t = std::make_unsigned_t<lit_t>;

struct watch_tag;
struct watch_sep_tag;
struct adjacency_tag;

template<typename> class Clause;
template<typename> class Cnf;
struct Assignment;

struct Assignment {
    /* list of variable assignments
     * 0 = unassigned or free
     * +d = assigned true at decision lvl d
     * -d = assigned false at decision lvl d
     */
    std::vector<lit_t> variables; // variables[0] stands for the the contradiction
    std::vector<lit_t> antecedents; // antecedents[0] stands for the the contradiction

    // list of assigned variables
    std::vector<var_t> assigned;

    // set of unassigned variables
    std::unordered_set<var_t> unassigned;
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

    constexpr std::size_t size() const noexcept {
        return end() - begin();
    }

    constexpr bool is_unit() const noexcept {
        return size() == 1;
    }

    constexpr bool is_empty() const noexcept {
        return size() == 0;
    }

private:
    lit_t* start_;
    lit_t* end_;
};

class Solver {
public:
    Solver() : derived(0), decided(0) {}
    virtual ~Solver() = 0;

    virtual bool solve() = 0;
    virtual void set(std::vector<std::vector<lit_t>> &list) = 0;

    Assignment assign;
    std::size_t derived;
    std::size_t decided;
};

inline Solver::~Solver() {}


/**
 * @brief
 *
 * @tparam tag
 * @param antecedents
 * @param variables
 * @param clauses
 * @param assigned
 * @return the returned vector is the clause to learn, the last element is the variable with the highest decision level and the penultimate one is the literal with the second highest decision level
 */
template<typename tag>
inline auto clause1uip(const decltype(Assignment::antecedents) &antecedents, const decltype(Assignment::variables) &variables, const decltype(Cnf<tag>::clauses) &clauses, const decltype(Assignment::assigned) &assigned) {
    using value_t = typename Clause<tag>::value_t;

    const var_t d = variables[0]; // always positive

    std::pair<std::vector<value_t>, var_t> return_v = {{}, 0};
    auto &&[others, level] = return_v;
    // others: contains all literals l'@d' s.t. d' < d

    std::size_t level_idx = 0;

    std::vector<bool> added(variables.size(), false);
    std::size_t last = 0;

    // TOOO: relearns known clauses

    {
        auto &&clause = clauses[antecedents[0]];

        for (auto &&[lit, _pos] : clause) {
            var_t var = std::abs(lit);
            var_t other_d = std::abs(variables[var]);
            added[var] = true;

            assert(other_d <= d);

            if (other_d == d) {
                ++last;
            } else {
                if (other_d > level) {
                    level = other_d;
                    level_idx = others.size();
                }

                others.emplace_back(lit, 0);
            }
        }
    }

    auto pivot = assigned.rbegin();

    for (;;++pivot) {
        while (!added[*pivot])
            ++pivot;

        assert(last >= 1);
        if (last == 1)
            break;

        var_t var = *pivot;
        auto ante = antecedents[var];
        --last;

        assert(ante != 0);

        auto &&clause = clauses[ante];

        for (auto &&[lit, _pos] : clause) {
            var_t other_var = std::abs(lit);
            var_t other_d = std::abs(variables[other_var]);

            if (added[other_var])
                continue;

            added[other_var] = true;

            assert(other_d <= d);

            if (other_d == d) {
                ++last;
            } else {
                if (other_d > level) {
                    level = other_d;
                    level_idx = others.size();
                }

                others.emplace_back(lit, 0);
            }
        }
    }

    assert(last == 1);
    assert(added[*pivot]);

    others.emplace_back(variables[*pivot] > 0 ? -*pivot : *pivot, 0);

    if (others.size() == 1)
        level = 1;

    if (level_idx > 0)
        std::swap(others[level_idx], others[0]);

    return return_v;
}

// http://oeis.org/A182105
template<typename Int>
class luby_generator {
public:
    constexpr luby_generator() noexcept : u(1), v(1) {}
    Int& operator*() noexcept { return v; }
    const Int& operator*() const noexcept { return v; }
    luby_generator &operator++() noexcept {
        if ((u & -u) == v)
            u += v = 1;
        else
            v <<= 1;

        return *this;
    }
private:
    Int u, v;
};

#endif // SHARED_HPP
