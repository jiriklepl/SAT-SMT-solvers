#ifndef SHARED_HPP
#define SHARED_HPP

#include <cassert>
#include <cstdint>
#include <utility>
#include <vector>
#include <unordered_set>

using lit_t = std::int32_t;

struct watch_tag;
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
    std::vector<std::size_t> assigned;

    // set of unassigned variables
    std::unordered_set<std::size_t> unassigned;
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

#endif // SHARED_HPP
