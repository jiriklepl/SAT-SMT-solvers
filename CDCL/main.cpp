#include <vector>
#include <cassert>
#include <iostream>
#include <unordered_set>
#include <cstdint>

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
    void update(std::size_t variable, bool is_true) {
        if (is_true) {
            for (auto &&a : adj.adjacency[variable]) {
                if (a > 0) {
                    auto &&clause = cnf.clauses[a];
                    clause.remove(clause.find(variable));
                    for (auto &&l : clause) {
                        if (l > 0) {
                            std::size_t n = adj.adjacency[l].erase(a);
                            assert(n == 1);
                        } else {
                            std::size_t n = adj.adjacency[-l].erase(-a);
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
                            std::size_t n = adj.adjacency[l].erase(-a);
                            assert(n == 1);
                        } else {
                            std::size_t n = adj.adjacency[-l].erase(a);
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

    bool unit_propag(std::size_t d) {
        for (;!cnf.units.empty();) {
            Clause<nowatch_tag> *clause = cnf.units.back();
            cnf.units.pop_back();
            auto l = *clause->begin();
            if (clause->is_empty())
                continue;

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

    void set(std::initializer_list<std::initializer_list<lit_t>> list) {
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

                auto new_it = adj.adjacency[var].emplace((l > 0) ? c_counter : -c_counter);
                auto it = adj.adjacency[var].find((l < 0) ? c_counter : -c_counter);
                if (it != adj.adjacency[var].end()) {
                    adj.adjacency[var].erase(it);
                    adj.adjacency[var].erase(new_it.first);

                    --c_counter;
                    l_counter  = clause_begin;
                    goto skip_clause;
                }
            }

            cnf.clauses[c_counter] = Clause<nowatch_tag>(cnf.literals.data() + clause_begin, cnf.literals.data() + l_counter);
            skip_clause:;
        }
    }
};

int main()
{
    Solver<dpll_tag> solver;

//     solver.set({
//     { -199 },
//     { -199, -198 },
//     { -199, -1 },
//     { -198, 2 },
//     { -198, 197 },
//     { -197, 3 },
//     { -197, 196 },
//     { -196, 4 },
//     { -196, 195 },
//     { -195, 5 },
//     { -195, 194 },
//     { -194, 6 },
//     { -194, 193 },
//     { -193, 7 },
//     { -193, 192 },
//     { -192, 8 },
//     { -192, 191 },
//     { -191, 9 },
//     { -191, 190 },
//     { -190, 10 },
//     { -190, 189 },
//     { -189, 11 },
//     { -189, 188 },
//     { -188, 12 },
//     { -188, 187 },
//     { -187, 13 },
//     { -187, 186 },
//     { -186, 14 },
//     { -186, 185 },
//     { -185, 15 },
//     { -185, 184 },
//     { -184, 16 },
//     { -184, 183 },
//     { -183, 17 },
//     { -183, 182 },
//     { -182, 18 },
//     { -182, 181 },
//     { -181, 19 },
//     { -181, 180 },
//     { -180, 20 },
//     { -180, 179 },
//     { -179, 21 },
//     { -179, 178 },
//     { -178, 22 },
//     { -178, 177 },
//     { -177, 23 },
//     { -177, 176 },
//     { -176, 24 },
//     { -176, 175 },
//     { -175, 25 },
//     { -175, 174 },
//     { -174, 26 },
//     { -174, 173 },
//     { -173, 27 },
//     { -173, 172 },
//     { -172, 28 },
//     { -172, 171 },
//     { -171, 29 },
//     { -171, 170 },
//     { -170, 30 },
//     { -170, 169 },
//     { -169, 31 },
//     { -169, 168 },
//     { -168, 32 },
//     { -168, 167 },
//     { -167, 33 },
//     { -167, 166 },
//     { -166, 34 },
//     { -166, 165 },
//     { -165, 35 },
//     { -165, 164 },
//     { -164, 36 },
//     { -164, 163 },
//     { -163, 37 },
//     { -163, 162 },
//     { -162, 38 },
//     { -162, 161 },
//     { -161, 39 },
//     { -161, 160 },
//     { -160, 40 },
//     { -160, 159 },
//     { -159, 41 },
//     { -159, 158 },
//     { -158, 42 },
//     { -158, 157 },
//     { -157, 43 },
//     { -157, 156 },
//     { -156, 44 },
//     { -156, 155 },
//     { -155, 45 },
//     { -155, 154 },
//     { -154, 46 },
//     { -154, 153 },
//     { -153, 47 },
//     { -153, 152 },
//     { -152, 48 },
//     { -152, 151 },
//     { -151, 49 },
//     { -151, 150 },
//     { -150, 50 },
//     { -150, 149 },
//     { -149, 51 },
//     { -149, 148 },
//     { -148, 52 },
//     { -148, 147 },
//     { -147, 53 },
//     { -147, 146 },
//     { -146, 54 },
//     { -146, 145 },
//     { -145, 55 },
//     { -145, 144 },
//     { -144, 56 },
//     { -144, 143 },
//     { -143, 57 },
//     { -143, 142 },
//     { -142, 58 },
//     { -142, 141 },
//     { -141, 59 },
//     { -141, 140 },
//     { -140, 60 },
//     { -140, 139 },
//     { -139, 61 },
//     { -139, 138 },
//     { -138, 62 },
//     { -138, 137 },
//     { -137, 63 },
//     { -137, 136 },
//     { -136, 64 },
//     { -136, 135 },
//     { -135, 65 },
//     { -135, 134 },
//     { -134, 66 },
//     { -134, 133 },
//     { -133, 67 },
//     { -133, 132 },
//     { -132, 68 },
//     { -132, 131 },
//     { -131, 69 },
//     { -131, 130 },
//     { -130, 70 },
//     { -130, 129 },
//     { -129, 71 },
//     { -129, 128 },
//     { -128, 72 },
//     { -128, 127 },
//     { -127, 73 },
//     { -127, 126 },
//     { -126, 74 },
//     { -126, 125 },
//     { -125, 75 },
//     { -125, 124 },
//     { -124, 76 },
//     { -124, 123 },
//     { -123, 77 },
//     { -123, 122 },
//     { -122, 78 },
//     { -122, 121 },
//     { -121, 79 },
//     { -121, 120 },
//     { -120, 80 },
//     { -120, 119 },
//     { -119, 81 },
//     { -119, 118 },
//     { -118, 82 },
//     { -118, 117 },
//     { -117, 83 },
//     { -117, 116 },
//     { -116, 84 },
//     { -116, 115 },
//     { -115, 85 },
//     { -115, 114 },
//     { -114, 86 },
//     { -114, 113 },
//     { -113, 87 },
//     { -113, 112 },
//     { -112, 88 },
//     { -112, 111 },
//     { -111, 89 },
//     { -111, 110 },
//     { -110, 90 },
//     { -110, 109 },
//     { -109, 91 },
//     { -109, 108 },
//     { -108, 92 },
//     { -108, 107 },
//     { -107, 93 },
//     { -107, 106 },
//     { -106, 94 },
//     { -106, 105 },
//     { -105, 95 },
//     { -105, 104 },
//     { -104, 96 },
//     { -104, 103 },
//     { -103, 97 },
//     { -103, 102 },
//     { -102, 98 },
//     { -102, 101 },
//     { -101, 99 },
//     { -101, 100 },
//     { -197, -2, 198 },
//     { -196, -3, 197 },
//     { -195, -4, 196 },
//     { -194, -5, 195 },
//     { -193, -6, 194 },
//     { -192, -7, 193 },
//     { -191, -8, 192 },
//     { -190, -9, 191 },
//     { -189, -10, 190 },
//     { -188, -11, 189 },
//     { -187, -12, 188 },
//     { -186, -13, 187 },
//     { -185, -14, 186 },
//     { -184, -15, 185 },
//     { -183, -16, 184 },
//     { -182, -17, 183 },
//     { -181, -18, 182 },
//     { -180, -19, 181 },
//     { -179, -20, 180 },
//     { -178, -21, 179 },
//     { -177, -22, 178 },
//     { -176, -23, 177 },
//     { -175, -24, 176 },
//     { -174, -25, 175 },
//     { -173, -26, 174 },
//     { -172, -27, 173 },
//     { -171, -28, 172 },
//     { -170, -29, 171 },
//     { -169, -30, 170 },
//     { -168, -31, 169 },
//     { -167, -32, 168 },
//     { -166, -33, 167 },
//     { -165, -34, 166 },
//     { -164, -35, 165 },
//     { -163, -36, 164 },
//     { -162, -37, 163 },
//     { -161, -38, 162 },
//     { -160, -39, 161 },
//     { -159, -40, 160 },
//     { -158, -41, 159 },
//     { -157, -42, 158 },
//     { -156, -43, 157 },
//     { -155, -44, 156 },
//     { -154, -45, 155 },
//     { -153, -46, 154 },
//     { -152, -47, 153 },
//     { -151, -48, 152 },
//     { -150, -49, 151 },
//     { -149, -50, 150 },
//     { -148, -51, 149 },
//     { -147, -52, 148 },
//     { -146, -53, 147 },
//     { -145, -54, 146 },
//     { -144, -55, 145 },
//     { -143, -56, 144 },
//     { -142, -57, 143 },
//     { -141, -58, 142 },
//     { -140, -59, 141 },
//     { -139, -60, 140 },
//     { -138, -61, 139 },
//     { -137, -62, 138 },
//     { -136, -63, 137 },
//     { -135, -64, 136 },
//     { -134, -65, 135 },
//     { -133, -66, 134 },
//     { -132, -67, 133 },
//     { -131, -68, 132 },
//     { -130, -69, 131 },
//     { -129, -70, 130 },
//     { -128, -71, 129 },
//     { -127, -72, 128 },
//     { -126, -73, 127 },
//     { -125, -74, 126 },
//     { -124, -75, 125 },
//     { -123, -76, 124 },
//     { -122, -77, 123 },
//     { -121, -78, 122 },
//     { -120, -79, 121 },
//     { -119, -80, 120 },
//     { -118, -81, 119 },
//     { -117, -82, 118 },
//     { -116, -83, 117 },
//     { -115, -84, 116 },
//     { -114, -85, 115 },
//     { -113, -86, 114 },
//     { -112, -87, 113 },
//     { -111, -88, 112 },
//     { -110, -89, 111 },
//     { -109, -90, 110 },
//     { -108, -91, 109 },
//     { -107, -92, 108 },
//     { -106, -93, 107 },
//     { -105, -94, 106 },
//     { -104, -95, 105 },
//     { -103, -96, 104 },
//     { -102, -97, 103 },
//     { -101, -98, 102 },
//     { -100, -99, 101 },
//     { 1, 198, 199 }
// });



    volatile auto x = 1;
    volatile auto y = x;
    solver.set({
    {130},
    {-130, -129},
    {-130, -70},
    {-129, -128},
    {-129, -97},
    {-128, -127},
    {-128, -112},
    {-127, -126},
    {-127, -119},
    {-126, -125},
    {-126, -122},
    {-125, -124},
    {-125, -123},
    {-124, 8},
    {-124, 23},
    {-123, 1},
    {-123, 4},
    {-122, -121},
    {-122, -120},
    {-121, -5},
    {-121, 1},
    {-120, 1},
    {-120, 17},
    {-119, -118},
    {-119, -115},
    {-118, -117},
    {-118, -116},
    {-117, -4},
    {-117, 14},
    {-116, -91},
    {-116, -8},
    {-115, -114},
    {-115, -113},
    {-114, 8},
    {-113, -2},
    {-113, -1},
    {-112, -111},
    {-112, -104},
    {-111, -110},
    {-111, -107},
    {-110, -109},
    {-110, -108},
    {-109, -91},
    {-109, 4},
    {-108, 1},
    {-108, 23},
    {-107, -106},
    {-107, -105},
    {-106, -5},
    {-106, 23},
    {-105, -91},
    {-105, -4},
    {-104, -103},
    {-104, -100},
    {-103, -102},
    {-103, -101},
    {-102, -4},
    {-102, 2},
    {-101, -17},
    {-101, 2},
    {-100, -99},
    {-100, -98},
    {-99, -1},
    {-99, 1},
    {-98, -17},
    {-98, -8},
    {-97, -96},
    {-97, -83},
    {-96, -95},
    {-96, -88},
    {-95, -94},
    {-95, -90},
    {-94, -93},
    {-94, -92},
    {-93, -2},
    {-93, 23},
    {-92, 91},
    {-91, 92},
    {-90, -89},
    {-90, -79},
    {-89, -23},
    {-89, 1},
    {-88, -87},
    {-88, -85},
    {-87, -86},
    {-87, -31},
    {-86, 2},
    {-86, 5},
    {-85, -84},
    {-85, -44},
    {-84, -23},
    {-84, -5},
    {-83, -82},
    {-83, -76},
    {-82, -81},
    {-82, -78},
    {-81, -80},
    {-81, -79},
    {-80, -17},
    {-80, 14},
    {-79, -2},
    {-79, 14},
    {-78, -77},
    {-78, -24},
    {-77, -5},
    {-77, 4},
    {-76, -75},
    {-76, -72},
    {-75, -74},
    {-75, -73},
    {-74, -2},
    {-74, 2},
    {-73, -1},
    {-73, 17},
    {-72, -71},
    {-72, -19},
    {-71, 4},
    {-71, 14},
    {-70, -69},
    {-70, -39},
    {-69, -68},
    {-69, -54},
    {-68, -67},
    {-68, -60},
    {-67, -66},
    {-67, -63},
    {-66, -65},
    {-66, -64},
    {-65, -5},
    {-65, 5},
    {-64, -4},
    {-64, -1},
    {-63, -62},
    {-63, -61},
    {-62, -17},
    {-62, 14},
    {-61, -5},
    {-61, 23},
    {-60, -59},
    {-60, -56},
    {-59, -58},
    {-59, -57},
    {-58, -8},
    {-58, -5},
    {-57, -23},
    {-57, 4},
    {-56, -55},
    {-56, -32},
    {-55, -17},
    {-55, 4},
    {-54, -53},
    {-54, -46},
    {-53, -52},
    {-53, -49},
    {-52, -51},
    {-52, -50},
    {-51, -8},
    {-51, 5},
    {-50, -4},
    {-49, -48},
    {-49, -47},
    {-48, -8},
    {-48, -2},
    {-47, -14},
    {-47, 2},
    {-46, -45},
    {-46, -42},
    {-45, -44},
    {-45, -43},
    {-44, 2},
    {-44, 17},
    {-43, -5},
    {-43, 17},
    {-42, -41},
    {-42, -40},
    {-41, -8},
    {-41, 4},
    {-40, -23},
    {-40, 5},
    {-39, -38},
    {-39, -22},
    {-38, -37},
    {-38, -30},
    {-37, -36},
    {-37, -33},
    {-36, -35},
    {-36, -34},
    {-35, -2},
    {-35, 17},
    {-34, -17},
    {-34, -14},
    {-33, -32},
    {-33, -31},
    {-32, -23},
    {-32, 23},
    {-31, 2},
    {-31, 23},
    {-30, -29},
    {-30, -26},
    {-29, -28},
    {-29, -27},
    {-28, -17},
    {-28, 5},
    {-27, -5},
    {-27, 8},
    {-26, -25},
    {-26, -24},
    {-25, -17},
    {-25, -1},
    {-24, -23},
    {-24, 23},
    {-22, -21},
    {-22, -12},
    {-21, -20},
    {-21, -16},
    {-20, -19},
    {-20, -18},
    {-19, -4},
    {-19, 17},
    {-18, -17},
    {-18, 4},
    {-16, -15},
    {-16, -13},
    {-15, -14},
    {-15, -2},
    {-13, 5},
    {-12, -11},
    {-12, -7},
    {-11, -10},
    {-11, -9},
    {-10, 2},
    {-10, 5},
    {-9, 1},
    {-9, 8},
    {-8, 114},
    {-7, -6},
    {-7, -3},
    {-6, 4},
    {-6, 5},
    {-5, 13},
    {-3, -2},
    {-3, 1},
    {4, 50},
    {-23, -8, 124},
    {-23, -2, 31},
    {-23, -1, 108},
    {-23, 2, 93},
    {-23, 5, 61},
    {-23, 5, 106},
    {-23, 23, 24},
    {-23, 23, 32},
    {-17, -2, 44},
    {-17, -1, 120},
    {-17, 1, 73},
    {-17, 2, 35},
    {-17, 4, 19},
    {-17, 5, 43},
    {-14, -4, 71},
    {-14, 2, 79},
    {-14, 4, 117},
    {-14, 17, 62},
    {-14, 17, 80},
    {-8, -1, 9},
    {-8, 5, 27},
    {-5, -4, 6},
    {-5, -2, 10},
    {-5, -2, 86},
    {-5, 5, 65},
    {-5, 8, 51},
    {-5, 17, 28},
    {-5, 23, 40},
    {-4, -1, 123},
    {-4, 5, 77},
    {-4, 8, 41},
    {-4, 17, 18},
    {-4, 17, 55},
    {-4, 23, 57},
    {-4, 91, 109},
    {-2, 2, 74},
    {-2, 4, 102},
    {-2, 14, 47},
    {-2, 17, 101},
    {-1, 1, 99},
    {-1, 2, 3},
    {-1, 5, 121},
    {-1, 23, 89},
    {1, 2, 113},
    {1, 4, 64},
    {1, 17, 25},
    {2, 8, 48},
    {2, 14, 15},
    {3, 6, 7},
    {4, 91, 105},
    {5, 8, 58},
    {5, 23, 84},
    {7, 11, 12},
    {8, 17, 98},
    {8, 91, 116},
    {9, 10, 11},
    {12, 21, 22},
    {13, 15, 16},
    {14, 17, 34},
    {16, 20, 21},
    {18, 19, 20},
    {19, 71, 72},
    {22, 38, 39},
    {24, 25, 26},
    {24, 77, 78},
    {26, 29, 30},
    {27, 28, 29},
    {30, 37, 38},
    {31, 32, 33},
    {31, 86, 87},
    {32, 55, 56},
    {33, 36, 37},
    {34, 35, 36},
    {39, 69, 70},
    {40, 41, 42},
    {42, 45, 46},
    {43, 44, 45},
    {44, 84, 85},
    {46, 53, 54},
    {47, 48, 49},
    {49, 52, 53},
    {50, 51, 52},
    {54, 68, 69},
    {56, 59, 60},
    {57, 58, 59},
    {60, 67, 68},
    {61, 62, 63},
    {63, 66, 67},
    {64, 65, 66},
    {70, 129, 130},
    {72, 75, 76},
    {73, 74, 75},
    {76, 82, 83},
    {78, 81, 82},
    {79, 80, 81},
    {79, 89, 90},
    {83, 96, 97},
    {85, 87, 88},
    {88, 95, 96},
    {90, 94, 95},
    {92, 93, 94},
    {97, 128, 129},
    {98, 99, 100},
    {100, 103, 104},
    {101, 102, 103},
    {104, 111, 112},
    {105, 106, 107},
    {107, 110, 111},
    {108, 109, 110},
    {112, 127, 128},
    {113, 114, 115},
    {115, 118, 119},
    {116, 117, 118},
    {119, 126, 127},
    {120, 121, 122},
    {122, 125, 126},
    {123, 124, 125}
});

    std::cout << (solver.solve(0) ? "true" : "false") << std::endl;

    for (auto &&a : solver.assign.assigned) {
        std::cout << a << ": " << (solver.assign.variables[a] > 0 ?  "true" : solver.assign.variables[a] < 0 ? "false" : "free") << std::endl;
    }
}