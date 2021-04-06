#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <memory>
#include <utility>


using lit_t = std::int32_t;

void gen_queens(std::ostream &out, lit_t n)
{

    std::vector<std::vector<lit_t>> cnf;
    // x_{i, j} = i * n + j + 1; i ~ row, j ~ col .. (both from 0 to n-1)
    auto x = [n](lit_t i, lit_t j) { return i * n + j + 1; };

    // at least one queen per row (this ensures the number of the queens)
    for (lit_t i = 0; i < n; ++i) {
        cnf.emplace_back();

        auto &&clause = cnf.back();
        for (lit_t j = 0; j < n; ++j) {
            clause.push_back(x(i, j));
        }
    }

    // at most one queen per row & col
    for (lit_t i = 0; i < n; ++i) {
        for (lit_t j = 0; j < n; ++j) {
            for (lit_t k = 0; k < j; ++k) {
                {
                    cnf.emplace_back();
                    auto &&clause = cnf.back();
                    clause.push_back(-x(i, k)); // first queen
                    clause.push_back(-x(i, j)); // second queen
                }

                {
                    cnf.emplace_back();
                    auto &&clause = cnf.back();
                    clause.push_back(-x(k, i)); // first queen
                    clause.push_back(-x(j, i)); // second queen
                }
            }

        }
    }

    // at most one queen per diag starting from col 0 & starting from row 0
    for (lit_t i = 0; i < n; ++i) {
        for (lit_t j = 0; j < n - i; ++j) {
            for (lit_t k = 0; k < j; ++k) {
                {
                    cnf.emplace_back();
                    auto &&clause = cnf.back();
                    clause.push_back(-x(i + k, k)); // first queen
                    clause.push_back(-x(i + j, j)); // second queen
                }

                if (i > 0) {
                    cnf.emplace_back();
                    auto &&clause = cnf.back();
                    clause.push_back(-x(k, i + k)); // first queen
                    clause.push_back(-x(j, i + j)); // second queen
                }
            }

        }
    }

    // the same, but for antidiags
    for (lit_t i = 0; i < n; ++i) {
        for (lit_t j = 0; j < n - i; ++j) {
            for (lit_t k = 0; k < j; ++k) {
                {
                    cnf.emplace_back();
                    auto &&clause = cnf.back();
                    clause.push_back(-x(n - i - 1 - k, k)); // first queen
                    clause.push_back(-x(n - i - 1 - j, j)); // second queen
                }

                if (i > 0) {
                    cnf.emplace_back();
                    auto &&clause = cnf.back();
                    clause.push_back(-x(n - k - 1, i + k)); // first queen
                    clause.push_back(-x(n - j - 1, i + j)); // second queen
                }
            }

        }
    }

    out << "p cnf " << n * n << ' ' << cnf.size() << std::endl;
    
    for (auto &&clause : cnf) {
        for (auto &&lit : clause)
            out << lit << ' ';

        out << "0\n";
    }

    out.flush();
}

int main(int argc, const char *argv[])
{
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " NUM" << std::endl;
        return 1;
    }

    std::istringstream ss(argv[1]);
    lit_t n;

    ss >> n;

    for (lit_t i = 1; i <= n; ++i) {
        std::ofstream file("out/queens" + std::to_string(i) + ".cnf");
        gen_queens(file, i);
    }
}