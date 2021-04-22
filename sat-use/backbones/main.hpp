#ifndef MAIN_HPP
#define MAIN_HPP

#include <cstdint>
#include <chrono>
#include <iostream>
#include <vector>

namespace {

enum suspicion : std::uint8_t {
    TRUE,
    FALSE,
    UNSUS
};

std::vector<suspicion> suspected;


void init(int argc, char *argv[]);
int run();

}

int main(int argc, char *argv[])
{
    init(argc, argv);

    auto start = std::chrono::high_resolution_clock::now();

    auto result = run();

    auto end = std::chrono::high_resolution_clock::now();

    auto difference = end - start;

    std::cout << 1000.L * decltype(difference)::period::num * difference.count() / decltype(difference)::period::den << " ms" << std::endl;

    if (result == 0) {
        std::cout << "No backbones. (after 1 call)" << std::endl;

        return 20;
    }

    std::size_t count = 0;

    for (auto it = suspected.begin() + 1; it != suspected.end(); ++it)
        if (*it != UNSUS)
            ++count;

    std::cout << count << " backbones. (after " << result << " calls)" << std::endl;

    for (std::size_t i = 1; i < suspected.size(); ++i) {
        if (suspected[i] == TRUE)
            std::cout << i << ": true" << std::endl;
        else if (suspected[i] == FALSE)
            std::cout << i << ": false" << std::endl;
    }

    return 10;
}

#endif // MAIN_HPP
