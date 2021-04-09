#include <memory>
#include <utility>

class Formula {
public:
    virtual ~Formula() noexcept = 0;
};

inline Formula::~Formula() noexcept = default;

class AndFormula : public Formula {
public:
    ~AndFormula() noexcept override = default;
    AndFormula(std::unique_ptr<Formula> left, std::unique_ptr<Formula> right) :
        left(std::move(left)), right(std::move(right)) {}
    std::unique_ptr<Formula> left, right;
};

class OrFormula : public Formula {
public:
    ~OrFormula() noexcept override = default;
    OrFormula(std::unique_ptr<Formula> left, std::unique_ptr<Formula> right) :
        left(std::move(left)), right(std::move(right)) {}
    std::unique_ptr<Formula> left, right;
};

class NotFormula : public Formula {
public:
    ~NotFormula() noexcept override = default;
    NotFormula(std::string value) : inner(value) {}
    std::string inner;
};

class PosFormula : public Formula {
public:
    ~PosFormula() noexcept override = default;
    PosFormula(std::string value) : inner(value) {}
    std::string inner;
};

enum Token {
    FORMULA,
    AND,
    OR,
    NOT,
    LPAR,
    RPAR,
    VAR
};
