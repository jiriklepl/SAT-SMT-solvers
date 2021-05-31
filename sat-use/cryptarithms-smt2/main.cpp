#include <bitset>
#include <iostream>
#include <memory>
#include <string>
#include <string>
#include <sstream>
#include <vector>

class Expression;
class Negatable;
class Disjunction;
class Conjunction;
class Equation;
template<class> class NegExpression;
class Parser;

class Expression {
public:
    virtual ~Expression() noexcept = 0;
    virtual std::ostream &bounce_print(std::ostream &out, Parser &parser) const = 0;
};

Expression::~Expression() noexcept = default;

class Negatable : public Expression {
public:
    virtual std::unique_ptr<Negatable> negate() = 0;
};

template<class T, class E>
class WithBouncePrint : public E {
public:
    WithBouncePrint() = default;
    WithBouncePrint(E &&e) : E(std::move(e)) {}

private:
    std::ostream &bounce_print(std::ostream &out, Parser &parser) const override;
};

class Disjunction : public WithBouncePrint<Disjunction, Negatable> {
public:
    std::vector<Conjunction> conjunctions;

private:
    std::unique_ptr<Negatable> negate() override;
};

class Conjunction : public WithBouncePrint<Conjunction, Expression> {
public:
    std::vector<std::unique_ptr<Expression>> terms;
};

class Equation : public WithBouncePrint<Equation, Negatable> {
public:
    std::vector<std::string> pos, neg;
    std::string res;

private:
    std::unique_ptr<Negatable> negate() override;
};

template<typename E>
class NegExpression : public WithBouncePrint<NegExpression<E>, E> {
public:
    NegExpression(E &&e) : WithBouncePrint<NegExpression, E>(std::move(e)) { }

private:
    std::unique_ptr<Negatable> negate() override;
};

class Parser {
public:
    template<class T, class E>
    friend class WithBouncePrint;

    Parser(std::istream &in, int k, bool distinct)
        : first_chars(), chars(), in(in), k(k), peek(in.get()), line(1), distinct(distinct) { skip(); }

    bool too_much_chars() {
        int distinct_chars = 0;

        for (char i = 0; i++ < 127;) {
            if (chars[i])
                ++distinct_chars;
        }

        return distinct_chars > k;
    }

    void parse(std::ostream &out) {
        auto result = expression();

        struct _finally {
            ~_finally() {
                out << "(check-sat)\n(get-model)" << std::endl;
            }

            std::ostream &out;
        } finally{out};

        out << "(set-logic QF_UFLIA)\n";

        if (distinct && too_much_chars()) {
            out << "(assert false)\n";

            return;
        }

        for (char i = 0; i++ < 127;) {
            if (chars[i])
                out << "(declare-const " << i << " Int)\n";
        }

        for (char i = 0; i++ < 127;) {
            if (chars[i]) {
                if (!first_chars[i])
                    out << "(assert (>= " << i << " 0))\n";
                else
                    out << "(assert (>= " << i << " 1))\n";

                out << "(assert (<= " << i << " " << k - 1 << "))\n";
            }
        }

        if (distinct) {
            out << "(assert (distinct";

            for (char i = 0; i++ < 127;) {
                if (chars[i])
                    out << ' ' << i;
            }

            out << "))\n";
        }

        out << "(assert ";
        print(out, result) << ")\n";
    }

private:
    std::ostream &print(std::ostream &out, const Disjunction &disjunction) {
        if (disjunction.conjunctions.size() == 1)
            return print(out, disjunction.conjunctions[0]);

        out << "(or\n";

        for (auto &&conjunction : disjunction.conjunctions)
            print(out, conjunction);

        return out << ")\n";
    }

    std::ostream &print(std::ostream &out, const Conjunction &conjunction) {
        if (conjunction.terms.size() == 1)
            return print(out, conjunction.terms[0]);

        out << "(and\n";

        for (auto &&term : conjunction.terms)
            print(out, term);

        return out << ")\n";
    }

    std::ostream &print(std::ostream &out, const std::unique_ptr<Expression> &term) {
        return term->bounce_print(out, *this);
    }

    std::ostream &print(std::ostream &out, const std::string &word) {
        if (word.size() == 1)
            return out << word << "\n";

        out << "(+ " << word.back() << ' ';

        for (auto it = word.rbegin() + 1; it != word.rend(); ++it) {
            out << "(* " << k << " (+ " << *it << ' ';
        }

        for (auto i = 2 * word.size() - 1; i-- > 0;) {
            out << ')';
        }

        return out << '\n';
    }

    std::ostream &print(std::ostream &out, const std::vector<std::string> &pos) {
        if (pos.size() == 1)
            return print(out, pos[0]);

        out << "(+\n";

        for (auto &&pos_word : pos)
            print(out, pos_word);

        return out << ")\n";
    }

    std::ostream &print(std::ostream &out, const Equation &equation) {
        auto &&[pos, neg, res] = equation;

        out << "(= ";
        print(out, res);

        if (neg.size() >= 1) {
            out << "(-\n";

            print(out, pos);

            for (auto &&neg_word : neg)
                print(out, neg_word);

            out << ")\n";
        } else {
            print(out, pos);
        }

        return out << ")\n";
    }

    template<class E>
    std::ostream &print(std::ostream &out, const NegExpression<E> &neg_expression) {
        out << "(not\n";
        return print(out, static_cast<const E&>(neg_expression)) << ")\n";
    }

    Disjunction expression() {
        auto result = disjunction();

        if (peek == ')') {
            std::cerr << "Encountered dangling ')' at line " << line << std::endl;

            exit(1);
        }

        return result;
    }

    Disjunction disjunction() {
        Disjunction disjunction_result;
        auto &&[conjunctions] = disjunction_result;

        while (true) {
            conjunctions.emplace_back(conjunction());

            switch (peek) {
                case '|':
                    next();
                continue;

                case ')':
                break;

                default:
                    if (in.eof())
                        break;

                    std::cerr << "Unexpected character '" << (char)peek << "' at line " << line << std::endl;
                exit(1);
            }

            break;
        }

        return disjunction_result;
    }

    Conjunction conjunction() {
        Conjunction conjunction_return;
        auto &&[terms] = conjunction_return;

        while (true) {
            terms.emplace_back(term());

            switch (peek) {
                case '&':
                    peek = in.get();

                    if (peek != '&') {
                        std::cerr << "Expected '&&' at line " << line << ". Instread, encountered '&" << (char)peek << '\'' << std::endl;
                        exit(1);
                    }

                    next();

                continue;

                case '|':
                    peek = in.get();

                    if (peek != '|') {
                        std::cerr << "Expected '||' at line " << line << ". Instread, encountered '|" << (char)peek << '\'' << std::endl;
                        exit(1);
                    }
                break;

                case ')':
                break;

                default:
                    if (in.eof())
                        break;

                    std::cerr << "Unexpected character '" << (char)peek << "' at line " << line << std::endl;
                exit(1);
            }

            break;
        }

        return conjunction_return;
    }

    std::unique_ptr<Negatable> term() {
        if (peek == '(') {
            next();

            return std::make_unique<Disjunction>(par_expression());
        } else if (peek == '!') {
            next();

            return term()->negate();
        } else {
            return std::make_unique<Equation>(equation());
        }
    }

    Disjunction par_expression() {
        auto expr = disjunction();

        if (peek == ')') {
            next();
        } else {
            std::cerr << "Expected ')' at line " << line << std::endl;

            exit(1);
        }

        return expr;
    }

    Equation equation() {
        Equation equation_result;
        auto &&[pos, neg, res] = equation_result;

        pos.emplace_back(word());

        while (true) {
            switch (peek) {
                case '+':
                    next();
                    pos.emplace_back(word());
                break;

                case '-':
                    next();
                    neg.emplace_back(word());
                break;

                case '=':
                    next();
                    res = word();
                return equation_result;

                default:
                    std::cerr << "Expected either '+' or '-' or '=' at line " << line;
                exit(1);
            }
        }

    }

    std::string word() {
        std::string w;

        if ((peek | 32) < 'a' || (peek | 32) > 'z')
            goto invalid;

        if (!first_chars[peek])
            first_chars[peek] = true;

        while (true) {
            if (!chars[peek])
                chars[peek] = true;

            w.push_back(peek);
            peek = in.get();

            if ((peek | 32) >= 'a' && (peek | 32) <= 'z')
                continue;

            if (!std::isspace(peek) && !std::ispunct(peek)) {
                invalid:
                std::cerr << "Invalid word format at line " << line;

                exit(1);
            }

            break;
        }

        skip();

        return w;
    }

    void next() {
        peek = in.get();
        skip();
    }

    void skip() {
        for (;;peek = in.get()) {
            switch (peek) {
                case '\n': line++;
                case ' ':
                case '\t':
                case '\r':
                continue;

                default:
                return;
            }
        }
    }

private:
    std::bitset<256> first_chars;
    std::bitset<256> chars;
    std::istream &in;
    int k;
    int peek;
    int line;
    bool distinct;
};

template<class T, class E>
std::ostream &WithBouncePrint<T, E>::bounce_print(std::ostream &out, Parser &parser) const {
    return parser.print(out, static_cast<const T &>(*this));
}

std::unique_ptr<Negatable> Disjunction::negate()
{
    return std::make_unique<NegExpression<Disjunction>>(std::move(*this));
}

std::unique_ptr<Negatable> Equation::negate()
{
    return std::make_unique<NegExpression<Equation>>(std::move(*this));
}

template<class E>
std::unique_ptr<Negatable> NegExpression<E>::negate()
{
    return std::make_unique<E>(static_cast<E &&>(*this));
}

using namespace std::string_literals;

int main(int, char * argv[]) {
    int base = 10;
    bool distinct = true;
    for (char **arg = argv + 1; *arg != nullptr; ++arg) {
        if (*arg == "-d"s) {
            distinct = false;
        } else if (*arg == "-b"s) {
            if (*++arg == nullptr || (std::istringstream(*arg) >> base, base) == 0) {
                std::cerr << "bad arguments" << std::endl;
                exit(2);
            }
        } else {
            std::cerr << "bad arguments" << std::endl;
            exit(2);
        }
    }


    Parser parser(std::cin, base, distinct);
    parser.parse(std::cout);
}
