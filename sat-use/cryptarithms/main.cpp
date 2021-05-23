#include <bitset>
#include <iostream>
#include <memory>
#include <string>
#include <string>
#include <vector>

class Expression;
class Disjunction;
class Conjunction;
class Equation;

class Expression {
public:
    virtual ~Expression() noexcept = 0;
};

Expression::~Expression() noexcept = default;

class Disjunction : public Expression {
public:
    std::vector<Conjunction> conjunctions;
};

class Conjunction : public Expression {
public:
    std::vector<std::unique_ptr<Expression>> terms;
};

class Equation : public Expression {
public:
    std::vector<std::string> pos, neg;
    std::string res;
};

class Parser {
public:
    Parser(std::istream &in, int k, bool distinct)
        : first_chars(), chars(), in(in), k(k), peek(in.get()), line(1), distinct(distinct) { skip(); }

    void parse(std::ostream &out) {
        auto result = expression();

        out << R"((benchmark test.smt
:logic QF_UFLIA
:extrafuns ()";

        for (char i = 0; i++ < 127;) {
            if (chars[i])
                out << '(' << i << " Int)";
        }

        out << ")\n";
        out << ":formula (and\n";

        for (char i = 0; i++ < 127;) {
            if (chars[i]) {
                if (!first_chars[i])
                    out << "(>= " << i << " 0)\n";
                else
                    out << "(>= " << i << " 1)\n";

                out << "(<= " << i << " " << k << ")\n";
            }
        }

        if (distinct) {
            out << "(distinct";

            for (char i = 0; i++ < 127;) {
                if (chars[i])
                    out << ' ' << i;
            }

            out << ")\n";
        }

        print(out, result) << ")\n)" << std::endl;
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
        if (auto equation = dynamic_cast<const Equation *>(term.get()); equation)
            return print(out, *equation);
        else
            return print(out, *static_cast<const Disjunction *>(term.get()));
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

    std::unique_ptr<Expression> term() {
        if (peek == '(') {
            next();

            return std::make_unique<Disjunction>(par_expression());
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
                case '\r': [[likely]]
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

using namespace std::string_literals;

int main(int argc, char * argv[]) {
    int base = 10;
    bool distinct = false;

    if (argc > 1) {
        if (argv[1] == "-d"s)
            distinct = true;
    }


    Parser parser(std::cin, base, distinct);
    parser.parse(std::cout);
}
