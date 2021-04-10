%{
/* need this for the call to atof() below */
#include <vector>
#include <cassert>
#include "satlib.hpp"
#define YY_DECL int yyFlexLexer::yylex()

static std::string yylval;

enum Token {
    TOK_FORMULA,
    TOK_AND,
    TOK_OR,
    TOK_NOT,
    TOK_LPAR,
    TOK_RPAR,
    TOK_VAR,
    TOK_EOF
};

%}


%option c++
%option noyywrap
%option prefix="satlib_"
%option noyywrap nounput batch noinput stack
%option never-interactive
%option nounistd

ID          [a-zA-Z][a-zA-Z0-9]*

%%

"("         { return TOK_LPAR; }
")"         { return TOK_RPAR; }
"not"       { return TOK_NOT; }
"or"        { return TOK_OR; }
"and"       { return TOK_AND; }

{ID}        { yylval = yytext; return TOK_VAR; }

[ \t\n\r]+  /* eat up whitespace */

<<EOF>>      { return TOK_EOF; }

.           { std::cerr << "unrecognized character: " << yytext << '\n'; }

%%

class Context : public satlib_parser {
public:
    Context(std::istream &in) :
        scanner(&in), parsing{TOK_FORMULA}, values() { }

    ~Context() noexcept override = default;

    lit_t get_name(std::string &variable) {
        auto [it, is_new] = names.try_emplace(variable, var_counter + 1);

        if (is_new)
            ++var_counter;

        return it->second;
    }

    void parse_variable() {
        int token = scanner.yylex();
        if (token == TOK_VAR) {
            values.push_back(get_name(yylval));
        } else {
            std::cerr << "expected a variable" << std::endl;
            exit(1);
        }
    }

    const char* translate(int what) {
        switch (what) {
            case TOK_FORMULA: return "`formula'";
            case TOK_AND: return "`and'";
            case TOK_OR: return "`or'";
            case TOK_NOT: return "`not'";
            case TOK_LPAR: return "'('";
            case TOK_RPAR: return "')'";
            case TOK_VAR: return yylval.c_str();
            default: return "unrecognized token";
        }
    }

    void parse_formula() {
        int token = scanner.yylex();
        if (token == TOK_LPAR) {
            token = scanner.yylex();
            switch (token) {
                case TOK_AND:
                    parsing.push_back(TOK_RPAR);
                    parsing.push_back(TOK_AND);
                    parsing.push_back(TOK_FORMULA);
                    parsing.push_back(TOK_FORMULA);
                break;
                case TOK_OR:
                    parsing.push_back(TOK_RPAR);
                    parsing.push_back(TOK_OR);
                    parsing.push_back(TOK_FORMULA);
                    parsing.push_back(TOK_FORMULA);
                break;
                case TOK_NOT:
                    parsing.push_back(TOK_RPAR);
                    parsing.push_back(TOK_NOT);
                    parsing.push_back(TOK_VAR);
                break;
                default:
                    std::cerr << "expected either `and', `or', or `not'; instead: " << translate(token) << std::endl;
                    exit(1);
                break;
            }
        } else if (token == TOK_VAR) {
            values.push_back(get_name(yylval));
        } else {
            std::cerr << "expected a formula; instead: " << translate(token) << std::endl;
            exit(1);
        }
    }
    void parse()
    {
        while (!parsing.empty()) {
            auto goal = parsing.back();
            parsing.pop_back();
            switch (goal) {
                case TOK_FORMULA:
                    parse_formula();
                break;
                case TOK_VAR:
                    parse_variable();
                break;
                case TOK_NOT:
                {
                    auto var = values.back();
                    values.pop_back();
                    values.push_back(-var);
                }
                break;
                case TOK_AND:
                {
                    auto left = values.back();
                    values.pop_back();
                    auto right = values.back();
                    values.pop_back();
                    values.push_back(++var_counter);

                    cnf.push_back({-left,-right,var_counter});
                    cnf.push_back({-var_counter, left});
                    cnf.push_back({-var_counter, right});
                }
                break;
                case TOK_OR:
                {
                    auto left = values.back();
                    values.pop_back();
                    auto right = values.back();
                    values.pop_back();
                    values.push_back(++var_counter);

                    cnf.push_back({-left,var_counter});
                    cnf.push_back({-right,var_counter});
                    cnf.push_back({-var_counter, left, right});
                }
                break;
                case TOK_RPAR:
                {
                    auto token = scanner.yylex();
                    if (token != TOK_RPAR) {
                        std::cerr << "expected a right parenthesis; instead: " << translate(token) << std::endl;
                        exit(1);
                    }
                }
                break;
                default:
                    assert(((void)"Control flow corruption", 0));
            }
        }

        assert(values.size() == 1);
        cnf.push_back({values.back()});
    }

private:
    yyFlexLexer scanner;
    std::vector<Token> parsing;
    std::vector<lit_t> values;
};

std::unique_ptr<satlib_parser> satlib_parse(std::istream &in) {
    auto context = std::make_unique<Context>(in);
    context->parse();

    return context;
}