%{
/* need this for the call to atof() below */
#include <vector>
#include <cassert>
#include "satlib.hpp"
#define YY_DECL int yyFlexLexer::yylex()

static std::string yylval;

enum Token {
    FORMULA,
    AND,
    OR,
    NOT,
    LPAR,
    RPAR,
    VAR
};

%}


%option noyywrap
%option c++

ID          [a-z][a-z0-9]*

%%

"("         { return LPAR; }
")"         { return RPAR; }
"not"       { return NOT; }
"or"        { return OR; }
"and"       { return AND; }

{ID}        { yylval = yytext; return VAR; }

[ \t\n\r]+  /* eat up whitespace */

.           { std::cerr << "unrecognized character: " << yytext << '\n'; }

%%

class Context : public satlib_parser {
public:
    Context(std::istream &in) :
        scanner(&in), parsing{FORMULA}, values() { }

    lit_t get_name(std::string &variable) {
        auto [it, is_new] = names.try_emplace(variable, var_counter + 1);

        if (is_new)
            ++var_counter;

        return it->second;
    }

    void parse_variable() {
        int token = scanner.yylex();
        if (token == VAR) {
            values.push_back(get_name(yylval));
        } else {
            std::cerr << "expected a variable" << std::endl;
            exit(1);
        }
    }

    const char* translate(int what) {
        switch (what) {
            case FORMULA: return "`formula'";
            case AND: return "`and'";
            case OR: return "`or'";
            case NOT: return "`not'";
            case LPAR: return "'('";
            case RPAR: return "')'";
            case VAR: return yylval.c_str();
            default: return "unrecognized token";
        }
    }

    void parse_formula() {
        int token = scanner.yylex();
        if (token == LPAR) {
            token = scanner.yylex();
            switch (token) {
                case AND:
                    parsing.push_back(RPAR);
                    parsing.push_back(AND);
                    parsing.push_back(FORMULA);
                    parsing.push_back(FORMULA);
                break;
                case OR:
                    parsing.push_back(RPAR);
                    parsing.push_back(OR);
                    parsing.push_back(FORMULA);
                    parsing.push_back(FORMULA);
                break;
                case NOT:
                    parsing.push_back(RPAR);
                    parsing.push_back(NOT);
                    parsing.push_back(VAR);
                break;
                default:
                    std::cerr << "expected either `and', `or', or `not'; instead: " << translate(token) << std::endl;
                    exit(1);
                break;
            }
        } else if (token == VAR) {
            values.push_back(get_name(yylval));
        } else {
            std::cerr << "expected a formula; instead: " << translate(token) << std::endl;
            exit(1);
        }
    }
    void parse()
    {
        while (!parsing.empty())
        {

            auto goal = parsing.back();
            parsing.pop_back();
            switch (goal) {
                case FORMULA:
                    parse_formula();
                break;
                case VAR:
                    parse_variable();
                break;
                case NOT:
                {
                    auto var = values.back();
                    values.pop_back();
                    values.push_back(-var);
                }
                break;
                case AND:
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
                case OR:
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
                case RPAR:
                {
                    auto token = scanner.yylex();
                    if (token != RPAR) {
                        std::cerr << "expected a right parenthesis; instead: " << translate(token) << std::endl;
                        exit(1);
                    }
                }
                break;
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