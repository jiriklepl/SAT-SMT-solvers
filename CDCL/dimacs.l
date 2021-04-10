%{
/* need this for the call to atof() below */
#include <vector>
#include <cassert>
#include <sstream>
#include "dimacs.hpp"
#define YY_DECL int yyFlexLexer::yylex()

static lit_t yylval;

enum Token {
    TOK_HEAD,
    TOK_C_END,
    TOK_C_LIT,
    TOK_EOF,
};

%}


%option noyywrap
%option c++

CLAUSE_LIT   -?[1-9][0-9]*
CLAUSE_END   0

%%

c.*\n        /* eat comments */
"p cnf"      { return TOK_HEAD; }

{CLAUSE_LIT} { auto ss = std::istringstream(yytext); ss >> yylval; return TOK_C_LIT; }
{CLAUSE_END} { return TOK_C_END; }

[ \t\n\r]+  /* eat up whitespace */

<<EOF>>      { return TOK_EOF; }

.            { std::cerr << "unrecognized character: " << yytext << '\n'; }

%%

class Context : public dimacs_parser {
public:
    Context(std::istream &in) :
        dimacs_parser(), scanner(&in), lits() { }

    ~Context() noexcept override = default;

    const char* translate(int what) {
        switch ((Token)what) {
            default: return "unrecognized token";
        }
    }

    void parse_header() {
        auto token = scanner.yylex();

        if (token == TOK_HEAD) {
            token = scanner.yylex();
            if (token == TOK_C_LIT && yylval > 0) {
                var_counter = yylval;
            } else if (token != TOK_C_END) {
                std::cerr << "bad format for the number of variables" << std::endl;
            }
            
            token = scanner.yylex();
            if (token == TOK_C_LIT && yylval > 0) {
                cnf.reserve(yylval);
            } else if (token == TOK_C_END) {
                cnf.reserve(0);
            } else {
                std::cerr << "bad format for the number of clauses" << std::endl;
            }
        } else {
            std::cerr << "`p cnf nbvar nbclause' expected; instead: " << translate(token) << std::endl;
            exit(1);
        }
    }

    bool maybe_parse_clause() {
        auto token = scanner.yylex();

        if (token == TOK_EOF) {
            return false;
        }

        while (token == TOK_C_LIT) {
            assert((
                (void)"The literal index shouldn't exceed the declared number of literals",
                std::abs(yylval) <= var_counter));

            lits.push_back(yylval);

            token = scanner.yylex();
        }

        if (token != TOK_C_END) {
            std::cerr << "a literal or `0' expected; instead: " << translate(token) << std::endl;
            exit(1);

            return false;
        }

        assert((
            (void)"The real number of clauses shouldn't exceed the expected number of clauses",
            cnf.size() < cnf.capacity()));

        cnf.push_back(lits);
        lits.clear();

        return true;
    }

    void parse() {
        parse_header();
        while (cnf.size() < cnf.capacity() && maybe_parse_clause());
    }

private:
    yyFlexLexer scanner;
    std::vector<lit_t> lits;
};

std::unique_ptr<dimacs_parser> dimacs_parse(std::istream &in) {
    auto context = std::make_unique<Context>(in);
    context->parse();
    assert((
        (void)"The real number of clauses should match the expected number of clauses",
        context->cnf.size() == context->cnf.capacity()));

    return context;
}

int main() {
    auto parser = dimacs_parse(std::cin);

    for (auto &&clause : parser->cnf) {
        for (auto &&lit : clause) {
            std::cout << lit << ' ';
        }
        std::cout << std::endl;
    }
}