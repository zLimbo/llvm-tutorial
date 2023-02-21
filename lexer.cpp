#include <cctype>
#include <cstdio>
#include <string>
#include <memory>
#include <vector>
#include <unordered_map>

/// The lexer returns tokens [0-255] if it is an unknown character,
/// otherwise one of these for known things
enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier 
static double NumVal; // Filled in if tok_number

/// gettok - Return the next token from standard input
static int gettok() {
    static int LastChar = ' ';

    // Skip any whitespace
    while (isspace(LastChar)) {
        LastChar = getchar();
    }
        
    if (isalpha(LastChar)) {    // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        while (isalpha(LastChar = getchar())) {
            IdentifierStr += LastChar;
        }

        if (IdentifierStr == "def") {
            return tok_def;
        }
        if (IdentifierStr == "extern") {
            return tok_extern;
        }
        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == '.') {     // Number: [0-9.]+
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    if (LastChar == '#') {
        // Comment until end of line
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF) {
            return gettok(); // 递归
        }
    }

    // Check for end of file. Don't eat the EOF
    if (LastChar == EOF) {
        return tok_eof;
    }

    // Otherwise, just return the character as its ascii value.
    // 剩下的都是些不常见的字符，如不可见和标点符号
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}


/// AST（Abstract Syntax Tree)
/// The AST for a program captures its behavior in such a way
/// that it is easy for later stages of the compiler to interpret.

/// expressions, prototype, function object

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
    virtual ~ExprAST() = default;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST: public ExprAST {
private:
    double Val;

public:
    NumberExprAST(double Val) : Val(Val) {}
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST: public ExprAST {
private:
    std::string Name;
public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST: public ExprAST {
private:
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                    std::unique_ptr<ExprAST> RHS)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

/// CallExprAST - Expression class for function calls.
class CallExprAST: public ExprAST {
private:
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
public:
    CallExprAST(const std::string &Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
        : Callee(Callee), Args(std::move(Args)) {}
};


/// PrototypeAST - This class represents the "prototype" for a function,
/// which capture its name, and its argument names (thus implicity the number
/// of arguments the function takes).
class PrototypeAST {
private:
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &Name, std::vector<std::string> Args)
        : Name(Name), Args(std::move(Args)) {}
    
    const std::string &getName() const { return Name; }
};

/// FunctionAST - This class represents a function definition iteself.
class FunctionAST {
private:
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) {}
};


/// CurTok/getNextToken - Provide a simple token buffer.
/// CurTok is the current token the parser is looking at.
/// getNextToken reads another token from the lexer and CurTok with its result;

static int CurTok;
static int getNextToken() {
    return CurTok = gettok();
}


/// LogError* - These are little helper functions for error handing.
std::unique_ptr<ExprAST> LogError(const char *Str) {
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
    LogError(Str);
    return nullptr;
}


/// For each production in grammar, we'll define a function which parse that production.

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken(); // consume the number
    return std::move(Result);
}


/// recursive descent parsers.
/// The most important role of parentheses are to guide the parser and provide grouping.
/// Once the parser constructs the AST, parentheses are not needed.
static std::unique_ptr<ExprAST> ParseExpression();

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken(); // eat (.
    auto V = ParseExpression();
    if (!V) {
        return nullptr;
    }
    if (CurTok != ')') {
        return LogError("expected ')'");
    }
    getNextToken(); // eat ).
    return V;
}

/// identifierexpr
///     ::= identifier
///     ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;

    getNextToken(); // eat idenfifier

    if (CurTok != '(')  { // Simple variable ref.
        return std::make_unique<VariableExprAST>(IdName);
    }

    // Call.
    getNextToken(); // eat (.
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != ')') {
        while (true) {
            if (auto Arg = ParseExpression()) {
                Args.push_back(Arg);
            } else {
                return nullptr;
            }
            
            if (CurTok == ')') {
                break;
            }
            if (CurTok != ',') {
                return LogError("Expected ')' or ',' in argument list");
            }
            getNextToken();
        }
    }

    // Eat the ')'
    getNextToken();

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
///     ::= identifierexpr
///     ::= numberexpr
///     ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        default:
            return LogError("unknown token when expecting an expression");
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
    }
}  

/// BinopPrecedence - This holds the precedence for each binary operator that is defined.
static std::unordered_map<char, int> BinopPrecedence = {
    // Install standard binary operators.
    // 1 is lowest precedence.
    {'<', 10},
    {'+', 20},
    {'-', 20},
    {'*', 40}
};

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
    if (!isascii(CurTok)) {
        return -1;
    }

    // Make sure it's a declared binop.
    if (auto TokPrec = BinopPrecedence.find(CurTok); 
            TokPrec != BinopPrecedence.end()) {
        return TokPrec->second;
    }
    return -1;
}


/// expression
///     ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS) {
        return nullptr;
    }
    return ParseBinOpRHS(0, std::move(LHS));
}

/// binoprhs
///     ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                            std::unique_ptr<ExprAST> LHS) {
    // If this is a binop, find its precedence.
    while (true) {
        int TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec) {
            return LHS;
        }

        // Okay, we know this is a binop
        int BinOp = CurTok;
        getNextToken(); // eat binop

        // Parse the primary expression after the binary operator
        auto RHS = ParsePrimary();
        if (!RHS) {
            return nullptr;
        }

        // If BinOp binds less tightly with RHS than the operator after RHS,
        // let the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS) {
                return nullptr;
            }
        }
        // Merge LHS/RHS.
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), 
                                                std::move(RHS));
    } // loop around to the top of the while loop.
    
}


int main() {

}




