#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include <cctype>
#include <cstdio>
#include <map>
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
        while (isalnum(LastChar = getchar())) {
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

        NumVal = strtod(NumStr.c_str(), nullptr);
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

///===---------------------------------------------------------------===//
/// AST（Abstract Syntax Tree)
/// The AST for a program captures its behavior in such a way
/// that it is easy for later stages of the compiler to interpret.
///===---------------------------------------------------------------===//

/// expressions, prototype, function object

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual llvm::Value *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST: public ExprAST {
private:
    double Val;

public:
    NumberExprAST(double Val) : Val(Val) {}

    llvm::Value *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST: public ExprAST {
private:
    std::string Name;
public:
    VariableExprAST(const std::string &Name) : Name(Name) {}

    llvm::Value *codegen() override;
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

    llvm::Value *codegen() override;
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

    llvm::Value *codegen() override;
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
    
    llvm::Function *codegen();
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
    
    llvm::Function *codegen();
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
                Args.push_back(std::move(Arg));
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


static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                            std::unique_ptr<ExprAST> LHS);

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


/// prototype
///     ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier) {
        return LogErrorP("Expected function name in prototype");
    }

    std::string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(') {
        return LogErrorP("Expected '(' in prototype");
    }

    // Read the list of argument names.
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier) {
        ArgNames.push_back(IdentifierStr);
    }
    if (CurTok != ')') {
        return LogErrorP("Expected ')' in prototype");
    }

    // success
    getNextToken(); // eat ')'.

    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// definition := 'def' pototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken();
    auto Proto = ParsePrototype();
    if (!Proto) {
        return nullptr;
    }
    if (auto E = ParseExpression()) {
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

/// external := 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken(); // eat extern
    return ParsePrototype();
}

/// toplevelexpr := expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto E = ParseExpression()) {
        // Make an anonymous proto.
        auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

//===------------------===//
// Code Generation
//===------------------===//

// TheContext is an opaque object that owns a lot of core LLVM data structres,
// such as the type and constant value tables.
static std::unique_ptr<llvm::LLVMContext> TheContext;

// TheModule is an LLVM construct that contains functions and global variables.
// The LLVM Module is the container that holds the functions we are JIT'ing.
static std::unique_ptr<llvm::Module> TheModule;

// The Builder object is a helper object that makes it easy to gennerate LLVM instructions.
// Instances of the IRBuilder class template keep track of the current place to insert instructions
// and has methods to create new instructions.
static std::unique_ptr<llvm::IRBuilder<>> Builder;

// The NamedValue map keeps track of which values are defined in the current scope
// and what their LLVM representation is.
static std::map<std::string, llvm::Value *> NamedValues;

llvm::Value *LogErrorV(const char *Str) {
    LogError(Str);
    return nullptr;
}

llvm::Value *NumberExprAST::codegen() {
    // Use "foo::get(...)": that constants are all uniqued together and shared.
    return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Val));
}

llvm::Value *VariableExprAST::codegen() {
    // Look this variable up in the function.
    llvm::Value *V = NamedValues[Name];
    if (!V) {
        LogErrorV("Unknown variable name");
    }
    return V;
}

llvm::Value *BinaryExprAST::codegen() {
    llvm::Value *L = LHS->codegen();
    llvm::Value *R = RHS->codegen();
    if (!L || !R) {
        return nullptr;
    }
    switch (Op) {
        case '+':
            return Builder->CreateFAdd(L, R, "addtmp");
        case '-':
            return Builder->CreateFSub(L, R, "subtmp");
        case '*':
            return Builder->CreateFMul(L, R, "multmp");
        case '<':
            L = Builder->CreateFCmpULT(L, R, "cmptmp");
            // Convert bool 0/1 to double 0.0 or 1.0
            return Builder->CreateUIToFP(L, llvm::Type::getDoubleTy(*TheContext), 
                                        "booltmp");
        default:
            return LogErrorV("invalid binary operator");
    }
}

llvm::Value *CallExprAST::codegen() {
    // Look up the name in the global module table.
    llvm::Function *CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF) {
        return LogErrorV("Unknown function referenced");
    }

    // If argument mismatch error.
    if (CalleeF->arg_size() != Args.size()) {
        return LogErrorV("Incorrect # argument passed");
    }

    std::vector<llvm::Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back()) {
            return nullptr;
        }
    }
    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}


llvm::Function *PrototypeAST::codegen() {
    // Make the function type: double(double, double) etc.
    std::vector<llvm::Type*> Doubles(Args.size(), llvm::Type::getDoubleTy(*TheContext));
    llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getDoubleTy(*TheContext),
                                                    Doubles, false);
    llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, 
                                                Name, TheModule.get());
    // Set names for all arguments
    unsigned Idx = 0;
    for (auto &Arg: F->args()) {
        Arg.setName(Args[Idx++]);
    }                            
    return F;                    
}


llvm::Function *FunctionAST::codegen() {
    // First, check for an existing function from a previous 'extern' declaration.
    llvm::Function *TheFunction = TheModule->getFunction(Proto->getName());

    if (!TheFunction) {
        TheFunction = Proto->codegen();
    }

    if (!TheFunction) {
        return nullptr;
    }

    if (!TheFunction->empty()) {
        return (llvm::Function*)LogErrorV("Function cannot be redefined.");
    }

    // Create a new basic block to start insertion into.
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    // Record the function arguments in the NameValues map.
    NamedValues.clear();
    for (auto &Arg : TheFunction->args()) {
        NamedValues[std::string(Arg.getName())] = &Arg;
    }

    if (llvm::Value *RetVal = Body->codegen()) {
        // Finish off the function.
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency
        llvm::verifyFunction(*TheFunction);

        return TheFunction;
    }

    // Error reading body, remove function
    TheFunction->eraseFromParent();
    return nullptr;
}









//====-------------------===//
// Top-Level parsing
//====-------------------===//

static void HandleDefinition() {
    if (ParseDefinition()) {
        fprintf(stderr, "Parsed a function definition.\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern() {
    if (ParseExtern()) {
        fprintf(stderr, "Parsed an extern\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (ParseTopLevelExpr()) {
        fprintf(stderr, "Parsed a top-level expr\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top := definition | external | expression | ';'
static void MainLoop() {
    while (true) {
        fprintf(stderr, "ready> ");
        switch (CurTok) {
            case tok_eof: return;
            case ';': getNextToken(); break;
            case tok_def: HandleDefinition(); break;
            case tok_extern: HandleExtern(); break;
            default: HandleTopLevelExpression(); break;
        }
    }
}



int main() {

    // Prime the first token.
    fprintf(stderr, "ready> ");

    getNextToken();

    // Run the main "interpreter loop" now.
    MainLoop();

    // auto ast = ParseExpression();

    return 0;
}




