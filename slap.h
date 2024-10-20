#include <stdint.h>
#include <stdbool.h>

#define u8 uint8_t
#define u32 uint32_t
#define NodeID u32
#define NodeNil 0

enum TokenType {
  Token_DELIM,
  Token_NAME,
  Token_KEYWORD,
  Token_LIT_NUM,
  Token_LIT_STR,
  Token_LIT_CHAR,
};

typedef struct {
  char* s;
  u8 len;
  enum TokenType type;
} Token;

typedef struct {
  char* start;
  char* cur;
  u32 line;
  Token tok;
} Src;

typedef struct {
  NodeID vars;
  NodeID fns;
} NodeRoot;

typedef struct {
  Token name;
  NodeID type;
  NodeID init;
} NodeVar;

typedef struct {
  Token type;
  u8 nptr;  /* pointer */
  NodeID len;  /* array */
} NodeType;

enum OpType {
  Op_Token,
  Op_ArrayLiteral,
  Op_Call,
  Op_ArrayAccess,
  Op_Cast,

  Op_Unary,
  Op_Binary,
};

typedef struct {
  enum OpType op;
  Token tok;
  NodeID term0;
  NodeID term1;
} NodeExpr;

typedef struct {
  Token name;
  NodeID type;
} NodeFndefArg;

typedef struct {
  Token name;
  NodeID args;
  NodeID ret;
  NodeID stmts;
} NodeFndef;

typedef struct {
  NodeID type;
  NodeID lhs;
  NodeID rhs;
  bool decl;
} NodeStmtAssign;

typedef struct {
  NodeID cond;
  NodeID body;
} NodeStmtIf;

typedef struct {
  NodeID ifs;
} NodeStmtIfRoot;

typedef struct {
  NodeID body;
} NodeStmtElse;

typedef struct {
  NodeID cond;
  NodeID body;
} NodeStmtWhile;

typedef struct {
  NodeID expr;
} NodeStmtCall;

typedef struct {
  NodeID expr;
} NodeStmtRet;

enum StmtType {
  Stmt_Call,
  Stmt_Assign,
  Stmt_If,
  Stmt_While,
  Stmt_Return,

  Stmt_IfRoot,
  Stmt_Else,
};

typedef struct {
  enum StmtType type;
  union {
    NodeStmtAssign assign;
    NodeStmtIf xif;
    NodeStmtIfRoot ifs;
    NodeStmtElse xelse;
    NodeStmtWhile xwhile;
    NodeStmtCall call;
    NodeStmtRet ret;
  } val;
} NodeStmt;

enum NodeType {
  Type_Error,
  Type_Root,
  Type_Var,
  Type_Type,
  Type_Token,
  Type_Fndef,
  Type_FndefArg,
  Type_Expr,
  Type_Stmt,
};

typedef struct {
  enum NodeType type;
  NodeID next;

  union {
    NodeRoot root;
    NodeVar var;
    NodeType type;
    NodeExpr expr;
    NodeFndef fn;
    NodeFndefArg fnarg;
    NodeStmt stmt;
    Token tok;
  } val;
} Node;

void src_init(Src* s, char* start);
void parse_init();
NodeID parse(Src* s);
Node* node_get(NodeID id);