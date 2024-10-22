// Notes:
// * Every parse node should have a representative token that is used for
//   error reporting
// * Lexer: negative integers, parsing ints
// * Consider switching to iterating instead of recursion in parse fns, reduce
//   memory usage
// * Use Token_FN etc instead of src_is "fn"
// * Prefix all user-supplied names that go into codegen so that there are no
//   conflicts with internal names
// * UTF-8
// * address-of operator?
// * keep comments/newlines/parens in parse tree (for pretty print)?
// * cast?
// * Read input from stdin instead of argv[1]
// * Intern needed strings so inputs can be freed (when reading from stdin)
// * Use custom allocator for MIR, bump allocator at first

#include "slap.h"
#include "mir-gen.h"
#include "mir.h"

// libc
#include <errno.h>  // for strtol err
#include <string.h> // strtol, memcpy, strcmp, strncmp, strlen
int printf(const char *, ...);
void exit(int);

#ifndef NDEBUG
#include <stdio.h>
#define LOG(fmt, ...) fprintf(stderr, fmt "\n", ##__VA_ARGS__)
#else
#define LOG(fmt, ...)
#endif

#define CHECK_(cond, fmt, ...)                                                 \
  do {                                                                         \
    if (!(cond)) {                                                             \
      printf("check failed [%d]: (%s) " fmt "\n", __LINE__, #cond,             \
             ##__VA_ARGS__);                                                   \
      exit(1);                                                                 \
    }                                                                          \
  } while (0)
#define CHECK(cond, ...) CHECK_(cond, "" __VA_ARGS__);

#define PARSE_MAXNODES (1 << 20)
#define ARRDATA_MAXLEN (1 << 20)
#define STRDATA_MAXLEN (1 << 20)
#define FN_MAXARGS 32

// parser state
Node nodes[PARSE_MAXNODES];
Node *nodes_next;
// codegen state
u8 arr_data[ARRDATA_MAXLEN];
char name_buf[STRDATA_MAXLEN];
char *name_buf_next;
MIR_var_t m_vars[FN_MAXARGS];

// Various lexing constants
#define PTRSIZE (sizeof(u64))
#define NSYMTOKS 8
#define NKEYWORDS 11
struct {
  char *str;
  u8 len;
  enum TokenType type;
} keywords[NKEYWORDS] = {
    {"u8", 2, Token_U8},         {"u16", 3, Token_U16},
    {"u32", 3, Token_U32},       {"u64", 3, Token_U64},
    {"void", 4, Token_VOID},     {"extern", 6, Token_EXTERN},
    {"fn", 2, Token_FN},         {"if", 2, Token_IF},
    {"else", 4, Token_ELSE},     {"while", 5, Token_WHILE},
    {"return", 6, Token_RETURN},
};
// P=punctuation, O=op, T=type, K=keyword, L=literal, N=name
char token_type_strs[Token__count] = "PPOOOOOOOTTTTTKKKKKKLLLN";
#define NDELIMETERS 30
char delimeters[NDELIMETERS] = "~!@#$%^&*()+`-=[]{}|;':\",.<>\n ";
#define NPREFIX_OPS 4
char *prefix_ops = "-!*~";
#define TokenNil ((Token){0})

// Lexer
// ----------------------------------------------------------------------------
static bool lex_match2(char *s, char *f) { return (*s == *f) && s[1] == f[1]; }

static bool lex_is_delim(char c) {
  for (u8 i = 0; i < NDELIMETERS; ++i) {
    if (c == delimeters[i])
      return true;
  }
  return false;
}

static Token lex_peek_symtok(Src *src) {
  char *s = src->cur;
  if (!lex_is_delim(*s))
    return TokenNil;

  struct {
    char *str;
    enum TokenType type;
  } symtoks[NSYMTOKS] = {
      {"==", Token_EQEQ},   {"<<", Token_LTLT},     {"<=", Token_LTEQ},
      {">>", Token_GTGT},   {">=", Token_GTEQ},     {":=", Token_COLONEQ},
      {"&&", Token_AMPAMP}, {"||", Token_PIPEPIPE},
  };

  for (u8 i = 0; i < NSYMTOKS; ++i) {
    if (lex_match2(s, symtoks[i].str))
      return (Token){s, 2, symtoks[i].type};
  }

  return (Token){s, 1, Token_PUNCT};
}

static enum TokenType lex_tok_type(Token t) {
  u8 i;
  if (*t.s >= '0' && *t.s <= '9')
    return Token_LIT_NUM;
  for (i = 0; i < NKEYWORDS; ++i) {
    if (t.len == keywords[i].len && strncmp(keywords[i].str, t.s, t.len) == 0) {
      return keywords[i].type;
    }
  }
  return Token_NAME;
}

static void lex_skip_ws_comments(Src *s) {
  bool clean = false;
  while (!clean && *s->cur) {
    clean = true;
    while (*s->cur == '\n') {
      clean = false;
      s->line++;
      s->cur++;
    }
    while (*s->cur == ' ') {
      clean = false;
      s->cur++;
    }
    if (*s->cur && *s->cur == '/' && s->cur[1] == '/') {
      clean = false;
      while (*s->cur && *s->cur != '\n')
        s->cur++;
    }
  }
}

static Token lex_token_next(Src *s) {
  char *start;
  u8 len;
  Token t;

  lex_skip_ws_comments(s);
  if (!*s->cur) {
    t = TokenNil;
    s->tok = t;
    return t;
  }

  if (*s->cur == '\'') {
    start = s->cur;
    s->cur++;
    for (len = 1; *s->cur != '\''; s->cur++)
      len++;
    s->cur++;
    len++;
    t = (Token){start, len, Token_LIT_CHAR};
    s->tok = t;
    return t;
  }

  if (*s->cur == '"') {
    start = s->cur;
    s->cur++;
    for (len = 1; *s->cur != '"'; s->cur++)
      len++;
    s->cur++;
    len++;
    t = (Token){start, len, Token_LIT_STR};
    s->tok = t;
    return t;
  }

  t = lex_peek_symtok(s);
  if (t.len > 0) {
    s->cur += t.len;
    s->tok = t;
    return t;
  }

  start = s->cur;
  len = 0;
  while (*s->cur && !lex_is_delim(*s->cur)) {
    s->cur++;
    len++;
  }

  t = (Token){start, len};
  t.type = lex_tok_type(t);
  s->tok = t;

  return t;
}
// ----------------------------------------------------------------------------

// Helpers
// ----------------------------------------------------------------------------
static void strz_reset() { name_buf_next = name_buf; }
static char *tok_strz(Token t) {
  CHECK(t.len < 256);
  memcpy(name_buf_next, t.s, t.len);
  name_buf_next[t.len] = 0;
  char *out = name_buf_next;
  name_buf_next += t.len + 1;
  CHECK((name_buf_next - name_buf) <= STRDATA_MAXLEN);
  return out;
}

static bool tok_eq(Token a, Token b) {
  return (a.len == b.len && strncmp(a.s, b.s, a.len) == 0);
}

static bool tok_is_literal(Token t) {
  return t.type == Token_LIT_CHAR || t.type == Token_LIT_STR ||
         t.type == Token_LIT_NUM;
}

static Token src_tok(Src *s) {
  if (s->tok.len == 0)
    lex_token_next(s);
  return s->tok;
}

static bool src_tok_nil(Token t) { return t.len == 0; }

static void print_tok(Token t) {
  if (src_tok_nil(t)) {
    printf("<nil>");
    return;
  }
  printf("[%.*s:%c]", t.len, t.s, token_type_strs[t.type]);
}

static Token src_tok_eat(Src *s) {
  Token t = s->tok;
  lex_token_next(s);

  print_tok(t);
  printf("\n");

  return t;
}

static bool src_tis(Src *src, enum TokenType t) {
  return src_tok(src).type == t;
}

static bool src_is(Src *src, char *s) {
  Token t;
  t = src_tok(src);
  if (src_tok_nil(t))
    return false;
  return (strlen(s) == src->tok.len && strncmp(t.s, s, t.len) == 0);
}

static void src_print(Src *s) {
  printf("curtok=");
  print_tok(s->tok);
  printf(", cur=%s\n", s->cur);
}

static void fail(Src *src, char *s) {
  src_print(src);
  printf("error: %s\n", s);
  exit(1);
}

static bool src_texpect(Src *src, enum TokenType type) {
  Token t;
  t = src_tok(src);
  if (t.type != type) {
    src_print(src);
    printf("error line=%d expected token type %d got %d\n", src->line, type,
           t.type);
    exit(1);
  }
  src_tok_eat(src);
  return true;
}

static bool src_expect(Src *src, char *s) {
  Token t;
  t = src_tok(src);
  if (strlen(s) != t.len || strncmp(t.s, s, t.len) != 0) {
    src_print(src);
    printf("error line=%d expected %s\n", src->line, s);
    exit(1);
    return false;
  }
  src_tok_eat(src);
  return true;
}

static void src_print_toks(Src *s) {
  Token t;
  while (1) {
    t = lex_token_next(s);
    if (src_tok_nil(t))
      break;
    print_tok(t);
  }
  printf("\n");
}

static Node *node(enum NodeType t) {
  Node *n = nodes_next;
  ++nodes_next;
  n->type = t;
  return n;
}

static char prefix_op(Src *s) {
  Token t;
  u8 i;

  t = src_tok(s);
  if (t.len != 1)
    return 0;
  for (i = 0; i < NPREFIX_OPS; ++i)
    if (prefix_ops[i] == *t.s)
      return *t.s;
  return 0;
}

enum Precedence {
  Prec_NONE,
  Prec_COMP,
  Prec_SUM,
  Prec_PROD,
  Prec_BITS,
  Prec_POST,
};

static u8 op_precedence(Token t) {
  switch (t.type) {
  case Token_EQEQ:
  case Token_LTEQ:
  case Token_GTEQ:
    return Prec_COMP;
  case Token_LTLT:
  case Token_GTGT:
    return Prec_PROD;
  case Token_AMPAMP:
  case Token_PIPEPIPE:
    return Prec_BITS;
  default:
    break;
  }

  switch (*t.s) {
  case '<':
  case '>':
  case '!':
    return Prec_COMP;
  case '+':
  case '-':
    return Prec_SUM;
  case '/':
  case '*':
  case '%':
  case '&':
  case '|':
  case '^':
    return Prec_PROD;
  case '(':
  case '[':
    return Prec_POST;
  }

  return Prec_NONE;
}

static NodeID node_id(Node *n) { return (NodeID)(n - nodes) + 1; }

static Node *stmt(enum StmtType t) {
  Node *n;
  n = node(Type_Stmt);
  n->val.stmt.type = t;
  return n;
}

static void parse_init() { nodes_next = nodes; }
// ----------------------------------------------------------------------------

// Parser
// ----------------------------------------------------------------------------

// Forward decls for recursively called functions
static NodeID parse_expr(Src *s, u8 precedence);
static NodeID parse_stmts(Src *s);

static NodeID parse_type_array(Src *s) {
  NodeID out;
  src_expect(s, "[");
  out = parse_expr(s, 0);
  src_expect(s, "]");
  return out;
}

static NodeID parse_typespec(Src *s) {
  Node *tn = node(Type_Type);
  NodeType *n = &tn->val.type;

  n->nptr = 0;
  while (src_is(s, "*")) {
    src_tok_eat(s);
    n->nptr++;
  }

  switch (src_tok(s).type) {
  case Token_U8:
  case Token_U16:
  case Token_U32:
  case Token_U64:
  case Token_VOID:
    break;
  default:
    fail(s, "expected a return type");
    break;
  }
  n->type = src_tok_eat(s);

  if (src_is(s, "[")) {
    n->len = parse_type_array(s);
  }
  return node_id(tn);
}

static NodeID parse_exprs(Src *s, char *stop) {
  Node *n;
  NodeID nid;

  if (src_is(s, stop))
    return NodeNil;

  nid = parse_expr(s, 0);
  n = node_get(nid);

  if (src_is(s, ","))
    src_tok_eat(s);

  n->next = parse_exprs(s, stop);
  return node_id(n);
}

static NodeID parse_expr_array(Src *s) {
  Node *n;

  src_expect(s, "{");

  n = node(Type_Expr);
  n->val.expr.op = Op_ArrayLiteral;
  n->val.expr.term0 = parse_exprs(s, "}");
  src_expect(s, "}");

  return node_id(n);
}

static NodeID parse_expr_token(Src *s) {
  Node *n;
  n = node(Type_Expr);
  n->val.expr.op = Op_Token;
  n->val.expr.tok = src_tok_eat(s);
  return node_id(n);
}

static NodeID parse_expr_prefix(Src *s, char prefix) {
  Node *n = node(Type_Expr);
  n->val.expr.op = Op_Unary;
  n->val.expr.tok = src_tok_eat(s);
  n->val.expr.term0 = parse_expr(s, 0);
  return node_id(n);
}

static NodeID parse_expr_array_access(Src *s, NodeID lhs) {
  Node *n;
  src_expect(s, "[");

  n = node(Type_Expr);
  n->val.expr.op = Op_ArrayAccess;
  n->val.expr.term0 = lhs;
  n->val.expr.term1 = parse_expr(s, 0);

  src_expect(s, "]");
  return node_id(n);
}

static NodeID parse_expr_call(Src *s, NodeID lhs) {
  Node *n;

  src_expect(s, "(");

  n = node(Type_Expr);
  n->val.expr.op = Op_Call;
  n->val.expr.term0 = lhs;
  n->val.expr.term1 = parse_exprs(s, ")");

  src_expect(s, ")");
  return node_id(n);
}

static NodeID parse_expr_infix(Src *s, NodeID lhs, enum Precedence prec) {
  Node *n;

  if (src_is(s, "("))
    return parse_expr_call(s, lhs);
  if (src_is(s, "["))
    return parse_expr_array_access(s, lhs);

  n = node(Type_Expr);
  n->val.expr.op = Op_Binary;
  n->val.expr.tok = src_tok_eat(s);
  n->val.expr.term0 = lhs;
  n->val.expr.term1 = parse_expr(s, prec);

  return node_id(n);
}

static NodeID parse_expr(Src *s, u8 precedence) {
  NodeID nid;
  char prefix;
  enum Precedence prec;

  prefix = prefix_op(s);
  if (prefix) {
    nid = parse_expr_prefix(s, prefix);
  } else if (src_is(s, "(")) {
    src_tok_eat(s);
    nid = parse_expr(s, 0);
    src_expect(s, ")");
  } else if (src_is(s, "{")) {
    nid = parse_expr_array(s);
  } else if (tok_is_literal(src_tok(s)) || src_tok(s).type == Token_NAME) {
    nid = parse_expr_token(s);
  } else {
    CHECK(false);
  }

  while (1) {
    prec = op_precedence(src_tok(s));
    if (precedence >= prec)
      break;
    nid = parse_expr_infix(s, nid, prec);
  }

  return nid;
}

static NodeID parse_vardecls(Src *s) {
  if (src_tok(s).type != Token_NAME)
    return NodeNil;

  Node *n = node(Type_Var);
  n->val.var.name = src_tok_eat(s);
  src_expect(s, ":");
  n->val.var.type = parse_typespec(s);

  if (s->tok.type == Token_COLONEQ) {
    src_tok_eat(s);
    n->val.var.init = parse_expr(s, 0);
    src_expect(s, ";");
  } else {
    src_expect(s, ";");
  }

  n->next = parse_vardecls(s);

  return node_id(n);
}

static NodeID parse_fndef_args(Src *s) {
  if (src_is(s, ")"))
    return NodeNil;

  Node *n;
  n = node(Type_FndefArg);

  n->val.fnarg.name = src_tok_eat(s);
  src_expect(s, ":");
  n->val.fnarg.type = parse_typespec(s);

  if (src_is(s, ","))
    src_tok_eat(s);

  n->next = parse_fndef_args(s);

  return node_id(n);
}

static NodeID parse_stmt_assign(Src *s, NodeID lhs) {
  Node *n;
  n = stmt(Stmt_Assign);
  n->val.stmt.val.assign.lhs = lhs;

  if (src_is(s, ":")) {
    src_tok_eat(s);
    n->val.stmt.val.assign.type = parse_typespec(s);
  }

  if (s->tok.type == Token_COLONEQ) {
    n->val.stmt.val.assign.decl = true;
  } else if (src_is(s, "=")) {
    n->val.stmt.val.assign.decl = false;
  } else {
    fail(s, "expected = or :=");
  }
  src_tok_eat(s);

  n->val.stmt.val.assign.rhs = parse_expr(s, 0);
  src_expect(s, ";");
  return node_id(n);
}

static NodeID parse_stmt_return(Src *s) {
  Node *n;

  n = stmt(Stmt_Return);
  n->val.stmt.val.ret.tok = src_tok_eat(s);
  n->val.stmt.val.ret.expr = parse_expr(s, 0);

  src_expect(s, ";");

  return node_id(n);
}

static NodeID parse_stmt_while(Src *s) {
  Node *n;

  n = stmt(Stmt_While);
  src_expect(s, "while");
  n->val.stmt.val.xwhile.cond = parse_expr(s, 0);
  src_expect(s, "{");
  n->val.stmt.val.xwhile.body = parse_stmts(s);
  src_expect(s, "}");

  return node_id(n);
}

static NodeID parse_stmt_if(Src *s) {
  Node *n;
  Node *ne;

  n = stmt(Stmt_If);
  src_expect(s, "if");
  n->val.stmt.val.xif.cond = parse_expr(s, 0);
  src_expect(s, "{");
  n->val.stmt.val.xif.body = parse_stmts(s);
  src_expect(s, "}");

  if (src_is(s, "else")) {
    src_tok_eat(s);
    if (src_is(s, "if")) {
      n->next = parse_stmt_if(s);
    } else {
      src_expect(s, "{");
      ne = stmt(Stmt_Else);
      ne->val.stmt.val.xelse.body = parse_stmts(s);
      n->next = node_id(ne);
      src_expect(s, "}");
    }
  }

  return node_id(n);
}

static NodeID parse_stmt_if_root(Src *s) {
  Node *n;
  n = stmt(Stmt_IfRoot);
  n->val.stmt.val.ifs.ifs = parse_stmt_if(s);
  return node_id(n);
}

static NodeID parse_stmts(Src *s) {
  NodeID nid = 0;
  Node *n = 0;

  if (src_is(s, "}"))
    return NodeNil;

  switch (src_tok(s).type) {
  case Token_RETURN:
    nid = parse_stmt_return(s);
    break;
  case Token_IF:
    nid = parse_stmt_if_root(s);
    break;
  case Token_WHILE:
    nid = parse_stmt_while(s);
    break;
  default:
    NodeID neid;
    Node *ne;
    neid = parse_expr(s, 0);
    ne = node_get(neid);
    if (ne->val.expr.op == Op_Call) {
      n = stmt(Stmt_Call);
      n->val.stmt.val.call.expr = neid;
      nid = node_id(n);
      src_expect(s, ";");
    } else {
      nid = parse_stmt_assign(s, neid);
    }
  }

  n = node_get(nid);
  n->next = parse_stmts(s);

  return nid;
}

static NodeID parse_fndefs(Src *s) {
  Node *n;

  if (!(src_tis(s, Token_FN) || src_tis(s, Token_EXTERN)))
    return NodeNil;

  bool xextern = src_tis(s, Token_EXTERN);
  if (xextern)
    src_tok_eat(s);
  src_texpect(s, Token_FN);

  n = node(Type_Fndef);
  n->val.fn.xextern = xextern;
  n->val.fn.name = src_tok_eat(s);
  src_expect(s, "(");
  n->val.fn.args = parse_fndef_args(s);
  src_expect(s, ")");
  n->val.fn.ret = parse_typespec(s);

  if (src_is(s, "{")) {
    if (xextern)
      fail(s, "extern functions cannot have definitions");
    src_tok_eat(s);
    n->val.fn.stmts = parse_stmts(s);
    src_expect(s, "}");
  } else {
    src_expect(s, ";");
  }

  n->next = parse_fndefs(s);

  return node_id(n);
}

static NodeID parse_root(Src *s) {
  Node *n = node(Type_Root);
  n->val.root.vars = parse_vardecls(s);
  n->val.root.fns = parse_fndefs(s);
  return node_id(n);
}
// ----------------------------------------------------------------------------

// Printer
// ----------------------------------------------------------------------------

// Forward decls for recursively called functions
static void print_expr(NodeID id);
static void print_stmts(NodeID nid, u8 indent);

static void print_exprs(NodeID id) {
  Node *n = node_get(id);
  while (n) {
    print_expr(node_id(n));
    n = node_get(n->next);
    if (n)
      printf(", ");
  }
}

static void print_expr(NodeID id) {
  Node *c;
  NodeExpr *e = &node_get(id)->val.expr;
  switch (e->op) {
  case Op_Token:
    printf("%.*s", e->tok.len, e->tok.s);
    break;
  case Op_ArrayLiteral:
    printf("{");
    print_exprs(e->term0);
    printf("}");
    break;
  case Op_Unary:
    printf("%.*s", e->tok.len, e->tok.s);
    print_expr(e->term0);
    break;
  case Op_Binary:
    printf("(");
    print_expr(e->term0);
    printf(" %.*s ", e->tok.len, e->tok.s);
    print_expr(e->term1);
    printf(")");
    break;
  case Op_Call:
    print_expr(e->term0);
    printf("(");
    c = node_get(e->term1);
    while (c) {
      print_expr(node_id(c));
      c = node_get(c->next);
      if (c)
        printf(", ");
    }
    printf(")");
    break;
  case Op_ArrayAccess:
    print_expr(e->term0);
    printf("[");
    print_expr(e->term1);
    printf("]");
    break;
  default:
    printf("<expr %d>", e->op);
  }
}

static void print_type(NodeID id) {
  u8 i;
  Node *n = node_get(id);
  for (i = 0; i < n->val.type.nptr; ++i)
    printf("*");
  printf("%.*s", n->val.type.type.len, n->val.type.type.s);
  if (n->val.type.len) {
    printf("[");
    print_expr(n->val.type.len);
    printf("]");
  }
}

static void print_var(NodeVar *n) {
  printf("%.*s: ", n->name.len, n->name.s);
  print_type(n->type);
  if (n->init) {
    printf(" := ");
    print_expr(n->init);
  }
  printf(";\n");
}

static void pindent(u8 indent) {
  u8 i;
  for (i = 0; i < indent; ++i)
    putchar(' ');
}

static void print_stmt(Node *stmt, u8 indent) {
  Node *c;
  NodeStmt *n = &stmt->val.stmt;
  pindent(indent);
  switch (n->type) {
  case Stmt_Call:
    print_expr(n->val.call.expr);
    printf(";\n");
    break;
  case Stmt_While:
    printf("while ");
    print_expr(n->val.xwhile.cond);
    printf(" {\n");
    print_stmts(n->val.xwhile.body, indent + 2);
    pindent(indent);
    printf("}\n");
    break;
  case Stmt_Return:
    printf("return ");
    print_expr(n->val.ret.expr);
    printf(";\n");
    break;
  case Stmt_Assign:
    print_expr(n->val.assign.lhs);
    if (n->val.assign.type) {
      printf(": ");
      print_type(n->val.assign.type);
    }
    if (n->val.assign.decl)
      printf(" := ");
    else
      printf(" = ");
    print_expr(n->val.assign.rhs);
    printf(";\n");
    break;
  case Stmt_IfRoot:
    c = node_get(n->val.ifs.ifs);
    while (c) {
      if (c->val.stmt.type == Stmt_Else) {
        printf("{\n");
        print_stmts(c->val.stmt.val.xelse.body, indent + 2);
        pindent(indent);
        printf("}");
      } else {
        printf("if ");
        print_expr(c->val.stmt.val.xif.cond);
        printf(" {\n");
        print_stmts(c->val.stmt.val.xif.body, indent + 2);
        pindent(indent);
        printf("}");
      }
      c = node_get(c->next);
      if (c)
        printf(" else ");
    }
    printf("\n");
    break;
  default:
    printf("<stmt %d>;\n", n->type);
  }
}

static void print_stmts(NodeID nid, u8 indent) {
  Node *c;
  c = node_get(nid);
  while (c) {
    print_stmt(c, indent);
    c = node_get(c->next);
  }
}

static void print_fn_args(NodeID id) {
  Node *n = node_get(id);
  while (n) {
    printf("%.*s: ", n->val.fnarg.name.len, n->val.fnarg.name.s);
    print_type(n->val.fnarg.type);
    n = node_get(n->next);
    if (n)
      printf(", ");
  }
}

static void print_fn(NodeFndef *n) {
  bool def = n->stmts;
  if (def)
    printf("\n");
  if (n->xextern)
    printf("extern ");
  printf("fn %.*s(", n->name.len, n->name.s);
  print_fn_args(n->args);
  printf(") ");
  print_type(n->ret);
  if (n->stmts) {
    printf(" {\n");
    print_stmts(n->stmts, 2);
    printf("}");
  } else {
    printf(";");
  }
  printf("\n");
}

static void print_root(NodeRoot *n) {
  Node *c;

  printf("ROOT\n");

  c = node_get(n->vars);
  while (c) {
    print_var(&c->val.var);
    c = node_get(c->next);
  }
  if (n->vars)
    printf("\n");

  c = node_get(n->fns);
  while (c) {
    print_fn(&c->val.fn);
    c = node_get(c->next);
  }
}

static void print_parse(NodeID id) {
  Node *n = node_get(id);
  if (!n) {
    printf("<nil>\n");
    return;
  }
  if (n->type != Type_Root) {
    printf("<not ROOT>\n");
    return;
  }
  print_root(&n->val.root);
}
// ----------------------------------------------------------------------------

// Code generation
// ----------------------------------------------------------------------------

static u32 type_base_size(enum TokenType t) {
  switch (t) {
  case Token_VOID:
    return 0;
  case Token_U8:
    return 1;
  case Token_U16:
    return 2;
  case Token_U32:
    return 4;
  case Token_U64:
    return 8;
  default:
    return UINT32_MAX;
  }
}

static void check_error(Token t, char *s) {
  printf("error at token %.*s: %s\n", t.len, t.s, s);
  exit(1);
}

static u64 parse_int(Token t) {
  for (u8 i = 0; i < t.len; ++i) {
    if (t.s[i] < '0' || t.s[i] > '9') {
      check_error(t, "expected an integer literal");
    }
  }
  char *end = t.s + t.len;
  long out = strtol(t.s, &end, 10);
  if (errno == ERANGE || out <= 0 || out >= UINT64_MAX) {
    check_error(t, "integer literal out of range");
  }
  return out;
}

typedef struct {
  u32 elsz;
  u64 n;
} TypeSize;

static TypeSize type_size(NodeType t) {
  TypeSize sz = {0};

  if (t.nptr) {
    sz.elsz = PTRSIZE;
  } else {
    sz.elsz = type_base_size(t.type.type);
    if (sz.elsz == UINT32_MAX) {
      check_error(t.type, "expected a type");
    }
  }

  if (t.len) {
    Node *n = node_get(t.len);
    CHECK(n->type == Type_Expr);
    Token arr = n->val.expr.tok;
    if (arr.type == Token_LIT_NUM) {
      sz.n = parse_int(arr);
    } else if (arr.len == 1 && *arr.s == '_') {
      sz.n = UINT64_MAX;
    } else {
      check_error(arr, "expected a length");
    }
  }

  return sz;
}

static void check_inttype(Token tok, NodeType t) {
  if (t.type.type < Token_U8 || t.type.type > Token_U64 || t.nptr || t.len) {
    check_error(tok, "expected an integer type given the value");
  }
}

static bool is_inttype(enum TokenType t) {
  return t >= Token_U8 && t <= Token_U64;
}

static MIR_type_t to_mirtype(NodeType t) {
  CHECK(!t.len);
  if (t.nptr)
    return MIR_T_P;
  MIR_type_t types[4] = {MIR_T_U8, MIR_T_U16, MIR_T_U32, MIR_T_U64};
  CHECK(is_inttype(t.type.type));
  return types[t.type.type - Token_U8];
}

static bool is_voidtype(NodeType t) {
  return t.type.type == Token_VOID && !t.nptr && !t.len;
}

typedef struct {
  bool isvoid;
  MIR_type_t type;
} CretType;

static CretType codegen_rettype(NodeFndef fn) {
  NodeType ret = node_get(fn.ret)->val.type;
  u8 nres = !is_voidtype(ret);
  MIR_type_t mret = nres ? to_mirtype(ret) : 0;
  return (CretType){nres == 0, mret};
}

static u8 codegen_fillvars(NodeFndef fn) {
  MIR_var_t *var = m_vars;
  u8 nargs = 0;
  Node *arg = node_get(fn.args);
  while (arg) {
    var->name = tok_strz(arg->val.fnarg.name);
    NodeType arg_type = node_get(arg->val.fnarg.type)->val.type;
    if (arg_type.len)
      check_error(
          arg->val.fnarg.name,
          "functions cannot take arrays as arguments, pass a pointer instead");
    var->type = to_mirtype(arg_type);
    arg = node_get(arg->next);
    ++nargs;
    ++var;
  }
  return nargs;
}

typedef struct {
  Token name;
  NodeID type;
  MIR_item_t val;
} EnvEntry;

#define ENV_ENTRIES_MAX 4096

typedef struct {
  Token name;
  MIR_item_t addr;
  MIR_item_t proto;
  CretType ret;
} FnDef;

typedef struct {
  EnvEntry *top;
  EnvEntry entries[ENV_ENTRIES_MAX];
  FnDef fns[ENV_ENTRIES_MAX];
  FnDef *fn_next;
} Env;

typedef struct {
  MIR_context_t mir;
  MIR_item_t fn;
  Env env;
} CodegenState;

void env_push(CodegenState *s, Token name, NodeID type, MIR_item_t val) {
  Env *env = &s->env;
  if (!env->top)
    env->top = env->entries;
  EnvEntry *e = env->top++;
  e->name = name;
  e->type = type;
  e->val = val;
}

void env_reset(CodegenState *s, EnvEntry *top) { s->env.top = top; }

void env_fn(CodegenState *s, Token name, MIR_item_t addr, MIR_item_t proto,
            CretType ret) {
  Env *env = &s->env;
  if (!env->fn_next)
    env->fn_next = env->fns;

  FnDef *fn = env->fn_next++;
  fn->name = name;
  fn->addr = addr;
  fn->proto = proto;
  fn->ret = ret;
}

FnDef *env_fn_get(CodegenState *s, Token name) {
  if (!s->env.fn_next)
    return 0;
  FnDef *cur = s->env.fn_next - 1;
  while (cur >= s->env.fns) {
    if (tok_eq(cur->name, name))
      return cur;
    --cur;
  }
  return 0;
}

EnvEntry *env_get(CodegenState *s, Token name) {
  if (!s->env.top)
    return 0;
  EnvEntry *cur = s->env.top - 1;
  while (cur >= s->env.entries) {
    if (tok_eq(cur->name, name))
      return cur;
    --cur;
  }
  return 0;
}

MIR_op_t env_get_op(CodegenState *s, Token name) {
  EnvEntry *e = env_get(s, name);
  if (!e)
    check_error(name, "unrecognized name");
  return MIR_new_ref_op(s->mir, e->val);
}

static void codegen_stmt_call(CodegenState *s, NodeStmtCall stmt) {
  NodeExpr *expr = &node_get(stmt.expr)->val.expr;
  CHECK(expr->op == Op_Call);
  Token name = node_get(expr->term0)->val.expr.tok;

  // Evaluate function
  CHECK(node_get(expr->term0)->val.expr.op == Op_Token);
  FnDef *fn = env_fn_get(s, name);
  if (!fn)
    check_error(name, "could not find function");

  // Call
  // ops: prototype, fnaddr, retops..., argops...
  u8 nops = 2;
  MIR_op_t ops[FN_MAXARGS + 3];
  ops[0] = MIR_new_ref_op(s->mir, fn->proto);
  ops[1] = MIR_new_ref_op(s->mir, fn->addr);

  // Create a local for the return value
  if (!fn->ret.isvoid) {
    nops++;
    // TODO
    check_error(name, "ret type unimpl");
  }

  // Evaluate and typecheck arguments
  // TODO:
  u8 nargs = 0;
  Node *arg = node_get(expr->term1);
  while (arg) {
    // Each argument is an arbitrary expression

    // Could allocate a local output variable per arg based on the fn
    // signature, then codegen_expr into that output. Typecheck along the way.
    CHECK(arg->type == Type_Expr);
    NodeExpr *expr = &arg->val.expr;
    switch (expr->op) {
    case Op_Token:
      if (expr->tok.type == Token_NAME) {
        ops[nops++] = env_get_op(s, expr->tok);
      } else
        check_error(name, "arg expr type unimplemented");
      break;
    default:
      check_error(name, "arg expr type unimplemented");
    }

    arg = node_get(arg->next);
    ++nargs;
  }

  MIR_append_insn(s->mir, s->fn, MIR_new_insn_arr(s->mir, MIR_CALL, nops, ops));
}

static void codegen_stmt_ret(CodegenState *s, NodeStmtRet stmt) {
  if (!stmt.expr) {
    MIR_append_insn(s->mir, s->fn, MIR_new_insn_arr(s->mir, MIR_RET, 0, 0));
    return;
  }

  NodeExpr *expr = &node_get(stmt.expr)->val.expr;
  MIR_op_t ret;

  switch (expr->op) {
  case Op_Token:
    if (expr->tok.type == Token_NAME) {
      ret = env_get_op(s, expr->tok);
    } else if (expr->tok.type == Token_LIT_NUM) {
      u64 intval = parse_int(expr->tok);
      ret = MIR_new_int_op(s->mir, intval);
    } else
      check_error(stmt.tok, "ret expr type unimplemented");
    break;
  default:
    check_error(stmt.tok, "ret expr type unimplemented");
  }

  MIR_append_insn(s->mir, s->fn, MIR_new_insn_arr(s->mir, MIR_RET, 1, &ret));
}

static void codegen_stmt_assign(CodegenState *s, NodeStmtAssign stmt) {}
static void codegen_stmt_if(CodegenState *s, NodeStmtIfRoot stmt) {}
static void codegen_stmt_while(CodegenState *s, NodeStmtWhile stmt) {}

static void codegen(MIR_context_t mir, NodeID root_id) {
  CodegenState state;
  state.mir = mir;

  MIR_module_t mod = MIR_new_module(mir, "slap");
  (void)mod;

  Node *root = node_get(root_id);

  Node *var_node = node_get(root->val.root.vars);
  while (var_node) {
    NodeVar *var = &var_node->val.var;
    strz_reset();

    // Name
    Token name = var->name;
    char *name_str = tok_strz(name);

    // Type
    Node *tn = node_get(var->type);
    CHECK(tn->type == Type_Type);
    TypeSize sz = type_size(tn->val.type);

    MIR_item_t mval;

    // Initializer
    if (var->init) {
      // Data
      Node *init = node_get(var->init);
      CHECK(init->type == Type_Expr);

      if (init->val.expr.op == Op_Token) {
        Token init_tok = init->val.expr.tok;
        if (init_tok.type == Token_LIT_NUM) {
          // TODO:
          // * Really shouldn't create data but instead should use a literal
          //   integer op in MIR
          // * Check that the value matches the type (out of range)
          check_inttype(name, tn->val.type);
          u64 intval = parse_int(init_tok);
          mval = MIR_new_data(mir, name_str, MIR_T_I64, 1, &intval);
        } else if (init_tok.type == Token_LIT_CHAR) {
          if (tn->val.type.type.type != Token_U8)
            check_error(name, "type must be u8 for a character value");
          u64 charval = *init_tok.s;
          mval = MIR_new_data(mir, name_str, MIR_T_I64, 1, &charval);
        } else if (init_tok.type == Token_LIT_STR) {
          if (tn->val.type.type.type != Token_U8)
            check_error(name, "base type must be u8 for a string value");
          if (!tn->val.type.len)
            check_error(name, "type must be an array for a string value");
          if (sz.n == UINT64_MAX)
            sz.n = init_tok.len - 2;
          if (init_tok.len - 2 != sz.n)
            check_error(name, "string doesn't match the given array length");
          init_tok.s[init_tok.len - 1] = 0;
          mval =
              MIR_new_data(mir, name_str, MIR_T_U8, sz.n + 1, init_tok.s + 1);
        } else {
          check_error(name, "constant initializer must be a literal");
        }
      } else if (init->val.expr.op == Op_ArrayLiteral) {
        if (!is_inttype(tn->val.type.type.type))
          check_error(
              name,
              "value must have an integer base type for array initialization");
        MIR_type_t mtype = to_mirtype(tn->val.type);
        u32 arr_len = 0;
        Node *el = node_get(init->val.expr.term0);
        u8 *data = arr_data;
        while (el) {
          if (el->val.expr.op != Op_Token ||
              el->val.expr.tok.type != Token_LIT_NUM)
            check_error(el->val.expr.tok,
                        "expected number literals in an array initializer");
          CHECK(data - arr_data < ARRDATA_MAXLEN);
          u64 intval = parse_int(el->val.expr.tok);
          switch (mtype) {
          case MIR_T_U8:
            *data = intval;
            data += 1;
            break;
          case MIR_T_U16:
            *(u16 *)data = intval;
            data += 2;
            break;
          case MIR_T_U32:
            *(u32 *)data = intval;
            data += 4;
            break;
          case MIR_T_U64:
            *(u64 *)data = intval;
            data += 8;
            break;
          default:
            CHECK(false);
          }
          el = node_get(el->next);
          ++arr_len;
        }

        if (sz.n == UINT64_MAX)
          sz.n = arr_len;
        if (sz.n != arr_len)
          check_error(name, "array initializer doesn't have the right length");
        mval = MIR_new_data(mir, name_str, mtype, sz.n, arr_data);
      } else {
        check_error(name, "constant initializer must be a literal");
      }
    } else {
      // BSS
      if (sz.n == UINT64_MAX)
        check_error(
            tn->val.type.type,
            "cannot use an array length of _ on an uninitialized variable");
      u64 totalsz = sz.n * sz.elsz;
      if (totalsz)
        mval = MIR_new_bss(mir, name_str, totalsz);
    }

    env_push(&state, name, var->type, mval);

    var_node = node_get(var_node->next);
  }

  EnvEntry *global = state.env.top;

  Node *fn = node_get(root->val.root.fns);
  while (fn) {
    strz_reset();
    env_reset(&state, global);

    char *name_str = tok_strz(fn->val.fn.name);
    char *proto_str = tok_strz(fn->val.fn.name);
    memcpy(proto_str + fn->val.fn.name.len, "__p", 4);
    name_buf_next += 3;

    CretType ret = codegen_rettype(fn->val.fn);
    u8 nargs = codegen_fillvars(fn->val.fn);

    MIR_item_t proto = MIR_new_proto_arr(mir, proto_str, !ret.isvoid, &ret.type,
                                         nargs, m_vars);
    MIR_item_t def;

    if (fn->val.fn.xextern) {
      def = MIR_new_import(mir, name_str);
    } else {
      if (fn->val.fn.stmts) {
        def = MIR_new_func_arr(mir, name_str, !ret.isvoid, &ret.type, nargs,
                               m_vars);
        state.fn = def;
        Node *stmtn = node_get(fn->val.fn.stmts);
        while (stmtn) {
          NodeStmt stmt = stmtn->val.stmt;
          switch (stmt.type) {
          case Stmt_Call:
            codegen_stmt_call(&state, stmt.val.call);
            break;
          case Stmt_Assign:
            codegen_stmt_assign(&state, stmt.val.assign);
            break;
          case Stmt_If:
            codegen_stmt_if(&state, stmt.val.ifs);
            break;
          case Stmt_While:
            codegen_stmt_while(&state, stmt.val.xwhile);
            break;
          case Stmt_Return:
            codegen_stmt_ret(&state, stmt.val.ret);
            break;
          default:
            CHECK(false);
          }
          stmtn = node_get(stmtn->next);
        }
        MIR_finish_func(mir);
      } else {
        def = MIR_new_forward(mir, name_str);
      }
    }

    env_fn(&state, fn->val.fn.name, def, proto, ret);
    fn = node_get(fn->next);
  }

  MIR_finish_module(mir);
}

static __attribute__((noreturn)) void
slap_mir_error(MIR_error_type_t error_type, const char *message, ...) {
  printf("mir error (%d): ", error_type);
  va_list args;
  va_start(args, message);
  vprintf(message, args);
  va_end(args);
  printf("\n");
  exit(1);
}
// ----------------------------------------------------------------------------

// Code execution
// ----------------------------------------------------------------------------
static void import_print(char *msg) { printf("MIR: %s\n", msg); }

static void *slap_mir_import_resolve(const char *name) {
  if (strcmp(name, "print") == 0)
    return import_print;
  return 0;
}

typedef int (*main_fn_sig)(int argc, char **argv);

static MIR_item_t load_fn(MIR_context_t mir, char *name) {
  // Load main
  MIR_item_t main_fn = 0;
  MIR_module_t mod = DLIST_HEAD(MIR_module_t, *MIR_get_module_list(mir));
  MIR_load_module(mir, mod);
  for (MIR_item_t item = DLIST_HEAD(MIR_item_t, mod->items); item != NULL;
       item = DLIST_NEXT(MIR_item_t, item)) {
    if (item->item_type != MIR_func_item)
      continue;
    MIR_func_t fn = MIR_get_item_func(mir, item);
    if (strcmp(fn->name, name) == 0) {
      main_fn = item;
      break;
    }
  }
  if (!main_fn) {
    printf("error: could not find main\n");
    exit(1);
  }
  return main_fn;
}

static void run_interp(MIR_context_t mir) {
  MIR_item_t iterp_main = load_fn(mir, "main");
  MIR_link(mir, MIR_set_interp_interface, slap_mir_import_resolve);
  char *argv[2] = {"slapinterp", "hello!"};
  MIR_val_t args[2] = {
      {.i = 2},
      {.a = argv},
  };
  MIR_val_t result;
  MIR_interp_arr(mir, iterp_main, &result, 2, args);
  printf("interp result %ld\n", result.i);
}

static void run_jit(MIR_context_t mir) {
  MIR_item_t gen_main = load_fn(mir, "main");
  MIR_gen_init(mir);
  MIR_gen_set_optimize_level(mir, 2);
  MIR_link(mir, MIR_set_gen_interface, slap_mir_import_resolve);
  void *main_gen = MIR_gen(mir, gen_main);
  char *argv[2] = {"slapinterp", "hello!"};
  int gen_res = ((main_fn_sig)(main_gen))(2, argv);
  printf("gen result %d\n", gen_res);
  MIR_gen_finish(mir);
}
// ----------------------------------------------------------------------------

// Public
// ----------------------------------------------------------------------------
Node *node_get(NodeID id) { return (id == 0) ? 0 : &nodes[id - 1]; }

void src_init(Src *s, char *start) {
  s->start = start;
  s->cur = start;
  s->line = 1;
  s->tok = TokenNil;
}

NodeID parse(Src *s) {
  parse_init();
  return parse_root(s);
}
// ----------------------------------------------------------------------------

#ifndef NOMAIN
int main(int argc, char **argv) {
  Src s;

  src_init(&s, argv[1]);
  src_print_toks(&s);

  src_init(&s, argv[1]);

  parse_init();
  NodeID root = parse(&s);

  src_print(&s);
  print_parse(root);

  printf("\n\n");
  printf("sizeof(Node)=%ld\n", sizeof(Node));
  printf("\n\n");

  MIR_context_t mir = MIR_init();
  MIR_set_error_func(mir, slap_mir_error);

  // Codegen
  codegen(mir, root);
  MIR_output(mir, stdout);

  // Run
  // TODO: if run_interp runs first, run_jit will segfault in MIR_link:
  // generate_func_code->dse->calculate_mem_live_info->initiate_mem_live_info->
  // initiate_bb_mem_live_info->make_live_from_mem (src/mir-gen.c:5045)
  run_jit(mir);
  run_interp(mir);

  MIR_finish(mir);
  return 0;
}
#endif
