/* TODO
* need to handle multi-char tokens
* address of?
* add back to op_precedence when == handled case '=':
* keep comments/newlines/parens in parse tree (for pretty print)?
* cast()
*/

#include "slap.h"

int strncmp(const char*, const char*, long unsigned int);
long unsigned int strlen(const char*);
int printf(const char*, ...);
void exit(int);

#include <stdio.h>
#define LOG(fmt, ...) fprintf(stderr, fmt "\n", ##__VA_ARGS__)

#define TokenNil ((Token){0})

/* parser state */
Node nodes[1 << 20];
Node* nodes_next;

#define NKEYWORDS 11
char* keywords[NKEYWORDS] = {
  "void", "u8", "u16", "u32", "u64", "bool",
  "while", "if", "else", "return",
  "fn",
};
u8 keywords_len[NKEYWORDS];
char* delimeters = "~!@#$%^&*()+`-=[]{}|;':\",.<>\n ";
#define NPREFIX_OPS 4
char* prefix_ops = "-!*~";
char* token_type_strs = "DNKLLL";

static void print_tok(Token t);
static void src_print(Src* s);
static void parse_print(NodeID id);
static NodeID parse_expr(Src* s, u8 precedence);
static NodeID parse_stmts(Src* s);
static void print_expr(NodeID id);
static void print_stmts(NodeID nid, u8 indent);

static bool is_delim(char c) {
  u8 ndelimeters = strlen(delimeters);
  u8 i = 0;
  for (; i < ndelimeters; ++i) {
    if (c == delimeters[i]) return true;
  }
  return false;
}

static bool tok_is_literal(Token t) {
  return t.type == Token_LIT_CHAR ||
         t.type == Token_LIT_STR ||
         t.type == Token_LIT_NUM;
}

static enum TokenType tok_type(Token t) {
  u8 i;
  if (*t.s == '\'')
    return Token_LIT_CHAR;
  if (*t.s == '"')
    return Token_LIT_STR;
  if (*t.s >= '0' && *t.s <= '9')
    return Token_LIT_NUM;
  for (i = 0; i < NKEYWORDS; ++i) {
    if (t.len == keywords_len[i] &&
        strncmp(keywords[i], t.s, t.len) == 0) {
      return Token_KEYWORD;
    }
  }
  return Token_NAME;
}

static bool src_peek_delimeter(Src* s) {
  char c = *s->cur;
  return is_delim(c);
}

static void src_skip_ws_comments(Src* s) {
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
      while (*s->cur && *s->cur != '\n') s->cur++;
    }
  }
}

static Token src_token_next(Src* s) {
  char* start;
  u8 len;
  Token t;

  src_skip_ws_comments(s);

  if (!*s->cur) {
    t = TokenNil;
    s->tok = t;
    return t;
  }

  if (*s->cur == '\'') {
    start = s->cur;
    s->cur++;
    for (len = 1; *s->cur != '\''; s->cur++) len++;
    s->cur++;
    len++;
    t = (Token){start, len, Token_LIT_CHAR};
    s->tok = t;
    return t;
  }

  if (*s->cur == '"') {
    start = s->cur;
    s->cur++;
    for (len = 1; *s->cur != '"'; s->cur++) len++;
    s->cur++;
    len++;
    t = (Token){start, len, Token_LIT_STR};
    s->tok = t;
    return t;
  }

  if (src_peek_delimeter(s)) {
    s->cur++;
    t = (Token){s->cur - 1, 1, Token_DELIM};
    s->tok = t;
    return t;
  }
  if (!*s->cur) return TokenNil;

  start = s->cur;
  len = 0;
  while (*s->cur && !src_peek_delimeter(s)) {
    s->cur++;
    len++;
  }

  t = (Token){start, len};
  t.type = tok_type(t);
  s->tok = t;

  return t;
}

static Token src_tok(Src* s) {
  if (s->tok.len == 0) src_token_next(s);
  return s->tok;
}

static bool src_tok_nil(Token t) { return t.len == 0; }

static Token src_tok_eat(Src* s) {
  Token t = s->tok;
  src_token_next(s);

  print_tok(t);
  printf("\n");

  return t;
}

static bool src_is(Src* src, char* s) {
  Token t;
  t = src_tok(src);
  if (src_tok_nil(t)) return false;
  return (strlen(s) == src->tok.len && strncmp(t.s, s, t.len) == 0);
}

static bool src_expect(Src* src, char* s) {
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

static void print_tok(Token t) {
  if (src_tok_nil(t)) {
    printf("<nil>");
    return;
  }
  printf("[%.*s:%c]", t.len, t.s, token_type_strs[t.type]);
}

static void src_print_toks(Src* s) {
  Token t;
  while (1) {
    t = src_token_next(s);
    if (src_tok_nil(t)) break;
    print_tok(t);
  }
  printf("\n");
}

static void src_print(Src* s) {
  printf("curtok=");
  print_tok(s->tok);
  printf(", cur=%s\n", s->cur);
}

static Node* node(enum NodeType t) {
  Node* n = nodes_next;
  ++nodes_next;
  n->type = t;
  return n;
}

static NodeID node_id(Node* n) { return (NodeID)(n - nodes) + 1; }

static Node* stmt(enum StmtType t) {
  Node* n;
  n = node(Type_Stmt);
  n->val.stmt.type = t;
  return n;
}

static NodeID parse_type_array(Src* s) {
  NodeID out;
  src_expect(s, "[");
  out = parse_expr(s, 0);
  src_expect(s, "]");
  return out;
}

static NodeID parse_typespec(Src* s) {
  Node* tn = node(Type_Type);
  NodeType* n = &tn->val.type;

  n->nptr = 0;
  while (src_is(s, "*")) {
    src_tok_eat(s);
    n->nptr++;
  }

  n->type = src_tok_eat(s);

  if (src_is(s, "[")) {
    n->len = parse_type_array(s);
  }
  return node_id(tn);
}

static NodeID parse_exprs(Src* s, char* stop) {
  Node* n;
  NodeID nid;

  if (src_is(s, stop)) return NodeNil;

  nid = parse_expr(s, 0);
  n = node_get(nid);

  if (src_is(s, ",")) src_tok_eat(s);

  n->next = parse_exprs(s, stop);
  return node_id(n);
}

static NodeID parse_expr_array(Src* s) {
  Node* n;

  src_expect(s, "{");

  n = node(Type_Expr);
  n->val.expr.op = Op_ArrayLiteral;
  n->val.expr.term0 = parse_exprs(s, "}");
  src_expect(s, "}");

  return node_id(n);
}

static NodeID parse_expr_token(Src* s) {
  Node* n;
  n = node(Type_Expr);
  n->val.expr.op = Op_Token;
  n->val.expr.tok = src_tok_eat(s);
  return node_id(n);
}

static char prefix_op(Src* s) {
  Token t;
  u8 i;

  t = src_tok(s);
  if (t.len != 1) return 0;
  for (i = 0; i < NPREFIX_OPS; ++i) if (prefix_ops[i] == *t.s) return *t.s;
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

static NodeID parse_expr_prefix(Src* s, char prefix) {
  Node* n = node(Type_Expr);
  n->val.expr.op = Op_Unary;
  n->val.expr.tok = src_tok_eat(s);
  n->val.expr.term0 = parse_expr(s, 0);
  return node_id(n);
}

static NodeID parse_expr_array_access(Src* s, NodeID lhs) {
  Node* n;
  src_expect(s, "[");

  n = node(Type_Expr);
  n->val.expr.op = Op_ArrayAccess;
  n->val.expr.term0 = lhs;
  n->val.expr.term1 = parse_expr(s, 0);

  src_expect(s, "]");
  return node_id(n);
}

static NodeID parse_expr_call(Src* s, NodeID lhs) {
  Node* n;

  src_expect(s, "(");

  n = node(Type_Expr);
  n->val.expr.op = Op_Call;
  n->val.expr.term0 = lhs;
  n->val.expr.term1 = parse_exprs(s, ")");

  src_expect(s, ")");
  return node_id(n);
}

static NodeID parse_expr_infix(Src* s, NodeID lhs, enum Precedence prec) {
  Node* n;

  if (src_is(s, "(")) return parse_expr_call(s, lhs);
  if (src_is(s, "[")) return parse_expr_array_access(s, lhs);


  n = node(Type_Expr);
  n->val.expr.op = Op_Binary;
  n->val.expr.tok = src_tok_eat(s);
  n->val.expr.term0 = lhs;
  n->val.expr.term1 = parse_expr(s, prec);

  return node_id(n);
}

static NodeID parse_expr(Src* s, u8 precedence) {
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
  } else if (tok_is_literal(src_tok(s)) || src_tok(s).type == Token_NAME) {
    nid = parse_expr_token(s);
  } else if (src_is(s, "{")) {
    nid = parse_expr_array(s);
  }

  while (1) {
    prec = op_precedence(src_tok(s));
    if (precedence >= prec) break;
    nid = parse_expr_infix(s, nid, prec);
  }

  return nid;
}

static NodeID parse_vardecls(Src* s) {
  if (src_tok(s).type != Token_NAME) return NodeNil;

  Node* n = node(Type_Var);
  n->val.var.name = src_tok_eat(s);
  src_expect(s, ":");
  n->val.var.type = parse_typespec(s);

  if (src_is(s, ":")) {
    src_tok_eat(s);
    src_expect(s, "=");
    n->val.var.init = parse_expr(s, 0);
    src_expect(s, ";");
  } else {
    src_expect(s, ";");
  }

  n->next = parse_vardecls(s);

  return node_id(n);
}

static NodeID parse_fndef_args(Src* s) {
  if (src_is(s, ")")) return NodeNil;

  Node* n;
  n = node(Type_FndefArg);

  n->val.fnarg.name = src_tok_eat(s);
  n->val.fnarg.type = parse_typespec(s);

  if (src_is(s, ",")) src_tok_eat(s);

  n->next = parse_fndef_args(s);

  return node_id(n);
}

static NodeID parse_stmt_assign(Src* s, NodeID lhs) {
  Node* n;
  n = stmt(Stmt_Assign);
  n->val.stmt.val.assign.lhs = lhs;
  if (src_is(s, ":")) {
    src_tok_eat(s);
    if (src_is(s, "=")) {
      n->val.stmt.val.assign.decl = true;
    } else {
      n->val.stmt.val.assign.type = parse_typespec(s);
      if (src_is(s, ":")) {
        src_tok_eat(s);
        n->val.stmt.val.assign.decl = true;
      }
    }
  }

  src_expect(s, "=");
  n->val.stmt.val.assign.rhs = parse_expr(s, 0);
  src_expect(s, ";");
  return node_id(n);
}

static NodeID parse_stmt_return(Src* s) {
  Node* n;

  src_tok_eat(s);
  n = stmt(Stmt_Return);
  n->val.stmt.val.ret.expr = parse_expr(s, 0);

  src_expect(s, ";");
  
  return node_id(n);
}

static NodeID parse_stmt_while(Src* s) {
  Node* n;

  n = stmt(Stmt_While);
  src_expect(s, "while");
  n->val.stmt.val.xwhile.cond = parse_expr(s, 0);
  src_expect(s, "{");
  n->val.stmt.val.xwhile.body = parse_stmts(s);
  src_expect(s, "}");

  return node_id(n);
}

static NodeID parse_stmt_if(Src* s) {
  Node* n;
  Node* ne;

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

static NodeID parse_stmt_if_root(Src* s) {
  Node* n;
  n = stmt(Stmt_IfRoot);
  n->val.stmt.val.ifs.ifs = parse_stmt_if(s);
  return node_id(n);
}

static NodeID parse_stmts(Src* s) {
  NodeID nid, neid;
  Node* n;
  Node* ne;

  if (src_is(s, "}")) return NodeNil;

  if (src_tok(s).type == Token_KEYWORD) {
    if (src_is(s, "return")) {
      nid = parse_stmt_return(s);
    } else if (src_is(s, "if")) {
      nid = parse_stmt_if_root(s);
    } else if (src_is(s, "while")) {
      nid = parse_stmt_while(s);
    }
  } else {
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

static NodeID parse_fndefs(Src* s) {
  Node* n;

  if (!src_is(s, "fn")) return NodeNil;
  src_tok_eat(s);

  n = node(Type_Fndef);
  n->val.fn.name = src_tok_eat(s);
  src_expect(s, "(");
  n->val.fn.args = parse_fndef_args(s);
  src_expect(s, ")");
  n->val.fn.ret = parse_typespec(s);

  if (src_is(s, "{")) {
    src_tok_eat(s);
    n->val.fn.stmts = parse_stmts(s);
    src_expect(s, "}");
  } else {
    src_expect(s, ";");
  }
  
  n->next = parse_fndefs(s);

  return node_id(n);
}

static void print_exprs(NodeID id) {
  Node* n = node_get(id);
  while (n) {
    print_expr(node_id(n));
    n = node_get(n->next);
    if (n) printf(", ");
  }
}

static void print_expr(NodeID id) {
  Node* c;
  NodeExpr* e = &node_get(id)->val.expr;
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
        if (c) printf(", ");
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
  Node* n = node_get(id);
  for (i = 0; i < n->val.type.nptr; ++i) printf("*");
  printf("%.*s", n->val.type.type.len, n->val.type.type.s);
  if (n->val.type.len) {
    printf("[");
    print_expr(n->val.type.len);
    printf("]");
  }
}

static void print_var(NodeVar* n) {
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
  for (i = 0; i < indent; ++i) putchar(' ');
}

static void print_stmt(Node* stmt, u8 indent) {
  Node* c;
  NodeStmt* n = &stmt->val.stmt;
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
      if (n->val.assign.decl) printf(" := "); else printf(" = ");
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
        if (c) printf(" else ");
      }
      printf("\n");
      break;
    default:
      printf("<stmt %d>;\n", n->type);
  }
}

static void print_stmts(NodeID nid, u8 indent) {
  Node* c;
  c = node_get(nid);
  while (c) {
    print_stmt(c, indent);
    c = node_get(c->next);
  }
}

static void print_fn(NodeFndef* n) {
  bool def = n->stmts;
  if (def) printf("\n");
  printf("fn %.*s(", n->name.len, n->name.s);
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

static void print_root(NodeRoot* n) {
  Node* c;

  printf("ROOT\n");

  c = node_get(n->vars);
  while (c) {
    print_var(&c->val.var);
    c = node_get(c->next);
  }
  if (n->vars) printf("\n");

  c = node_get(n->fns);
  while (c) {
    print_fn(&c->val.fn);
    c = node_get(c->next);
  }
}

static void parse_print(NodeID id) {
  Node* n = node_get(id);
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

Node* node_get(NodeID id) { return (id == 0) ? 0 : &nodes[id - 1]; }

void src_init(Src* s, char* start) {
  s->start = start;
  s->cur = start;
  s->line = 1;
  s->tok = TokenNil;
}

void parse_init() {
  u8 i;
  nodes_next = nodes;
  for (i = 0; i < NKEYWORDS; ++i) keywords_len[i] = strlen(keywords[i]);
}

NodeID parse(Src* s) {
  Node* n = node(Type_Root);
  n->val.root.vars = parse_vardecls(s);
  n->val.root.fns = parse_fndefs(s);
  return node_id(n);
}

void analyze(NodeID root) {
}

int main(int argc, char** argv) {
  Src s;

  src_init(&s, argv[1]);
  src_print_toks(&s);

  src_init(&s, argv[1]);

  parse_init();
  NodeID root = parse(&s);

  analyze(root);

  src_print(&s);
  parse_print(root);

  printf("\nsizeof(Node)=%ld\n", sizeof(Node));
  return 0;
}

/* Check + augment parse */
/* Generate code
 * Sections: text, rodata, data, bss
 * data: initialized module-level data
 * bss: uninitialized module-level data
 * rodata: const analysis?
 * text: functions in machine code, conforming to calling convention
 */
