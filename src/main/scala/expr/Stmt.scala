package expr

enum Stmt:
   case Print(expr: Expr)
   case Expression(expr: Expr)