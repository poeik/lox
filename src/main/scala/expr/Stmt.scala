package expr

import token.Token

enum Stmt:
   case Print(expr: Expr)
   case Expression(expr: Expr)
   case Var(name: Token, initializer: Expr)
   case Block(statements: List[Stmt])