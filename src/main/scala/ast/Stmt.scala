package ast

import token.Token

enum Stmt:
   case Print(expr: Expr)
   case Expression(expr: Expr)
   case Function(name: Token, params: List[Token], body: List[Stmt])
   case Return(keyword: Token, value: Expr)
   case Var(name: Token, initializer: Expr)
   case Block(statements: List[Stmt])
   case If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])
   case While(condition: Expr, body: Stmt)
