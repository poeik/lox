package ast

import token.Token

enum Expr:
   case Binary(left: Expr, operator: Token, right: Expr)
   case Call(callee: Expr, paren: Token, arguments: List[Expr])
   case Grouping(expr: Expr)
   case Literal(value: Lit)
   case Unary(operator: Token, right: Expr)
   case Variable(name: Token)
   case Assignment(name: Token, value: Expr)
   case Logical(left: Expr, operator: Token, right: Expr)
   case Lambda(params: List[Token], body: List[Stmt])
