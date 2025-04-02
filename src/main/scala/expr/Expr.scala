package expr

import token.Token

enum Lit:
  case Nil
  case Number(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)

enum Expr:
  case Binary(left: Expr, operator: Token, right: Expr)
  case Grouping(expr: Expr)
  case Literal(value: Lit)
  case Unary(operator: Token, right: Expr)
