package expr

import token.Token

enum Expr:
  case Binary(left: Expr, operator: Token, right: Expr)
  case Grouping(expr: Expr)
  case Literal[A](value: Option[A])
  case Unary(operator: Token, right: Expr)
