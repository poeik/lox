package expr

def prettyAst(expr: Expr): String =
  expr match
    case Expr.Binary(left, operator, right) => paren(operator.lexeme, left, right)
    case Expr.Grouping(expr) => paren("group", expr)
    case Expr.Literal(value) => value.map(_.toString).getOrElse("nil")
    case Expr.Unary(operator, right) => paren(operator.lexeme, right)

def paren(name: String, exprs: Expr*): String =
  s"($name${exprs.map(expr => s" ${prettyAst(expr)}").mkString("")})"