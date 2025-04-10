package expr

object PrintAstVisitorExpr extends VisitorExpr[String]:
   override def visitBinary(b: Expr.Binary): String =
     paren(b.operator.lexeme, b.left, b.right)

   override def visitGrouping(g: Expr.Grouping): String =
     paren("group", g.expr)

   override def visitLiteral(l: Expr.Literal): String =
     l.value.toString

   override def visitUnary(l: Expr.Unary): String =
     paren(l.operator.lexeme, l.right)

   override def visitVariable(l: Expr.Variable): String = l.name.lexeme

   override def visitAssignExpr(a: Expr.Assignment): String =
     s"${a.name.lexeme} = ${VisitorExpr.accept(a.value, PrintAstVisitorExpr)}"

def paren(name: String, exprs: Expr*): String =
  s"($name${exprs.map(expr => s" ${VisitorExpr.accept(expr, PrintAstVisitorExpr)}").mkString("")})"
