package expr

object PrintAstVisitor extends Visitor[String]:
   override def visitBinary(b: Expr.Binary): String =
     paren(b.operator.lexeme, b.left, b.right)

   override def visitGrouping(g: Expr.Grouping): String =
     paren("group", g.expr)

   override def visitLiteral(l: Expr.Literal): String =
     l.value.toString

   override def visitUnary(l: Expr.Unary): String =
     paren(l.operator.lexeme, l.right)

def paren(name: String, exprs: Expr*): String =
  s"($name${exprs.map(expr => s" ${Visitor.accept(expr, PrintAstVisitor)}").mkString("")})"
