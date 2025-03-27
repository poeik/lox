package expr

object PrintAstVisitor extends Visitor[String]:
   override def visitBinary(b: Expr.Binary): String =
     paren(b.operator.lexeme, b.left, b.right)

   override def visitGrouping(g: Expr.Grouping): String =
     paren("group", g.expr)

   override def visitLiteral[B](l: Expr.Literal[B]): String =
     l.value.map(_.toString).getOrElse("nil")

   override def visitUnary(l: Expr.Unary): String =
     paren(l.operator.lexeme, l.right)

def paren(name: String, exprs: Expr*): String =
  s"($name${exprs.map(expr => s" ${Visitor.accept(expr, PrintAstVisitor)}").mkString("")})"
