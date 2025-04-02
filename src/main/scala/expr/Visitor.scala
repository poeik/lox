package expr

trait Visitor[A]:
   def visitBinary(b:     Expr.Binary): A
   def visitGrouping(g:   Expr.Grouping): A
   def visitLiteral(l: Expr.Literal): A
   def visitUnary(l:      Expr.Unary): A

object Visitor {
  def accept[A](ex: Expr, visitor: Visitor[A]): A = ex match
    case b: Expr.Binary => visitor.visitBinary(b)
    case g: Expr.Grouping => visitor.visitGrouping(g)
    case l: Expr.Literal => visitor.visitLiteral(l)
    case u: Expr.Unary => visitor.visitUnary(u)
}

