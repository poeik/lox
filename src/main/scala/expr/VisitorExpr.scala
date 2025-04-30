package expr

trait VisitorExpr[A]:
   def visitBinary(b:      Expr.Binary): A
   def visitGrouping(g:    Expr.Grouping): A
   def visitLiteral(l:     Expr.Literal): A
   def visitUnary(l:       Expr.Unary): A
   def visitVariable(l:    Expr.Variable): A
   def visitAssignExpr(a:  Expr.Assignment): A
   def visitLogicalExpr(a: Expr.Logical): A

object VisitorExpr {
  def accept[A](ex: Expr, visitor: VisitorExpr[A]): A = ex match
     case b: Expr.Binary     => visitor.visitBinary(b)
     case g: Expr.Grouping   => visitor.visitGrouping(g)
     case l: Expr.Literal    => visitor.visitLiteral(l)
     case u: Expr.Unary      => visitor.visitUnary(u)
     case v: Expr.Variable   => visitor.visitVariable(v)
     case a: Expr.Assignment => visitor.visitAssignExpr(a)
     case l: Expr.Logical    => visitor.visitLogicalExpr(l)
}
