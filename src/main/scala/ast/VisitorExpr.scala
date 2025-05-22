package ast

trait VisitorExpr[A]:
    def visitBinary(b:      Expr.Binary): A
    def visitGrouping(g:    Expr.Grouping): A
    def visitLiteral(l:     Expr.Literal): A
    def visitUnary(l:       Expr.Unary): A
    def visitVariable(l:    Expr.Variable): A
    def visitAssignExpr(a:  Expr.Assignment): A
    def visitLogicalExpr(a: Expr.Logical): A
    def visitCallExpr(a:    Expr.Call): A
    def visitLambda(f:      Expr.Lambda): A
    def visitGet(expr:      Expr.Get): A
    def visitSet(expr:      Expr.Set): A

object VisitorExpr {
  def accept[A](ex: Expr, visitor: VisitorExpr[A]): A = ex match
      case expr: Expr.Binary     => visitor.visitBinary(expr)
      case expr: Expr.Grouping   => visitor.visitGrouping(expr)
      case expr: Expr.Literal    => visitor.visitLiteral(expr)
      case expr: Expr.Unary      => visitor.visitUnary(expr)
      case expr: Expr.Variable   => visitor.visitVariable(expr)
      case expr: Expr.Assignment => visitor.visitAssignExpr(expr)
      case expr: Expr.Logical    => visitor.visitLogicalExpr(expr)
      case expr: Expr.Call       => visitor.visitCallExpr(expr)
      case expr: Expr.Lambda     => visitor.visitLambda(expr)
      case expr: Expr.Get        => visitor.visitGet(expr)
      case expr: Expr.Set        => visitor.visitSet(expr)
}
