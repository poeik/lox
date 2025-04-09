package expr

trait VisitorStmt[A]:
   def visitExpressionStatement(b: Stmt.Expression): A
   def visitPrint(g:               Stmt.Print): A

object VisitorStmt {
  def accept[A](stmt: Stmt, visitor: VisitorStmt[A]): A = stmt match
     case p: Stmt.Print      => visitor.visitPrint(p)
     case e: Stmt.Expression => visitor.visitExpressionStatement(e)
}
