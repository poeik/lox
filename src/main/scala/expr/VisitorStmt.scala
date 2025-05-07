package expr

trait VisitorStmt[A]:
   def visitExpressionStatement(b: Stmt.Expression): A
   def visitPrint(g:               Stmt.Print): A
   def visitVarStmt(g:             Stmt.Var): A
   def visitBlock(b:               Stmt.Block): A
   def visitIf(i:                  Stmt.If): A
   def visitWhile(w:               Stmt.While): A
   def visitFunctionStatement(f:   Stmt.Function): A
   def visitReturnStatement(r:     Stmt.Return): A

object VisitorStmt {
  def accept[A](stmt: Stmt, visitor: VisitorStmt[A]): A = stmt match
     case p: Stmt.Print      => visitor.visitPrint(p)
     case e: Stmt.Expression => visitor.visitExpressionStatement(e)
     case v: Stmt.Var        => visitor.visitVarStmt(v)
     case b: Stmt.Block      => visitor.visitBlock(b)
     case i: Stmt.If         => visitor.visitIf(i)
     case w: Stmt.While      => visitor.visitWhile(w)
     case f: Stmt.Function   => visitor.visitFunctionStatement(f)
     case r: Stmt.Return     => visitor.visitReturnStatement(r)
}
