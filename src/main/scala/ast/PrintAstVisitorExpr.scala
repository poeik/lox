package ast

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

    override def visitLogicalExpr(a: Expr.Logical): String =
      s"${VisitorExpr.accept(a.left, PrintAstVisitorExpr)} ${a.operator} ${VisitorExpr.accept(a.right, PrintAstVisitorExpr)}"

    override def visitCallExpr(a: Expr.Call): String =
      s"${VisitorExpr.accept(a.callee, this)}"

    override def visitLambda(f: Expr.Lambda): String = "<lambda fn>"

    override def visitGet(expr: Expr.Get): String = s"${expr.name.lexeme}"

    override def visitSet(expr: Expr.Set): String =
      s"${expr.name.lexeme} = ${VisitorExpr.accept(expr.value, this)}"

    override def visitThis(expr: Expr.This): String = "this"

    override def visitSuper(expr: Expr.Super): String = "super"

def paren(name: String, exprs: Expr*): String =
  s"($name${exprs.map(expr => s" ${VisitorExpr.accept(expr, PrintAstVisitorExpr)}").mkString("")})"
