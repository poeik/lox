package resolver

import error as reporting

import ast.{ Expr, Stmt, VisitorExpr, VisitorStmt }
import interpreter.Interpreter
import resolver.FunctionType.Method
import token.Token

import scala.collection.mutable

private enum FunctionType:
    case None, Function, Method

private enum ClassType:
    case None, Class

class Resolver(private val interpreter: Interpreter)
    extends VisitorExpr[Unit]
    with VisitorStmt[Unit] {

  private val scopes: mutable.Stack[mutable.Map[String, Boolean]] =
    mutable.Stack.empty

  private var currentFunction = FunctionType.None
  private var currentClass    = ClassType.None

  override def visitBlock(b: Stmt.Block): Unit =
      beginScope()
      resolve(b.statements)
      endScope()

  override def visitClassStatement(stmt: Stmt.Class): Unit =
      val enclosingClass = currentClass
      currentClass = ClassType.Class

      declare(stmt.name)
      beginScope()

      scopes.top.put("this", true)
      stmt.methods.foreach(method =>
        resolveFunction(method.params, method.body, Method)
      )

      define(stmt.name)
      endScope()
      currentClass = enclosingClass;

  override def visitExpressionStatement(stmt: Stmt.Expression): Unit =
    resolve(stmt.expr)

  override def visitIf(stmt: Stmt.If): Unit = {
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    stmt.elseBranch.foreach(s => resolve(s))
  }

  override def visitPrint(stmt: Stmt.Print): Unit = resolve(stmt.expr)

  override def visitReturnStatement(stmt: Stmt.Return): Unit =
      if (currentFunction == FunctionType.None)
        reporting.error(stmt.keyword, "Can't return from top-level code.")
      resolve(stmt.value)

  override def visitFunctionStatement(stmt: Stmt.Function): Unit =
      declare(stmt.name)
      define(stmt.name)

      resolveFunction(stmt.params, stmt.body, FunctionType.Function)

  override def visitVarStmt(stmt: Stmt.Var): Unit =
      declare(stmt.name)
      resolve(stmt.initializer)
      define(stmt.name)

  override def visitWhile(stmt: Stmt.While): Unit =
      resolve(stmt.condition)
      resolve(stmt.body)

  override def visitAssignExpr(expr: Expr.Assignment): Unit =
      resolve(expr.value)
      resolveLocal(expr, expr.name)

  override def visitBinary(expr: Expr.Binary): Unit =
      resolve(expr.left)
      resolve(expr.right)

  override def visitCallExpr(expr: Expr.Call): Unit =
      resolve(expr.callee)
      expr.arguments.foreach(e => resolve(e))

  override def visitGet(expr: Expr.Get): Unit =
    resolve(expr.obj)

  override def visitGrouping(expr: Expr.Grouping): Unit = resolve(expr.expr)

  override def visitLiteral(expr: Expr.Literal): Unit = ()

  override def visitLogicalExpr(expr: Expr.Logical): Unit =
      resolve(expr.left)
      resolve(expr.right)

  override def visitSet(expr: Expr.Set): Unit =
      resolve(expr.value)
      resolve(expr.obj)

  override def visitThis(expr: Expr.This): Unit =
    currentClass match {
      case ClassType.None =>
        reporting.error(expr.keyword, "Can't use 'this' outside of a class.")
      case ClassType.Class => resolveLocal(expr, expr.keyword)
    }

  override def visitUnary(expr: Expr.Unary): Unit = resolve(expr.right)

  override def visitVariable(expr: Expr.Variable): Unit =
      if (scopes.nonEmpty && !scopes.top.getOrElse(expr.name.lexeme, true))
        reporting.error(
          expr.name,
          "Can't read local variable in its own initializer."
        )

      resolveLocal(expr, expr.name)

  override def visitLambda(expr: Expr.Lambda): Unit =
    resolveFunction(expr.params, expr.body, FunctionType.Function)

  def resolve(statements: List[Stmt]): Unit =
    statements.foreach(s => resolve(s))

  private def resolve(stmt: Stmt): Unit = VisitorStmt.accept(stmt, this)

  private def resolve(expr: Expr): Unit = VisitorExpr.accept(expr, this)

  private def resolveFunction(
      params:       List[Token],
      body:         List[Stmt],
      functionType: FunctionType
  ): Unit = {
    val enclosingFunction: FunctionType = currentFunction
    currentFunction = functionType

    beginScope()
    params.foreach { p =>
        declare(p)
        define(p)
    }
    resolve(body)
    endScope()
    currentFunction = enclosingFunction
  }

  private def beginScope(): Unit =
    scopes.push(mutable.HashMap[String, Boolean]())

  private def endScope() = scopes.pop()

  private def declare(name: Token): Unit =
    if scopes.nonEmpty then
        if (scopes.top.contains(name.lexeme))
            reporting.error(
              name,
              "Already a variable with this name in this scope."
            )
            val scope = scopes.top
            scope.put(name.lexeme, false)

  private def define(name: Token): Unit =
    if scopes.nonEmpty
    then scopes.top.update(name.lexeme, true)

  private def resolveLocal(expr: Expr, name: Token): Unit =
      val depth = scopes.indexWhere(_.contains(name.lexeme))
      if (depth >= 0) {
        interpreter.resolve(expr, depth)
      }
}
