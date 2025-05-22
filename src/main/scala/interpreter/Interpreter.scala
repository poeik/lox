package interpreter

import error.{ runtimeError, RuntimeError }
import ast.Fn.Lox
import ast.*
import interpreter.Globals.globals
import token.TokenType.*
import token.{ Token, TokenType }

import scala.annotation.tailrec
import scala.collection.mutable

class Interpreter(
    val environment: Environment,
    val locals:      mutable.Map[Expr, Integer] = mutable.HashMap()
) extends VisitorExpr[Either[RuntimeError, Lit]]
    with VisitorStmt[Either[RuntimeError, Unit]]:

    def interpret(statements: List[Stmt]): Unit =
        def go(stmts: List[Stmt]): Unit =
          stmts match
              case head :: tail =>
                execute(head).fold(runtimeError, _ => go(tail))
              case Nil => ()
        go(statements)

    private def execute(stmt: Stmt): Either[RuntimeError, Unit] =
      VisitorStmt.accept(stmt, Interpreter(environment, locals))

    def resolve(expr: Expr, depth: Int): Option[Integer] =
      locals.put(expr, depth)

    override def visitBlock(
        stmt: Stmt.Block
    ): Either[RuntimeError, Unit] =
        val temp            = Environment(Some(environment))
        val tempInterpreter = Interpreter(temp, locals)
        tempInterpreter.executeBlock(stmt.statements)

    private def executeBlock(
        statements: List[Stmt]
    ): Either[RuntimeError, Unit] =
        val result =
          statements.foldLeft[Either[RuntimeError, Unit]](Right(()))(
            (acc, cur) => acc.flatMap(_ => execute(cur))
          )
        result

    override def visitPrint(stmt: Stmt.Print): Either[RuntimeError, Unit] =
      evaluate(stmt.expr).map(l => println(stringify(l)))

    override def visitReturnStatement(
        r: Stmt.Return
    ): Either[RuntimeError, Unit] =
      throw Return(evaluate(r.value))

    override def visitVarStmt(stmt: Stmt.Var): Either[RuntimeError, Unit] =
      for result <- evaluate(stmt.initializer)
      yield environment.define(stmt.name.lexeme, result)

    override def visitWhile(stmt: Stmt.While): Either[RuntimeError, Unit] = {
      @tailrec
      def go(): Either[RuntimeError, Unit] =
        evaluate(stmt.condition) match {
          case Left(err) => Left(err) // error in the condition detected
          case Right(c) if !isTruthy(c).value =>
            Right(()) // condition does not hold
          case Right(_) =>
            execute(stmt.body) match {
              case Left(err) => Left(err)
              case Right(_)  => go()
            }
        }
      go()
    }

//   this would be a much simpler version of while, however we will pretty fast run into a stackoverflow
//   override def visitWhile(stmt: Stmt.While): Either[RuntimeError, Unit] =
//      def go(): Either[RuntimeError, Unit] =
//        evaluate(stmt.condition).flatMap { c =>
//          if isTruthy(c).value then execute(stmt.body).flatMap(_ => go())
//          else Right(())
//        }
//      go()

    override def visitIf(stmt: Stmt.If): Either[RuntimeError, Unit] =
      evaluate(stmt.condition).flatMap { b =>
        if isTruthy(b).value then execute(stmt.thenBranch)
        else stmt.elseBranch.fold(Right(()))(execute)
      }

    override def visitExpressionStatement(
        stmt: Stmt.Expression
    ): Either[RuntimeError, Unit] = evaluate(stmt.expr).map(_ => ())

    override def visitFunctionStatement(
        f: Stmt.Function
    ): Either[RuntimeError, Unit] =
      Right(
        environment.define(
          f.name.lexeme,
          Lit.Callable(Fn.Lox(f.body, f.params, this.environment))
        )
      )

    override def visitAssignExpr(
        expr: Expr.Assignment
    ): Either[RuntimeError, Lit] =
      for value <- evaluate(expr.value)
      yield
          locals.get(expr) match {
            case Some(distance) =>
              environment.assignAt(distance, expr.name, value)
            case None => globals.assign(expr.name, value)
          }
          value

    override def visitLambda(f: Expr.Lambda): Either[RuntimeError, Lit] =
      Right(Lit.Callable(Fn.Lox(f.body, f.params, this.environment)))

    override def visitVariable(expr: Expr.Variable): Either[RuntimeError, Lit] =
      lookupVariable(expr.name, expr)

    private def lookupVariable(
        name: Token,
        expr: Expr.Variable
    ): Either[RuntimeError, Lit] =
      locals
        .get(expr)
        .map(d => environment.getAt(d, name))
        .getOrElse(globals.get(name))

    override def visitBinary(b: Expr.Binary): Either[RuntimeError, Lit] =
        val left  = evaluate(b.left)
        val right = evaluate(b.right)

        b.operator.tokenType match
            case GREATER =>
              checkNumberOperands(b.operator, left, right)
                .map(tpl => Lit.Bool(tpl._1 > tpl._2))
            case GREATER_EQUAL =>
              checkNumberOperands(b.operator, left, right)
                .map(tpl => Lit.Bool(tpl._1 >= tpl._2))
            case LESS =>
              checkNumberOperands(b.operator, left, right)
                .map(tpl => Lit.Bool(tpl._1 < tpl._2))
            case LESS_EQUAL =>
              checkNumberOperands(b.operator, left, right)
                .map(tpl => Lit.Bool(tpl._1 <= tpl._2))
            case MINUS =>
              checkNumberOperands(b.operator, left, right)
                .map(tpl => Lit.Number(tpl._1 - tpl._2))
            case PLUS =>
              checkPlusOperands(b.operator, left, right).map {
                case (l: Double, r: Double) => Lit.Number(l + r)
                case (l: String, r: String) => Lit.Str(l + r)
              }
            case SLASH =>
              checkNumberOperands(b.operator, left, right)
                .map(tpl => Lit.Number(tpl._1 / tpl._2))
            case STAR =>
              checkNumberOperands(b.operator, left, right)
                .map(tpl => Lit.Number(tpl._1 * tpl._2))
            case BANG_EQUAL =>
              isEqual(left, right).map(b => Lit.Bool(!b))
            case EQUAL_EQUAL => isEqual(left, right).map(b => Lit.Bool(b))
            case _           => Left(RuntimeError(b.operator, "illegal state"))

    override def visitGrouping(g: Expr.Grouping): Either[RuntimeError, Lit] =
      evaluate(g.expr)

    override def visitLiteral(l: Expr.Literal): Either[RuntimeError, Lit] =
      Right(l.value)

    override def visitLogicalExpr(
        expr: Expr.Logical
    ): Either[RuntimeError, Lit] =
      evaluate(expr.left).flatMap { left =>
        if expr.operator.tokenType == TokenType.OR then
            if isTruthy(left).value then Right(left)
            else evaluate(expr.right)
        else if (!isTruthy(left).value) Right(left)
        else evaluate(expr.right)
      }

    override def visitUnary(expr: Expr.Unary): Either[RuntimeError, Lit] =
        val right = evaluate(expr.right)
        expr.operator.tokenType match
            case BANG => right.map(r => Lit.Bool(!isTruthy(r).value))
            case MINUS =>
              right
                .flatMap(r => checkNumberOperand(expr.operator, r))
                .map(Lit.Number(_))
            case _ => Left(RuntimeError(expr.operator, "illegal state"))

    override def visitCallExpr(expr: Expr.Call): Either[RuntimeError, Lit] =
      for
          callee <- evaluate(expr.callee)
          args <- expr.arguments.foldLeft[Either[RuntimeError, List[Lit]]](
            Right(Nil)
          ) {
            case (Right(all), currentArg) =>
              evaluate(currentArg).map(lit => all :+ lit)
            case (err, _) => err
          }
          result <- callee match {
            case Lit.Callable(fn) =>
              val amtParams = amtOfParams(fn)
              if amtParams != args.size then
                  Left(
                    RuntimeError(
                      expr.paren,
                      s"Expected $amtParams arguments but got ${args.size}."
                    )
                  )
              else
                  fn match {
                    case loxFn @ Fn.Lox(body, _, _) =>
                      callLox(loxFn, args)
                    case nativeFn @ Fn.Native(fn, _) =>
                      callNative(nativeFn, args)
                  }
            case _ =>
              Left(
                RuntimeError(expr.paren, "Can only call functions and classes")
              )
          }
      yield result

    private def amtOfParams(fn: Fn) =
      fn match {
        case Fn.Lox(body, params, _) => params.size
        case Fn.Native(fn, arity)    => arity
      }

    private def callLox(
        function: Fn.Lox,
        args:     List[Lit]
    ): Either[RuntimeError, Lit] =
        val environment = Environment(Some(function.closure))
        function.params.zip(args).foreach {
          case (token, lit) =>
            environment.define(token.lexeme, lit)
        }
        val interpreter = Interpreter(environment, locals)
        try interpreter.executeBlock(function.body).map(_ => Lit.Nil)
        catch case e: Return => e.value

    private def callNative(function: Fn.Native, args: List[Lit]) =
      function.fn(this, args)

    private def checkNumberOperands(
        operator: Token,
        left:     Either[RuntimeError, Lit],
        right:    Either[RuntimeError, Lit]
    ): Either[RuntimeError, (Double, Double)] =
      for
          l <- left
          r <- right
          pair <- (l, r) match
              case (l: Lit.Number, r: Lit.Number) => Right(l.value, r.value)
              case _ => Left(RuntimeError(operator, "Operands must be numbers"))
      yield pair

    private def checkPlusOperands(
        operator: Token,
        left:     Either[RuntimeError, Lit],
        right:    Either[RuntimeError, Lit]
    ): Either[RuntimeError, (String, String) | (Double, Double)] =
      for
          l <- left
          r <- right
          pair <- (l, r) match
              case (l: Lit.Number, r: Lit.Number) => Right(l.value, r.value)
              case (l: Lit.Str, r: Lit.Str)       => Right(l.value, r.value)
              case _ =>
                Left(
                  RuntimeError(
                    operator,
                    "Operands must be two strings or two numbers."
                  )
                )
      yield pair

    private def checkNumberOperand(
        operator: Token,
        operand:  Lit
    ): Either[RuntimeError, Double] =
      operand match
          case Lit.Number(value) => Right(value)
          case _ => Left(RuntimeError(operator, "Operand must be a number"))

    private def isEqual(
        left:  Either[RuntimeError, Lit],
        right: Either[RuntimeError, Lit]
    ) =
      for
          l <- left
          r <- right
      yield (l, r) match
          case (Lit.Nil, Lit.Nil) => true
          case (Lit.Nil, _)       => false
          case (left, right)      => left.equals(right)

    private def evaluate(expr: Expr): Either[RuntimeError, Lit] =
      VisitorExpr.accept(expr, this)

    private def isTruthy(lit: Lit): Lit.Bool =
      Lit.Bool(lit match
          case Lit.Nil     => false
          case Lit.Bool(b) => b
          case _           => true
      )

    def stringify(lit: Lit): String =
      lit match
          case Lit.Nil => "nil"
          case Lit.Number(value) =>
            value.toString match
                case t if t.endsWith(".0") => t.substring(0, t.length() - 2)
                case t                     => t
          case Lit.Str(value)  => value
          case Lit.Bool(value) => value.toString
          case ast.Lit.Callable(f) =>
            f match {
              case Fn.Lox(_, _, _)  => "<fn lox>"
              case Fn.Native(fn, _) => "<fn native>"
            }
