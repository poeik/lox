package parser

import error as reporting

import ast.Expr.*
import ast.Lit.Bool
import ast.Stmt.Var
import ast.{ Expr, Lit, Stmt }
import token.TokenType.*
import token.{ Token, TokenType }

import scala.util.boundary

/** This implementation is similar to the one in the book, but it uses
  * match-expression instead of the function `match` where useful. However, this
  * leads to cases where you need to manually advance(). This leads to cases
  * where you need to use `satisfies` instead of `consume`, because otherwise
  * you will consume a token too much.
  */
class Parser(private val tokens: Seq[Token]) {
  private class ParseError extends RuntimeException

  private var current = 0

  import scala.annotation.tailrec

  def parse(): List[Stmt] = {
    @tailrec
    def go(acc: List[Stmt]): List[Stmt] =
      if (!isAtEnd) go(declaration() :: acc)
      else acc.reverse
    go(Nil)
  }

  private def declaration(): Stmt =
    try
        if `match`(CLASS) then classDeclaration()
        else if `match`(FUN) then funDeclaration("function")
        else if `match`(VAR) then varDeclaration()
        else statement()
    catch
        case _ =>
          synchronize()
          null // TODO: hmmmmm....

  private def classDeclaration(): Stmt =
      val name = consume(IDENTIFIER, "Expect class name.")

      val superClass: Option[Expr.Variable] = if `match`(LESS) then
          consume(IDENTIFIER, "Expect superclass name.")
          Some(Expr.Variable(previous()))
      else None

      consume(LEFT_BRACE, "Expect '{' before class body.")

      val methods = Iterator
        .continually(())
        .takeWhile(_ => !check(RIGHT_BRACE) && !isAtEnd)
        .map(_ => funDeclaration("method"))
        .toList

      consume(RIGHT_BRACE, "Expect '}' after class body.")

      Stmt.Class(name, superClass, methods)

  private def lambda(kind: String): (List[Token], List[Stmt]) =
      @tailrec
      def parseParameters(params: List[Token]): List[Token] =
          if (params.size >= 255)
            error(peek(), "Can't have more than 255 arguments.")
          val token = consume(IDENTIFIER, "Expect parameter name.")
          val all   = params.appended(token)
          if `match`(COMMA) then parseParameters(all)
          else all

      consume(LEFT_PAREN, s"Expect '(' after $kind name.")
      val params = if !check(RIGHT_PAREN) then parseParameters(Nil) else Nil
      val paren  = consume(RIGHT_PAREN, "Expect ')' after params.")

      consume(LEFT_BRACE, s"Expect '{' before $kind body.")
      (params, block())

  private def funDeclaration(kind: String): Stmt.Function =
      val name            = consume(IDENTIFIER, s"Expect $kind name")
      val (params, block) = lambda(kind)
      Stmt.Function(name, params, block)

  private def varDeclaration(): Stmt =
      val name = consume(IDENTIFIER, "Expect variable name.")

      val initializer =
        if (`match`(EQUAL))
          expression()
        else Literal(Lit.Nil)

      consume(SEMICOLON, "Expect ';' after variable decalation")
      Var(name, initializer)

  private def statement(): Stmt =
    if (`match`(FOR)) forStatement()
    else if (`match`(PRINT)) printStatement()
    else if (`match`(RETURN)) returnStatement()
    else if (`match`(WHILE)) whileStatement()
    else if (`match`(IF)) ifStatement()
    else if (`match`(LEFT_BRACE)) Stmt.Block(block())
    else expressionStatement()

  /** This desugars a for loop into a while loop in the following way:
    *
    * `for (var b2 = 0; b2 <= 1000000; b2 = b2+1) print b2;`
    *
    * `var b2 = 0; while(b2 <= 1000000) { print b2; b2 = b2 + 1; }`
    *
    * So we only need to extend the parser and not the interpreter to support
    * for loops!
    */
  private def forStatement(): Stmt =
      consume(LEFT_PAREN, "Expect '(' after 'for'.")
      val initializer =
        if `match`(SEMICOLON) then None
        else if `match`(VAR) then Some(varDeclaration())
        else Some(expressionStatement())

      val optCondition = if !check(SEMICOLON) then Some(expression()) else None
      consume(SEMICOLON, "Expect ';' after loop condition")

      val increment = if !check(RIGHT_PAREN) then Some(expression()) else None
      consume(RIGHT_PAREN, "Expect ')' after for clauses")

      val body = increment match
          case Some(value) =>
            Stmt.Block(List(statement(), Stmt.Expression(value)))
          case None => statement()

      val condition = optCondition.getOrElse(Expr.Literal(Bool(true)))
      val loop      = Stmt.While(condition, body)

      initializer match
          case Some(value) => Stmt.Block(List(value, loop))
          case None        => loop

  private def block(): List[Stmt] =
      @tailrec
      def go(acc: List[Stmt]): List[Stmt] =
        // the check for isAtEnd is necessary because, the user might accidentally create a block that never ends.
        if (!check(RIGHT_BRACE) && !isAtEnd) go(declaration() :: acc)
        else
            consume(RIGHT_BRACE, "Expect '}' after block.")
            acc.reverse

      go(Nil)

  private def ifStatement() =
      consume(LEFT_PAREN, "Expect '(' after 'if'.")
      val condition = expression()
      consume(RIGHT_PAREN, "Expect ')' after if condition.")

      val thenBranch = statement()
      val elseBranch =
        if (`match`(ELSE)) Some(statement())
        else None

      Stmt.If(condition, thenBranch, elseBranch)

  private def printStatement() =
      val expr = expression()
      consume(SEMICOLON, "Expect ';' after value.")
      Stmt.Print(expr)

  private def returnStatement() =
      val keyword = previous()
      val returnValue =
        if !check(SEMICOLON) then expression() else Expr.Literal(Lit.Nil)
      consume(SEMICOLON, "Expect ',' after return value.")
      Stmt.Return(keyword, returnValue)

  private def whileStatement() =
      consume(LEFT_PAREN, "Expect '(' after 'while'.")
      val condition = expression()
      consume(RIGHT_PAREN, "Expect ')' after condition.")
      val body = statement()
      Stmt.While(condition, body)

  private def expressionStatement(): Stmt =
      val expr = expression()
      consume(SEMICOLON, "Expect ';' after value.")
      Stmt.Expression(expr)

  private def expression(): Expr = assignment()

  private def assignment(): Expr =
      val expr = or()

      if `match`(EQUAL) then
          val equals = previous()
          val value  = assignment()
          expr match
              case Variable(name) => Expr.Assignment(name, value)
              case get @ Get(_, _) =>
                Expr.Set(get.obj, get.name, value)
              case _ => throw error(equals, "Invalid assignment target.")
      else expr

  private def or(): Expr =
      @tailrec
      def go(expr: Expr): Expr =
        if !`match`(OR) then expr
        else
            val operator = previous()
            val right    = and()
            go(Expr.Logical(expr, operator, right))
      go(and())

  /** Just for fun: We parse the left hand side then, if we come across "and" we
    * parse the right hand side and store it with the operator in a tuple.
    *
    * Then we fold the whole (operator, equality) list into a single expression,
    * by taking the accumulator as the lhs of the operator.
    */
  private def and(): Expr = {
    val first = equality()

    val pairs: List[(Token, Expr)] =
      Iterator
        .continually(())
        .takeWhile(_ => `match`(AND))
        .map(_ => (previous(), equality()))
        .toList

    pairs.foldLeft(first) {
      case (leftExpr, (operator, rightExpr)) =>
        Expr.Logical(leftExpr, operator, rightExpr)
    }
  }

  private def equality(): Expr =
      var expr = comparison()

      while (`match`(BANG_EQUAL, EQUAL_EQUAL))
          val operator = previous()
          val right    = comparison()
          expr = Binary(expr, operator, right)

      expr

  private def comparison(): Expr =
      var expr = term()

      while (`match`(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL))
          val operator = previous()
          val right    = term()
          expr = Binary(expr, operator, right)

      expr

  private def term(): Expr =
      var expr = factor()

      while (`match`(MINUS, PLUS))
          val operator = previous()
          val right    = factor()
          expr = Binary(expr, operator, right)

      expr

  private def factor(): Expr =
      var expr = unary()
      while (`match`(SLASH, STAR))
          val operator = previous()
          val right    = unary()
          expr = Binary(expr, operator, right)
      expr

  private def unary(): Expr =
    peek().tokenType match
        case BANG | MINUS =>
          advance()
          val operator = previous()
          val right    = unary()
          Unary(operator, right)
        case _ => call()

  private def call(): Expr =
      @tailrec
      def go(expr: Expr): Expr =
        if `match`(LEFT_PAREN) then go(finishCall(expr))
        else if `match`(DOT) then
            val name = consume(IDENTIFIER, "Expect property name after '.'.")
            go(Expr.Get(expr, name))
        else expr

      go(primary())

  private def finishCall(callee: Expr) =
      @tailrec
      def parseArguments(args: List[Expr]): List[Expr] =
          if (args.size >= 255)
            error(peek(), "Can't have more than 255 arguments.")
          val expr = expression()
          val all  = args.appended(expr)
          if `match`(COMMA) then parseArguments(all)
          else all

      val args  = if !check(RIGHT_PAREN) then parseArguments(Nil) else Nil
      val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")

      Expr.Call(callee, paren, args)

  private def primary(): Expr =
      val res = peek().tokenType match
          case FALSE     => Literal(Lit.Bool(false))
          case TRUE      => Literal(Lit.Bool(true))
          case NUMBER(v) => Literal(Lit.Number(v))
          case STRING(v) => Literal(Lit.Str(v))
          case NIL       => Literal(Lit.Nil)
          case FUN =>
            advance()
            val (params, block) = lambda("anonymous function")
            current = current - 1
            Lambda(params, block)
          case THIS => This(peek())
          case IDENTIFIER =>
            Variable(peek())
          case LEFT_PAREN =>
            advance()
            val expr = expression()
            satisfies(RIGHT_PAREN, "Expect ')' after expression.")
            Grouping(expr)
          case x =>
            throw error(peek(), "Expect expression.")

      advance()
      res

  private def `match`(types: TokenType*): Boolean =
    boundary:
        for t <- types do
            if (check(t))
                advance()
                boundary.break(true)
        false

  private def previous(): Token = tokens(current - 1)

  private def check(t: TokenType): Boolean =
    if (isAtEnd)
      false
    else
      peek().tokenType == t

  private def isAtEnd: Boolean = tokens(current).tokenType == EOF

  private def peek(): Token = tokens(current)

  private def advance(): Token =
      if (!isAtEnd) current = current + 1
      previous()

  /*
   * Attention:
   * In the book this method is called `consume` and does not just peek but advance, meaning it moves the
   * cursor 1 step forward.
   */
  private def satisfies(tokenType: TokenType, message: String): Token =
    if (check(tokenType)) peek()
    else throw error(peek(), message)

  private def consume(tokenType: TokenType, message: String): Token =
      satisfies(tokenType, message)
      advance()

  private def error(token: Token, message: String): ParseError = {
    reporting.error(token, message)
    ParseError()
  }

  private def synchronize(): Unit =
      advance()
      while !isAtEnd do
          if (previous().tokenType == SEMICOLON) return
          else
            peek().tokenType match
                case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN =>
                  return
                case _ => advance()
}
