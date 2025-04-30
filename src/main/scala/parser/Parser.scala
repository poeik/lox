package parser

import expr.{ Expr, Lit, Stmt }
import expr.Expr.{ Binary, Grouping, Literal, Unary, Variable }
import token.TokenType.*
import token.{ Token, TokenType }
import error as reporting

import expr.Stmt.Var

import scala.util.boundary

/*
 * This implementation is similar to the one in the book, but it uses match-expression instead of the function `match`
 * where useful. However, this leads to cases where you need to manually advance(). This leads to cases where you need
 * to use `satisfies` instead of `consume`, because otherwise you will consume a token too much.
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
       if (`match`(VAR)) varDeclaration()
       else statement()
    catch
       case _ =>
         synchronize()
         null // TODO: hmmmmm....

  private def varDeclaration(): Stmt =
     val name = consume(IDENTIFIER, "Expect variable name.")

     val initializer =
       if (`match`(EQUAL))
         expression()
       else Literal(Lit.Nil)

     consume(SEMICOLON, "Expect ';' after variable decalation")
     Var(name, initializer)

  private def statement(): Stmt =
    if (`match`(PRINT)) printStatement()
    else if (`match`(IF)) ifStatement()
    else if (`match`(LEFT_BRACE)) Stmt.Block(block())
    else expressionStatement()

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
       case _ => primary()

  private def primary(): Expr =
     val res = peek().tokenType match
        case FALSE => Literal(Lit.Bool(false))
        case TRUE  => Literal(Lit.Bool(true))
        case NUMBER(v) =>
          Literal(Lit.Number(v))
        case STRING(v) => Literal(Lit.Str(v))
        case NIL       => Literal(Lit.Nil)
        case IDENTIFIER =>
          Variable(peek())
        case LEFT_PAREN =>
          advance()
          val expr = expression()
          satisfies(RIGHT_PAREN, "Expect ')' after expression.")
          Grouping(expr)
        case _ =>
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
   * in the boo
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
