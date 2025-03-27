package parser

import expr.Expr
import expr.Expr.{Binary, Grouping, Literal, Unary}
import token.TokenType.*
import token.{Token, TokenType}
import error as reporting

import scala.util.boundary

class Parser(private val tokens: Seq[Token]) {
  private class ParseError extends RuntimeException

  private var current = 0

  def parse(): Expr =
    try {
      expression()
    } catch
      case error: ParseError => null


  private def expression(): Expr = equality()

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
        case FALSE     => Literal(Some(false))
        case TRUE      => Literal(Some(true))
        case NUMBER(v) => Literal(Some(v))
        case STRING(v) => Literal(Some(v))
        case NIL =>       Literal(None)
        case LEFT_PAREN =>
          advance()
          val expr = expression()
          consume(RIGHT_PAREN, "Expect ')' after expression.")
          Grouping(expr)
        case _  =>
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

  private def isAtEnd: Boolean = tokens(current) == EOF

  private def peek(): Token = tokens(current)

  private def advance(): Token =
     if (!isAtEnd) current = current + 1
     previous()

  private def consume(tokenType: TokenType, message: String): Token =
    if (check(tokenType)) peek()
    else throw error(peek(), message)

  private def error(token: Token, message: String): ParseError = {
    reporting.error(token, message)
    ParseError()
  }

  def synchronize(): Unit =
     advance()
     while !isAtEnd do
        if (previous().tokenType == SEMICOLON) return
        else
          peek().tokenType match
             case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN =>
               return
             case _ => advance()
}
