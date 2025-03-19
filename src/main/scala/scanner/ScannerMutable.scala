package scanner

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import token.TokenType.*
import token.{Token, TokenType, keywords}
import error.error

class ScannerMutable(private val source: String):
   private val tokens: mutable.ListBuffer[Token] = ListBuffer()
   private var start: Int                        = 0
   private var current: Int                      = 0
   private var line: Int                         = 0

   def scanTokens(): Seq[Token] =
      while !isAtEnd do
         start = current
         scanToken()

      tokens += Token(EOF, "", line)
      tokens.toList

   private def isAtEnd: Boolean = current >= source.length

   private def scanToken(): Unit =
      val c = advance()
      c match {
        case '(' => addToken(LEFT_PAREN)
        case ')' => addToken(RIGHT_PAREN)
        case '{' => addToken(LEFT_BRACE)
        case '}' => addToken(RIGHT_BRACE)
        case ',' => addToken(COMMA)
        case '.' => addToken(DOT)
        case '-' => addToken(MINUS)
        case '+' => addToken(PLUS)
        case ';' => addToken(SEMICOLON)
        case '*' => addToken(STAR)
        case '!' => addToken(if nextIs('=') then BANG_EQUAL else BANG)
        case '=' => addToken(if nextIs('=') then EQUAL_EQUAL else EQUAL)
        case '<' => addToken(if nextIs('=') then LESS_EQUAL else LESS)
        case '>' => addToken(if nextIs('=') then GREATER_EQUAL else GREATER)
        case ' ' | '\r' | '\t' =>
        case '"'               => string()
        case '\n'              => line = line + 1
        case '/' =>
          if (nextIs('/'))
            while (peek() != '\n' && !isAtEnd) advance()
          else
            addToken(SLASH)
        case c if c.isDigit  => number()
        case c if c.isLetter => identifier()
        case x =>
          error(line, s"Unexpected character. $x")
      }

   private def string(): Unit =
      while peek() != '"' && !isAtEnd do
         if peek() == '\n' then line = line + 1
         advance()

      if isAtEnd then
         error(line, "Unterminated String")
         return

      advance() // consumes the closing "

      val value = source.substring(start + 1, current - 1)
      addToken(STRING(value))

   private def number(): Unit =
      while (peek().isDigit) advance()

      if peek() == '.' && peekNext().isDigit then
         advance() // consume the .
         while (peek().isDigit) advance()

      addToken(NUMBER(source.substring(start, current).toDouble))

   private def identifier(): Unit =
     while peek().isLetterOrDigit do advance()
     val text = source.substring(start, current)
     val maybeKeyword = keywords.get(text)
      
     addToken(maybeKeyword.getOrElse(IDENTIFIER))

   private def addToken(tokenType: TokenType): Unit =
      val text = source.substring(start, current)
      tokens += Token(tokenType, text, line)

   private def nextIs(expected: Char): Boolean =
      if (isAtEnd) return false
      if source.charAt(current) != expected then return false
      current = current + 1
      true

   private def peek(): Char =
     if isAtEnd then '\u0000' // originally it was \0
     else source.charAt(current)

   private def peekNext(): Char =
     if current + 1 >= source.length() then '\u0000' // originally it was \0
     else source.charAt(current + 1)

   private def advance(): Char =
      val c = source.charAt(current)
      current = current + 1
      c
