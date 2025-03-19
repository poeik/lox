package token

import token.TokenType.*

enum TokenType:
   // Single-character tokens
   case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT,
     MINUS, PLUS, SEMICOLON, SLASH, STAR,

     // ONE OR TWO CHARACTER TOKENS
     BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS,
     LESS_EQUAL,

     // LITERALS
     IDENTIFIER,

     // KEYWORDS
     AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS,
     TRUE, VAR, WHILE, EOF

   case STRING(value: String)
   case NUMBER(value: Double)

val keywords = Map(
  "and" -> AND,
  "class" -> CLASS,
  "else" -> ELSE,
  "false" -> FALSE,
  "for" -> FOR,
  "fun" -> FUN,
  "if" -> IF,
  "nil" -> NIL,
  "or" -> OR,
  "print" -> PRINT,
  "return" -> RETURN,
  "super" -> SUPER,
  "this" -> THIS,
  "true" -> TRUE,
  "var" -> VAR,
  "while" -> WHILE
)
