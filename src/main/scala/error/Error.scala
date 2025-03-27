package error

import token.Token
import token.TokenType.EOF

var hadError: Boolean = false

def error(line: Int, message: String): Unit =
  report(line, "", message)

def error(token: Token, message: String): Unit =
  token.tokenType match
    case EOF => report(token.line, " at end", message)
    case _ => report(token.line, s" at end '${token.lexeme}'", message)

def report(line: Int, where: String, message: String): Unit =
  println("[line " + line + "] Error" + where + ": " + message)
  hadError = true
  
