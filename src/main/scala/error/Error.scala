package error

import token.Token
import token.TokenType.EOF

var hadError: Boolean        = false
var hadRuntimeError: Boolean = false

def error(line: Int, message: String): Unit =
  report(line, "", message)

def error(token: Token, message: String): Unit =
  token.tokenType match
      case EOF => report(token.line, " at end", message)
      case _   => report(token.line, s" at end '${token.lexeme}'", message)

def runtimeError(error: RuntimeError): Unit =
    println(s"${error.getMessage} \n[line ${error.token.line}]")
    hadRuntimeError = true

def report(line: Int, where: String, message: String): Unit =
    println("[line " + line + "] Error" + where + ": " + message)
    hadError = true

case class RuntimeError(token: Token, message: String)
    extends RuntimeException(message)
