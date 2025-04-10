import error.{ hadError, hadRuntimeError }
import expr.{ Expr, Interpreter, VisitorExpr }
import parser.Parser
import scanner.{ Scanner, ScannerMutable }
import token.TokenType.EOF
import token.{ Token, TokenType }

import java.nio.file.Files
import java.nio.file.Paths
import java.io.InputStreamReader
import java.io.BufferedReader
import scala.util.Using
import java.nio.charset.Charset

@main def main(args: String*): Unit =
  args.toList match
     case Nil =>
       runPrompt()
     case filePath :: Nil =>
       runFile(filePath)
     case _ =>
       println("Usage: jlox [script]")
       System.exit(64)

def runFile(path: String): Unit =
   val content = Files.readString(Paths.get(path), Charset.defaultCharset())
   run(content)
   if (hadError) System.exit(65)
   if (hadRuntimeError) System.exit(70)

def runPrompt(): Unit =
  Using.resource(new BufferedReader(InputStreamReader(System.in))) { reader =>
    Iterator
      .continually {
        print("> ")
        reader.readLine()
      }
      .takeWhile(_ != null)
      .foreach { line =>
         run(line)
         hadError = false
      }
  }

def runMutable(source: String): Unit =
   val scanner            = new ScannerMutable(source)
   val tokens: Seq[Token] = scanner.scanTokens()
   tokens.foreach(println(_))

def run(source: String): Unit =
   val tokens: Seq[Token] = Scanner.scanTokens(source)
   val parser             = new Parser(tokens)
   val statements         = parser.parse()
   if hadError then ()
   else Interpreter().interpret(statements)
