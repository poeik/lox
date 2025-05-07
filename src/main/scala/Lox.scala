import error.{hadError, hadRuntimeError}
import expr.{Environment, Interpreter}
import interpreter.Globals
import parser.Parser
import scanner.Scanner
import token.Token

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.util.Using

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

def run(source: String): Unit =
   val tokens: Seq[Token] = Scanner.scanTokens(source)
   val parser             = new Parser(tokens)
   val statements         = parser.parse()
   if hadError then ()
   else
      val env = Environment(Some(Globals.globals))
      Interpreter(env).interpret(statements)
