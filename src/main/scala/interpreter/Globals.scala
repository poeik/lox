package interpreter

import error.RuntimeError
import ast.Lit
import ast.Fn.Native
import ast.Lit.{ Callable, Str }

object Globals {
  val globals: Environment = Environment(None)

  private def clock(
      interpreter: Interpreter,
      args:        List[Lit]
  ): Either[RuntimeError, Lit] = Right(Lit.Number(System.currentTimeMillis()))

  private def println(
      interpreter: Interpreter,
      args:        List[Lit]
  ): Either[RuntimeError, Lit] = {
    System.out.println(interpreter.stringify(args.head))
    Right(Lit.Nil)
  }

  globals.define("clock", Callable(Native(clock, 0)))
  globals.define("println", Callable(Native(println, 1)))
}
