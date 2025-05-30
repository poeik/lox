package ast

import error.RuntimeError
import interpreter.{ Environment, Interpreter }
import token.Token

import scala.collection.mutable

enum Lit:
  case Nil
  case Number(value: Double)
  case Str(value: String)
  case Bool(value: Boolean)
  case Callable(fn: Fn)
  case Instance(klass: Fn.Class, fields: mutable.Map[String, Lit])

enum Fn:
    case Lox(
        body:          List[Stmt],
        params:        List[Token],
        closure:       Environment,
        isInitializer: Boolean
    )
    case Class(
        name:       String,
        superclass: Option[Fn.Class],
        methods:    Map[String, Fn.Lox]
    )
    case Native(
        fn: (
            interpreter: Interpreter,
            params: List[Lit]
        ) => Either[RuntimeError, Lit],
        arity: Int
    )

