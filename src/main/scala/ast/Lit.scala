package ast

import error.RuntimeError
import interpreter.{ Environment, Interpreter }
import token.Token

import scala.collection.mutable

enum Fn:
    case Lox(body: List[Stmt], params: List[Token], closure: Environment)
    case Class(name: String, methods: Map[String, Lit.Callable])
    case Native(
        fn: (
            interpreter: Interpreter,
            params: List[Lit]
        ) => Either[RuntimeError, Lit],
        arity: Int
    )

enum Lit:
    case Nil
    case Number(value: Double)
    case Str(value: String)
    case Bool(value: Boolean)
    case Callable(fn: Fn)
    case Instance(klass: Fn.Class, fields: mutable.Map[String, Lit])
