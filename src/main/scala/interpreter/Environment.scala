package interpreter

import error.RuntimeError
import ast.Lit
import token.Token

class Environment(private val enclosing: Option[Environment]):
    private var values: Map[String, Lit] = Map()

    def define(name: String, value: Lit): Unit =
      values = values + (name -> value)

    private def ancestor(
        distance: Int,
        name:     Token
    ): Either[RuntimeError, Environment] =
      (0 until distance).foldLeft[Either[RuntimeError, Environment]](
        Right(this)
      )((env, _) =>
        env.flatMap(
          _.enclosing.toRight(
            RuntimeError(
              name,
              "did not find environment containing the requested variable"
            )
          )
        )
      )

    def get(name: Token): Either[RuntimeError, Lit] =
      values
        .get(name.lexeme)
        .orElse(lookupEnclosing(name))
        .toRight(RuntimeError(name, s"Undefined variable '${name.lexeme}'."))

    def getAt(distance: Int, name: Token): Either[RuntimeError, Lit] =
      ancestor(distance, name).map(_.values(name.lexeme))

    def assignAt(
        distance: Int,
        name:     Token,
        value:    Lit
    ): Either[RuntimeError, Lit] =
      ancestor(distance, name)
        .map(env => env.values = env.values + (name.lexeme -> value))
        .map(_ => value)

    def assign(name: Token, value: Lit): Either[RuntimeError, Lit] =
      if values.contains(name.lexeme) then
          values = values.updated(name.lexeme, value)
          Right(value)
      else
          enclosing.fold(
            Left(RuntimeError(name, s"Undefined variable '${name.lexeme}'."))
          )(env => env.assign(name, value))

    private def lookupEnclosing(name: Token) =
      enclosing.flatMap(e =>
        e.get(name) match
            case Left(_)      => None
            case Right(value) => Some(value)
      )
