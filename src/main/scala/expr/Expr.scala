package expr

import error.RuntimeError
import token.Token

enum Fn:
   case Lox(body: List[Stmt], params: List[Token])
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

enum Expr:
   case Binary(left: Expr, operator: Token, right: Expr)
   case Call(callee: Expr, paren: Token, arguments: List[Expr])
   case Grouping(expr: Expr)
   case Literal(value: Lit)
   case Unary(operator: Token, right: Expr)
   case Variable(name: Token)
   case Assignment(name: Token, value: Expr)
   case Logical(left: Expr, operator: Token, right: Expr)
