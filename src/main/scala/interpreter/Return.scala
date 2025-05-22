package interpreter

import error.RuntimeError
import ast.Lit

case class Return(value: Either[RuntimeError, Lit])
    extends RuntimeException(null, null, false, false)
