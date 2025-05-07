package interpreter

import error.RuntimeError
import expr.Lit

case class Return(value: Either[RuntimeError, Lit])
    extends RuntimeException(null, null, false, false)