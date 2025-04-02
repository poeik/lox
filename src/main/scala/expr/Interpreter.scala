package expr

import error.{RuntimeError, runtimeError}
import token.Token
import token.TokenType.*

object Interpreter extends Visitor[Any]:

  def interpret(expr: Expr): Unit =
    try
       val value = evaluate(expr)
       println(stringify(value))
    catch case e: RuntimeError => runtimeError(e)

  override def visitBinary(b: Expr.Binary): Any =
     val left  = evaluate(b.left)
     val right = evaluate(b.right)

     b.operator.tokenType match
        case GREATER =>
          val (l, r) = checkNumberOperands(b.operator, left, right)
          l > r
        case GREATER_EQUAL =>
          val (l, r) = checkNumberOperands(b.operator, left, right)
          l >= r
        case LESS =>
          val (l, r) = checkNumberOperands(b.operator, left, right)
          l < r
        case LESS_EQUAL =>
          val (l, r) = checkNumberOperands(b.operator, left, right)
          l <= r
        case MINUS =>
          val (l, r) = checkNumberOperands(b.operator, left, right)
          l - r
        case PLUS =>
          val ops = checkPlusOperands(b.operator, left, right)
          ops match
             case (l: Double, r: Double) => l + r
             case (l: String, r: String) => l + r

        case SLASH =>
          val (l, r) = checkNumberOperands(b.operator, left, right)
          l / r
        case STAR =>
          val (l, r) = checkNumberOperands(b.operator, left, right)
          l * r
        case BANG_EQUAL  => !isEqual(left, right)
        case EQUAL_EQUAL => isEqual(left, right)
        case _           => null

  override def visitGrouping(g: Expr.Grouping): Any =
    evaluate(g.expr)

  override def visitLiteral[B](l: Expr.Literal[B]): B = l.value.get

  override def visitUnary(expr: Expr.Unary): Any =
     val right = evaluate(expr)

     expr.operator.tokenType match
        case BANG => !isTruthy(right)
        case MINUS =>
          val r = checkNumberOperand(expr.operator, right)
          -r
        case _ => null

  private def checkNumberOperands(
      operator: Token,
      left:     Any,
      right:    Any
  ): (Double, Double) =
    (left, right) match
       case (l: Double, r: Double) => (l, r)
       case _ => throw RuntimeError(operator, "Operands must be numbers")

  private def checkPlusOperands(
      operator: Token,
      left:     Any,
      right:    Any
  ): (String, String) | (Double, Double) =
    (left, right) match
       case (l: String, r: String) => (l, r)
       case (l: Double, r: Double) => (l, r)
       case _ =>
         throw RuntimeError(
           operator,
           "Operands must be two strings or two numbers."
         )

  private def checkNumberOperand(operator: Token, operand: Any): Double =
    operand match
       case d: Double => d
       case _ => throw new RuntimeError(operator, "Operand must be a number")

  private def isEqual(l: Any, r: Any) =
    (l, r) match
       case (null, null)  => true
       case (null, _)     => false
       case (left, right) => left.equals(right)

  private def evaluate(expr: Expr): Any =
    Visitor.accept(expr, Interpreter)

  private def isTruthy(any: Any): Boolean =
    any match
       case null       => false
       case b: Boolean => b
       case _          => true

  private def stringify(any: Any): String =
    any match
       case null => "nil"
       case any: Double =>
         any.toString match
            case t if t.endsWith(".0") => t.substring(0, t.length() - 2)
            case t                     => t
       case a => a.toString
