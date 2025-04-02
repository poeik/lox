package expr

import error.{ runtimeError, RuntimeError }
import token.Token
import token.TokenType.*

object Interpreter extends Visitor[Either[RuntimeError, Any]]:

   def interpret(expr: Expr): Unit =
     evaluate(expr) match
        case Left(value)  => runtimeError(value)
        case Right(value) => println(stringify(value))

   override def visitBinary(b: Expr.Binary): Either[RuntimeError, Any] =
      val left  = evaluate(b.left)
      val right = evaluate(b.right)

      b.operator.tokenType match
         case GREATER =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => tpl._1 > tpl._2)
         case GREATER_EQUAL =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => tpl._1 >= tpl._2)
         case LESS =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => tpl._1 < tpl._2)
         case LESS_EQUAL =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => tpl._1 <= tpl._2)
         case MINUS =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => tpl._1 - tpl._2)
         case PLUS =>
           checkPlusOperands(b.operator, left, right).map {
             case (l: Double, r: Double) => l + r
             case (l: String, r: String) => l + r
           }
         case SLASH =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => tpl._1 / tpl._2)
         case STAR =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => tpl._1 * tpl._2)
         case BANG_EQUAL  => Right(!isEqual(left, right))
         case EQUAL_EQUAL => Right(isEqual(left, right))
         case _           => Left(RuntimeError(b.operator, "illegal state"))

   override def visitGrouping(g: Expr.Grouping): Either[RuntimeError, Any] =
     evaluate(g.expr)

   override def visitLiteral[B](l: Expr.Literal[B]): Either[RuntimeError, B] =
     Right(l.value)

   override def visitUnary(expr: Expr.Unary): Either[RuntimeError, Any] =
     for right <- evaluate(expr.right)
     yield expr.operator.tokenType match
        case BANG => !isTruthy(right)
        case MINUS =>
          checkNumberOperand(expr.operator, right)
            .map(-_)
        case _ => null

   private def checkNumberOperands(
       operator: Token,
       left:     Either[RuntimeError, Any],
       right:    Either[RuntimeError, Any]
   ): Either[RuntimeError, (Double, Double)] =
     for
        l <- left
        r <- right
        pair <- (l, r) match
           case (l: Double, r: Double) => Right(l, r)
           case _ => Left(RuntimeError(operator, "Operands must be numbers"))
     yield pair

   private def checkPlusOperands(
       operator: Token,
       left:     Either[RuntimeError, Any],
       right:    Either[RuntimeError, Any]
   ): Either[RuntimeError, (String, String) | (Double, Double)] =
     for
        l <- left
        r <- right
        pair <- (l, r) match
           case (l: String, r: String) => Right(l, r)
           case (l: Double, r: Double) => Right(l, r)
           case _ =>
             Left(
               RuntimeError(
                 operator,
                 "Operands must be two strings or two numbers."
               )
             )
     yield pair

   private def checkNumberOperand(
       operator: Token,
       operand:  Any
   ): Either[RuntimeError, Double] =
     operand match
        case d: Double => Right(d)
        case _ => Left(RuntimeError(operator, "Operand must be a number"))

   private def isEqual(l: Any, r: Any) =
     (l, r) match
        case (null, null)  => true
        case (null, _)     => false
        case (left, right) => left.equals(right)

   private def evaluate(expr: Expr): Either[RuntimeError, Any] =
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
