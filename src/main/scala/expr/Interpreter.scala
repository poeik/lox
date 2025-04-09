package expr

import error.{ runtimeError, RuntimeError }
import token.Token
import token.TokenType.*

object Interpreter
    extends VisitorExpr[Either[RuntimeError, Lit]]
    with VisitorStmt[Either[RuntimeError, Unit]]:

   def interpret(statements: List[Stmt]): Unit =
      def go(stmts: List[Stmt]): Unit =
        stmts match
           case head :: tail => execute(head).fold(runtimeError, _ => go(tail))
           case Nil          => ()
      go(statements)

   private def execute(stmt: Stmt): Either[RuntimeError, Unit] =
     VisitorStmt.accept(stmt, this)

   override def visitPrint(stmt: Stmt.Print): Either[RuntimeError, Unit] =
     evaluate(stmt.expr).map(l => println(stringify(l)))

   override def visitExpressionStatement(
       stmt: Stmt.Expression
   ): Either[RuntimeError, Unit] = evaluate(stmt.expr).map(_ => ())

   override def visitBinary(b: Expr.Binary): Either[RuntimeError, Lit] =
      val left  = evaluate(b.left)
      val right = evaluate(b.right)

      b.operator.tokenType match
         case GREATER =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => Lit.Bool(tpl._1 > tpl._2))
         case GREATER_EQUAL =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => Lit.Bool(tpl._1 >= tpl._2))
         case LESS =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => Lit.Bool(tpl._1 < tpl._2))
         case LESS_EQUAL =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => Lit.Bool(tpl._1 <= tpl._2))
         case MINUS =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => Lit.Number(tpl._1 - tpl._2))
         case PLUS =>
           checkPlusOperands(b.operator, left, right).map {
             case (l: Double, r: Double) => Lit.Number(l + r)
             case (l: String, r: String) => Lit.Str(l + r)
           }
         case SLASH =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => Lit.Number(tpl._1 / tpl._2))
         case STAR =>
           checkNumberOperands(b.operator, left, right)
             .map(tpl => Lit.Number(tpl._1 * tpl._2))
         case BANG_EQUAL =>
           isEqual(left, right).map(b => Lit.Bool(!b))
         case EQUAL_EQUAL => isEqual(left, right).map(b => Lit.Bool(b))
         case _           => Left(RuntimeError(b.operator, "illegal state"))

   override def visitGrouping(g: Expr.Grouping): Either[RuntimeError, Lit] =
     evaluate(g.expr)

   override def visitLiteral(l: Expr.Literal): Either[RuntimeError, Lit] =
     Right(l.value)

   override def visitUnary(expr: Expr.Unary): Either[RuntimeError, Lit] =
      val right = evaluate(expr.right)
      expr.operator.tokenType match
         case BANG => right.map(r => Lit.Bool(!isTruthy(r).value))
         case MINUS =>
           right
             .flatMap(r => checkNumberOperand(expr.operator, r))
             .map(Lit.Number(_))
         case _ => Left(RuntimeError(expr.operator, "illegal state"))

   private def checkNumberOperands(
       operator: Token,
       left:     Either[RuntimeError, Lit],
       right:    Either[RuntimeError, Lit]
   ): Either[RuntimeError, (Double, Double)] =
     for
        l <- left
        r <- right
        pair <- (l, r) match
           case (l: Lit.Number, r: Lit.Number) => Right(l.value, r.value)
           case _ => Left(RuntimeError(operator, "Operands must be numbers"))
     yield pair

   private def checkPlusOperands(
       operator: Token,
       left:     Either[RuntimeError, Lit],
       right:    Either[RuntimeError, Lit]
   ): Either[RuntimeError, (String, String) | (Double, Double)] =
     for
        l <- left
        r <- right
        pair <- (l, r) match
           case (l: Lit.Number, r: Lit.Number) => Right(l.value, r.value)
           case (l: Lit.Str, r: Lit.Str)       => Right(l.value, r.value)
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
       operand:  Lit
   ): Either[RuntimeError, Double] =
     operand match
        case Lit.Number(value) => Right(value)
        case _ => Left(RuntimeError(operator, "Operand must be a number"))

   private def isEqual(
       left:  Either[RuntimeError, Lit],
       right: Either[RuntimeError, Lit]
   ) =
     for
        l <- left
        r <- right
     yield (l, r) match
        case (Lit.Nil, Lit.Nil) => true
        case (Lit.Nil, _)       => false
        case (left, right)      => left.equals(right)

   private def evaluate(expr: Expr): Either[RuntimeError, Lit] =
     VisitorExpr.accept(expr, Interpreter)

   private def isTruthy(lit: Lit): Lit.Bool =
     Lit.Bool(lit match
        case Lit.Nil     => false
        case Lit.Bool(b) => b
        case _           => true
     )

   private def stringify(lit: Lit): String =
     lit match
        case Lit.Nil => "nil"
        case Lit.Number(value) =>
          value.toString match
             case t if t.endsWith(".0") => t.substring(0, t.length() - 2)
             case t                     => t
        case Lit.Str(value)  => value
        case Lit.Bool(value) => value.toString
