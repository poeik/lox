import TokenType.*

import scala.annotation.tailrec

case class ScanState(
    source: String,
    start:  Int         = 0,
    pos:    Int         = 0,
    line:   Int         = 1,
    tokens: List[Token] = Nil
)

object Scanner:
   private def loop[T](initial: T)(cond: T => Boolean)(body: T => T): T =
      @tailrec
      def go(previous: T): T =
        if (cond(previous)) go(body(previous)) else previous
      go(initial)

   def scanTokens(source: String): Seq[Token] =
      val initial = ScanState(source)
      val endState: ScanState = loop(initial)(s => !isAtEnd(s)) { current =>
         val currentState = current.copy(start = current.pos)
         scanToken(currentState)
      }
      endState.tokens :+ Token(EOF, "", endState.line)

   private def isAtEnd(scanState: ScanState): Boolean =
     scanState.pos >= scanState.source.length

   private def scanToken(initial: ScanState): ScanState =
      val (advancedState, c) = advance(initial)
      c match
         case '(' => addToken(advancedState, LEFT_PAREN)
         case ')' => addToken(advancedState, RIGHT_PAREN)
         case '{' => addToken(advancedState, LEFT_BRACE)
         case '}' => addToken(advancedState, RIGHT_BRACE)
         case ',' => addToken(advancedState, COMMA)
         case '.' => addToken(advancedState, DOT)
         case '-' => addToken(advancedState, MINUS)
         case '+' => addToken(advancedState, PLUS)
         case ';' => addToken(advancedState, SEMICOLON)
         case '*' => addToken(advancedState, STAR)
         case '!' =>
           val (nextState, result) = nextIs(advancedState, '=')
           addToken(nextState, if result then BANG_EQUAL else BANG)
         case '=' =>
           val (nextState, result) = nextIs(advancedState, '=')
           addToken(nextState, if result then EQUAL_EQUAL else EQUAL)
         case '<' =>
           val (nextState, result) = nextIs(advancedState, '=')
           addToken(nextState, if result then LESS_EQUAL else LESS)
         case '>' =>
           val (nextState, result) = nextIs(advancedState, '=')
           addToken(nextState, if result then GREATER_EQUAL else GREATER)
         case ' ' | '\r' | '\t' => advancedState
         case '"'               => string(advancedState)
         case '\n' => advancedState.copy(line = advancedState.line + 1)
         case '/' =>
           val (nextState, result) = nextIs(advancedState, '/')
           if (result)
             loop(nextState)(st => peek(st) != '\n' && !isAtEnd(st)) { st =>
               advance(st)._1
             }
           else
             addToken(nextState, SLASH)
         case c if c.isDigit  => number(advancedState)
         case c if c.isLetter => identifier(advancedState)
         case x =>
           error(advancedState.line, s"Unexpected character. $x")
           advancedState

   private def string(initial: ScanState): ScanState =
      val scanned = loop(initial)(s => peek(s) != '"' && !isAtEnd(s)) {
        current =>
           val nl =
             if peek(current) == '\n' then current.copy(line = current.line + 1)
             else current
           advance(nl)._1
      }

      if isAtEnd(scanned) then
         error(scanned.line, "Unterminated String")
         scanned
      else
         val (consumedQuote, _) = advance(scanned)

         val value = consumedQuote.source.substring(
           consumedQuote.start + 1,
           consumedQuote.pos - 1
         )
         addToken(consumedQuote, STRING(value))

   private def number(initial: ScanState): ScanState =
      val current = loop(initial)(peek(_).isDigit)(advance(_)._1)
      val num = if peek(current) == '.' && peekNext(current).isDigit then
         val (consumedDot, _) = advance(current)
         loop(consumedDot)(peek(_).isDigit)(advance(_)._1)
      else current

      addToken(num, NUMBER(num.source.substring(num.start, num.pos).toDouble))

   private def identifier(initial: ScanState): ScanState =
      val current      = loop(initial)(peek(_).isLetterOrDigit)(advance(_)._1)
      val text         = current.source.substring(current.start, current.pos)
      val maybeKeyword = keywords.get(text)
      addToken(current, maybeKeyword.getOrElse(IDENTIFIER))

   private def addToken(state: ScanState, tokenType: TokenType): ScanState =
      val text = state.source.substring(state.start, state.pos)
      state.copy(tokens = state.tokens :+ Token(tokenType, text, state.line))

   private def nextIs(state: ScanState, expected: Char): (ScanState, Boolean) =
      if (isAtEnd(state)) return (state, false)
      if state.source.charAt(state.pos) != expected then (state, false)
      else (state.copy(pos = state.pos + 1), true)

   private def peek(state: ScanState): Char =
     if isAtEnd(state) then '\u0000' // originally it was \0
     else state.source.charAt(state.pos)

   private def peekNext(state: ScanState): Char =
     if state.pos + 1 >= state.source.length() then
        '\u0000' // originally it was \0
     else state.source.charAt(state.pos + 1)

   private def advance(state: ScanState): (ScanState, Char) =
      val c = state.source.charAt(state.pos)
      (state.copy(pos = state.pos + 1), c)