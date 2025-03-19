package token


case class Token(
    tokenType: TokenType,
    lexeme:    String,
    line:      Int
)
