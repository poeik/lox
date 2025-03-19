package error

var hadError: Boolean = false

def error(line: Int, message: String): Unit =
  report(line, "", message)

def report(line: Int, where: String, message: String): Unit =
  println("[line " + line + "] Error" + where + ": " + message)
  hadError = true
