package shellshock

class Parser(input: String) {
  // length of input
  private val length = input.length
  
  // *********************** basic access of input
  private object LowLevelParseHelpers {
    // class invariant: 0 <= pos <= length
    
    /** exception object for parse error */
    case class Error(msg: String) extends java.lang.Exception(msg + " at position " + pos + ", found " + input.substring(pos))
  
    // current position in input - every call to a parseX method parses from this position and increments the position accordingly
    private var pos = 0
  
    // are we at end of input?
    def endOfInput = pos == length
    
    // does input start with s at current position?
    def startsWith(s: String) = pos+s.length <= length && input.substring(pos, pos+s.length) == s
    
    // *********************** low-level parsing function that actually inspect the input
    
    // parse a terminal symbol t (and throw it away)
    def parseTerminal(t: String) = {
       parseWhitespace
       if (startsWith(t)) {
         pos += t.length
       } else {
         throw Error("expected " + t)
       }
       parseWhitespace
    }
    
    // parse and drop initial whitespace (and throw it away)
    def parseWhitespace {
      while (pos < length && input(pos).isWhitespace && input(pos) != '\n') {
        pos += 1
      }
    }
  
    // parse some characters (as long as they satisfy a condition) and return them
    def parseCertainCharacters(take: Char => Boolean): String = {
      parseWhitespace
      val p = pos
      while (pos < length && take(input(pos))) {
         pos += 1
      }
      val n = input.substring(p, pos)
      n
    }
    
    // this defines which characters are allowed in names
    def parseNameCharacters = parseCertainCharacters(c => c.isLetter)
    
    // e.g., for integer literals 
    def parseDigits = parseCertainCharacters(c => c.isDigit)
    
    // e.g., for string literals
    def parseEscapedString: String = {
      var s = ""
      parseTerminal("\"")
      var notDone = true
      
      while (!endOfInput && notDone) {
        if (startsWith("\\")) {
          ??? //TODO
        } else if (startsWith("\"")) {
          ???//TODO
        } else {
          s += input(pos)
          pos += 1
        }
      }
      
      parseTerminal("\"")
      s
    }
  }  

  // convenience: make all methods in available without qualification
  import LowLevelParseHelpers._
  
  // now one parse function for every non-terminal
  
  def parseCommand: Comm = {
    val comm = if (startsWith("fun ")) {
      parseTerminal("fun")
      val funName = parseName
      parseTerminal("(")
      val argName = parseName
      parseTerminal(")")
      parseTerminal("{")
      val body = parseCommand
      FunDef(funName, argName, body)
    } else if (startsWith("run ")) {
      ??? // TDODO
    } else {
      val funName = parseName
      parseTerminal("(")
      val arg = parseExpr
      parseTerminal(")")
      Apply(funName, arg)
    }
    if (startsWith(";")) {
      parseTerminal(";")
      val second = parseCommand
      Sequence(comm, second)
    } else {
      comm
    }
  }
  
  def parseExpr: Expr = {
    if (startsWith("\"")) {
      // read characters until the matching quote
      val s = parseEscapedString
      StringExpr(s)
    } else {
      val n = parseNameCharacters
      NameRef(Name(n))
    }
  }
  
  def parseName: Name = {
    val s = parseNameCharacters
    Name(s)
  }
  
}