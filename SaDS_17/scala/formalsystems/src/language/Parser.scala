package language

/** @param input the string to parse
 *  
 *  after creating the parser, call exactly one parsing method depending on the expected concpet
 */
class Parser(input: String) {
   
  /** exception object for parse error */
  case class Error(msg: String) extends java.lang.Exception(msg + " at position " + pos + ", found " + input.substring(pos))
  
  // *********************** basic access of input

  // length of input
  private val length = input.length
  // current position in input - every call to a parseX method parses from this position and increments the position accordingly
  private var pos = 0

  // are we at end of input?
  private def endOfInput = pos == length
  
  // does input start with s at current position?
  private def startsWith(s: String) = pos+s.length <= length && input.substring(pos, pos+s.length) == s
  
  // *********************** low-level parsing function that actually inspect the input
  
  // parse a terminal symbol t (and throw it away)
  private def parseTerminal(t: String) = {
     parseWhitespace
     if (startsWith(t)) {
       pos += t.length
     } else {
       throw Error("expected " + t)
     }
     parseWhitespace
  }
  
  // parse and drop initial whitespace (and throw it away)
  private def parseWhitespace {
    while (pos < length && input(pos).isWhitespace && input(pos) != '\n') {
      pos += 1
    }
  }

  // parse some characters (as long as they satisfy a condition) and return them
  private def parseCertainCharacters(take: Char => Boolean): String = {
    parseWhitespace
    val p = pos
    while (pos < length && take(input(pos))) {
       pos += 1
    }
    val n = input.substring(p, pos)
    n
  }
  
  // this defines which characters are allowed in names
  private def parseNameCharacters = parseCertainCharacters(c => c.isLetter)
  
  // e.g., for integer literals 
  private def parseDigits = parseCertainCharacters(c => c.isDigit)
  
  // *********************** parser combinators
  
  // these are operators on parsers that return new parsers
  
  // parses a non-empty list of C's (each one parsed by calling parseC) separated by sep
  private def parseList[C](parseC: () => C, sep: String): List[C] = {
    var cs: List[C] = Nil
    var done = false
    while (!done) {
      val c = parseC()
      cs ::= c
      parseWhitespace
      if (startsWith(sep)) {
        parseTerminal(sep)
      } else {
        done = true
      }
    }
    cs.reverse
  }
  
  // parses an alternative (C1 | C2): tries parseC1 first, if that fails uses parseC2
  private def parseChoice[C](parseC1: () => C, parseC2: () => C): C = {
    val p = pos
    try {
      parseC1()
    } catch {case Error(_) =>
      pos = p // backtrack to original position
      parseC2()
    } 
  }
  
  // parses a C, then applies a function to return a D
  private def map[C,D](parseC: () => C, f: C => D): D = {
    val c = parseC()
    f(c)
  }

  // *********************** high-level parse functions: one function parseN per non-terminal symbol N
  
  def parseContext: Context = {
    parseWhitespace
    val decls = if (endOfInput) {
      Nil
    } else {
      parseList[Decl](() => parseDecl, "\n")
    }
    parseWhitespace
    if (!endOfInput)
      throw Error("expected end of input")
    Context(decls)
  }
   
  def parseDecl: Decl = {
    parseWhitespace
    if (startsWith("\n")) {
      // skip empty line
      parseTerminal("\n")
      parseDecl
    } else if (startsWith("val ")) {
      parseTerminal("val")
      parseVal
    } else {
      throw Error("keyword expected")
    }
  }
  
  // auxiliary function of parseDecl
  private def parseVal: Val = {
    val n = parseName
    parseTerminal(":")
    val t = parseType
    parseTerminal("=")
    val v = parseTerm
    Val(n,t,Some(v))
  }
  
  def parseType: Type = {
    parseWhitespace
    if (endOfInput)
      throw Error("no type found")
    val tp = if (startsWith("(")) {
      // bracketed type
      parseTerminal("(")
      val tp = parseType
      parseTerminal(")")
      tp
    } else {
      // name
      val n = parseNameCharacters
      // some names are predefined, everything else is a TypeRef
      n match {
        case "int" => Int()
        case "bool" => Bool()
        case n =>
          val name = Name(n)
          TypeRef(name)
      }
    }
    // we have a type now; we check what comes next to decide if we should parse more
    //TODO for product types: handle infix operator *
    val infixOps = List("->")
    parseWhitespace
    // check if an infix operator follows 
    infixOps.find(startsWith) match {
      case Some(op) =>
        // infix operator follows: continue
        parseTerminal(op)
        val second = parseType
        op match {
          case "->" => FunType(tp,second)
        }
      case None =>
        // something else follows: done
        tp
    }
  }

  def parseTerm: Term = {
    parseWhitespace
    if (endOfInput)
      throw Error("no term found")
    val c = input(pos)
    val tm = if (c == '(') {
      // bracketed term
      parseTerminal("(")
      val tm = parseTerm
      parseTerminal(")")
      tm
    } else if (c.isDigit) {
      // integer literal
      val i = parseDigits.toInt
      IntLit(i)
    } else if (c == '"') {
      // string literal
      ??? //TODO for strings
    } else if (c == '{') {
      // local declaration
      parseTerminal("{")
      val d = parseDecl
      parseTerminal(";")
      val t = parseTerm
      parseTerminal("}")
      LocalDecl(d, t)
    } else {
      // name, some names are predefined or special, everything else is a TermRef
      val n = parseNameCharacters
      n match {
        case "true" =>
          BoolLit(true)
        case "false" =>
          BoolLit(false)
        case "if" =>
          ??? //TODO
        case _ =>
          TermRef(Name(n))
      }
    }
    // we have a term now; we check what comes next to decide if we should parse more 
    parseWhitespace
    if (startsWith(".")) {
      // projections out of tm
      parseTerminal(".")
      val n = parseDigits
      ??? //TODO for product types
    } else if (startsWith("(")) {
      // function application of tm
      parseTerminal("(")
      val arg = parseTerm
      parseTerminal(")")
      Apply(tm, arg)
    } else if (startsWith("[")) {
      // TODO for type arguments
      ???
    } else if (startsWith(":")) {
      // lambda abstraction, but only if tm is a variable
      tm match {
        case TermRef(x) =>
          parseTerminal(":")
          val tp = parseType
          parseTerminal("=>")
          val body = parseTerm
          Lambda(x,tp,body)
        case _ =>
          tm
      }
    } else {
      // check if an infix operator follows 
      Operator.builtInInfixOperators.find(startsWith) match {
        case Some(op) =>
          // infix operator
          parseTerminal(op)
          val second = parseTerm
          Operator(op, List(tm, second))
        case None =>
            tm
      }
    }
  }
  
  def parseName: Name = {
    val n = parseNameCharacters
    if (n == "")
      throw Error("empty name")
    Name(n)
  }
  

  /* not needed, but might come in handy later
  // parses [A_1, ..., A_n], can h
  private def parseTypeArgs: List[Type] = {
    if (startsWith("[")) {
      parseTerminal("[")
      parseWhitespace
      if (startsWith("]")) {
        Nil
      } else {
        val tps = parseList[Type](() => parseType, ",")
        parseTerminal("]")
        tps
      }
    } else {
      Nil
    }
  }
  
  // parses [a_1, ..., a_n]
  private def parseTypeVars: List[Name] = {
    if (startsWith("[")) {
      parseTerminal("[")
      parseWhitespace
      if (startsWith("]")) {
        Nil
      } else {
        val vs = parseList[Name](() => parseName, ",")
        parseTerminal("]")
        vs
      }
    } else {
      Nil
    }
  }
  */
}