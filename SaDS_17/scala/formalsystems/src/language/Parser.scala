package language

class Parser(input: String) {
   
  // exception object for error messages
  case class Error(msg: String) extends java.lang.Exception(msg + " at position " + pos + ", found " + input.substring(pos))
  
  // basic access of input
  
  private val length = input.length
  // current position in input
  private var pos = 0

  // are we at end of input?
  private def endOfInput = pos == length
  
  // does input start with s at current position?
  private def startsWith(s: String) = pos+s.length <= length && input.substring(pos, pos+s.length) == s
  
  // parse terminal symbols and whitespace
  
  // parse a terminal symbol t
  private def parseTerminal(t: String) = {
     parseWhitespace
     if (startsWith(t)) {
       pos += t.length
     } else {
       throw Error("expected " + t)
     }
     parseWhitespace
  }
  
  // parse and drop initial whitespace
  private def parseWhitespace {
    while (pos < length && input(pos).isWhitespace && input(pos) != '\n') {
      pos += 1
    }
  }

  // parser combinators
  
  // parses a non-empty list of C's separated by sep
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

  // one parse function per non-terminal
  
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
    if (startsWith("val")) {
      parseTerminal("val")
      parseVal
    } else {
      throw Error("expected keyword")
    }
  }
  
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
    if (startsWith("(")) {
      // parses (A_1 op A_2) for some op in infixOps
      val infixOps = List("->", "*")
      parseTerminal("(")
      val first = parseType
      parseWhitespace
      val op = infixOps.find(startsWith) match {
        case Some(op) => op
        case None => throw Error("unknown infix operator in type")
      }
      parseTerminal(op)
      val second = parseType
      parseTerminal(")")
      op match {
        case "->" => FunType(first,second)
        case "*" => null //TODO product types
      }
    } else {
      val n = parseNameCharacters
      n match {
        case "int" => Int()
        case n =>
          val name = Name(n)
          TypeRef(name)
      }
    }
  }

  def parseTerm: Term = {
    parseWhitespace
    if (pos >= length)
      throw Error("no term found")
    val c = input(pos)
    val tm = if (c.isDigit) {
      // parse integer literal
      val i = parseDigits.toInt
      IntLit(i)
    } else if (c == '"') {
      // parse string literal
      ???
    } else if (c == '{') {
      // parse local declarations
      parseTerminal("{")
      val d = parseDecl
      parseTerminal(";")
      val t = parseTerm
      parseTerminal("}")
      LocalDecl(d, t)
    } else if (c == '(') {
      // parse (s op t) for op in infixOps
      parseTerminal("(")
      val first = parseTerm
      parseWhitespace
      val op = Operator.builtInInfixOperators.find(startsWith) match {
        case Some(op) => op
        case None => throw Error("unknown infix operator in term")
      }
      parseTerminal(op)
      val second = parseTerm
      parseTerminal(")")
      Operator(op, List(first, second))
    } else {
      // parse
      val n = parseNameCharacters
      n match {
        case "if" =>
          ???
        case "true" =>
          ???
        case "false" =>
          ???
        case _ =>
          TermRef(Name(n))
      }
    }
    // some special cases based on what follows after tm
    parseWhitespace
    if (startsWith(".")) {
      // parse projections out of tm
      parseTerminal(".")
      val n = parseDigits
      ???
    } else if (startsWith("@")) {
      // parse function application of tm
      parseTerminal("@")
      parseTerminal("(")
      val arg = parseTerm
      parseTerminal(")")
      Apply(tm, arg)
    } else if (startsWith(":")) {
      // parse lambda abstraction where tm must be a variable
      tm match {
        case TermRef(x) =>
          parseTerminal(":")
          val tp = parseType
          parseTerminal("=>")
          val body = parseTerm
          Lambda(x,tp,body)
        case _ => tm
      }
    } else {
      tm
    }
  }
  
  private def parseCertainCharacters(take: Char => Boolean): String = {
    parseWhitespace
    var l = 0
    while (pos+l < length && take(input(pos+l))) {
      l += 1
    }
    val n = input.substring(pos, pos+l)
    pos += l
    n
  }
  
  private def parseNameCharacters = parseCertainCharacters(c => c.isLetter)
  private def parseDigits = parseCertainCharacters(c => c.isDigit)
  
  def parseName: Name = Name(parseNameCharacters)
  
  
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