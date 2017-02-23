package language

/*
 * a simple application that executes a program in out language 
 */
object Main {
  /**
   * first command line argument: source file containing the program
   * remaining command line: term to interpret
   */
  def main(args: Array[String]) {
    val file = scala.io.Source.fromFile(args(0))
    val progS = file.getLines.mkString("\n")
    file.close
    
    // get the program and check it
    val prog = new Parser(progS).parseContext
    Checker.checkContext(Context(Nil), prog)
    println("program is " + Printer.printContext(prog))
    
    // get the term to interpret (the 'main' function)
    val termS = args.tail.mkString(" ")
    val term = new Parser(termS).parseTerm
    val tp = Checker.inferTypeOfTerm(prog, term)
    println("main call is " + Printer.printTerm(term) + " of type " + Printer.printType(tp))
    
    val progI = Interpreter.interpretContext(Context(Nil), prog)
    val result = Interpreter.interpretTerm(progI, term)
    println("result is " + Printer.printTerm(result))
  }
}