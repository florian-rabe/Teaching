package language

/*
 * a simple application that bundles and tests all components 
 */
object Main {
  /**
   * main method (command line arguments are passed as 'args')
   * 
   * first argument: source file containing the program
   * remaining arguments: the term to interpret relative to that program
   */
  def main(args: Array[String]) {
    // read the program
    val file = scala.io.Source.fromFile(args(0))
    val progS = file.getLines.mkString("\n")
    file.close
    
    // parse the program and check it and print it
    val prog = new Parser(progS).parseContext
    val progC = Checker.checkContext(Context(Nil), prog)
    println("context:\n" + Printer.printContext(progC) + "\n")
    
    // get the term to interpret (the 'main' function), check it, infer its type, print both
    val termS = args.tail.mkString(" ")
    val term = new Parser(termS).parseTerm
    val (termC,termTp) = Checker.inferOrCheckType(progC, term, None)
    println("main call: '" + Printer.printTerm(termC) + "' of type '" + Printer.printType(termTp) + "'\n")

    println("************************")
    // interpret term against prog
    val interpreter = new Interpreter(new JVMEnvironment)
    // first run all code in the declarations
    val progI = interpreter.interpretContext(Context(Nil), progC)
    // then run the main call and print the result
    val termI = interpreter.interpretTerm(progI, termC)
    println("************************")
    
    println("\ncontext after interpretation: \n" + Printer.printContext(progI) + "\n")
    println("result of main call: " + Printer.printTerm(termI))
  }
}