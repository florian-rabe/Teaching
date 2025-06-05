package shellshock

object Interpreter {
  
  def interpret(context: List[DefComm], command: Comm): List[DefComm] = command match {
    case FunDef(n,x,b) => List(FunDef(n,x,b))
    case ValDef(n,v) => List(ValDef(n,v))
    case Run(sc) => 
      ??? //TODO run the shell command
    case Apply(f,a) =>
      val aI = evaluate(context, a)
      ??? // TODO look up FunDef(f,x,body) in context, evaluate body with context:::List(ValDef(x,aI)) 
    case Sequence(c,d) =>
      val cI = interpret(context, c)
      val dI = interpret(context ::: cI, d)
      cI ::: dI
  }
  
  def evaluate(context: List[DefComm], expr: Expr): String = expr match {
    case NameRef(n) => ??? //TODO look up ValDef(n, v) in context, return v
    case StringExpr(s) => s
  }

}