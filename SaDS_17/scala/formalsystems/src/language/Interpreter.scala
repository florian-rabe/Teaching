package language

object Interpreter {  
  
  def interpretContext(context: Context, c: Context): Context = {
    val declsI = interpretDeclList(context, c.decls)
    Context(declsI)
  }
    
  private def interpretDeclList(context: Context, decls: List[Decl]): List[Decl] = decls match {
    case Nil => Nil
    case hd::tl =>
      val hdI = interpretDecl(context, hd)
      val tlI = interpretDeclList(context.and(hdI), tl)
      hdI :: tlI
  }
  
  def interpretDecl(context: Context, decl: Decl) = decl match {
    case Val(x,a,Some(v)) =>
      val vI = interpretTerm(context, v)
      Val(x,a,Some(vI))
    case _ => decl
  }
  
  def interpretTerm(context: Context, tm: Term): Term = tm match {
    case TermRef(n) =>
      context.get(n) match {
        // replace n with its value
        case Some(d) => d match {
          case Val(_,_,vOpt) => vOpt match {
            case Some(v) => interpretTerm(context, v)
            case None => tm
          }
        }
        case None => tm // impossible for well-formed terms
      }
    case IntLit(_) => tm
    case Operator(op, args) =>
      val argsI = args.map(a => interpretTerm(context, a))
      op match {
        case "+" => (argsI(0),argsI(1)) match {
          case (IntLit(i),IntLit(j)) => IntLit(i+j)
          case _ => Operator(op, argsI) // impossible
        }
        case _ => ??? // TODO other operators
      }
    case LocalDecl(d, t) =>
      // evaluate d and then t in the extended context
      val dI = interpretDecl(context, d)
      interpretTerm(context.and(dI), t)
    case Lambda(x,a,t) =>
      val tI = interpretTerm(context.and(Val(x,a,None)), t)
      Lambda(x,a,tI)
    case Apply(fun,arg) =>
      val funI = interpretTerm(context, fun)
      val argI = interpretTerm(context, arg)
      funI match {
        case Lambda(x,a,t) =>
          interpretTerm(context.and(Val(x,a,Some(argI))), t)
        case _ => Apply(funI, argI) // impossible for well-formed terms
      }
  }
}