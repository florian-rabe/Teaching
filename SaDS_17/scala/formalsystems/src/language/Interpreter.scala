package language

/** interpretN interprets all terms in object of non-terminal N
 *  
 * precondition: input must be well-formed (Checker.checkN)
 */
object Interpreter {  
  
  /** run-time errors */
  case class Error(msg: String) extends java.lang.Exception(msg)
  
  // interpret all value definitions in order
  def interpretContext(context: Context, c: Context): Context = {
    val declsI = interpretDeclList(context, c.decls)
    Context(declsI)
  }

  // auxiliary functions of interpretContext
  private def interpretDeclList(context: Context, decls: List[Decl]): List[Decl] = decls match {
    case Nil => Nil
    case hd::tl =>
      val hdI = interpretDecl(context, hd)
      val tlI = interpretDeclList(context.and(hdI), tl)
      hdI :: tlI
  }
  
  // interpret the definition of a value
  def interpretDecl(context: Context, decl: Decl): Decl = decl match {
    case Val(x,a,vOpt) => vOpt match {
      case Some(v) =>
        val vI = interpretTerm(context, v)
        Val(x,a,Some(vI))
      case None =>
        Val(x,a,None)
    }
  }
    
  // interpret a term by expanding all definitions and running functions
  def interpretTerm(context: Context, tm: Term): Term = tm match {
    case TermRef(n) =>
      context.get(n) match {
        // replace n with its definition
        case Some(d) => d match {
          case Val(_,_,vOpt) => vOpt match {
            case Some(v) => interpretTerm(context, v)
            case None => tm
          }
        }
        case None =>
          throw Error("unknown name: " + n) // impossible for well-formed terms
      }
    // nothing to do for literals
    case UnitLit() | IntLit(_) | BoolLit(_) => tm
    // apply operators: interpret all arguments left to right; if that returns only literals, apply the corresponding Scala operator
    case Operator(op, args) =>
      val argsI = args.map(a => interpretTerm(context, a))
      val tmI = Operator(op, argsI)
      tmI match {
        case Operator("+", List(IntLit(i),IntLit(j))) => IntLit(i+j)
        case Operator("==", List(a,b)) => BoolLit(a == b)
        case Operator("!=", List(a,b)) => BoolLit(a != b)
        case _ => tmI //TODO
      }
      
    case LocalDecl(d, t) =>
      val dI = interpretDecl(context, d)
      val tI = interpretTerm(context.and(dI), t)
      // true if we can contract the local declaration because it is not referred to anymore
      val canContract = d match {
        case Val(_,_,vOpt) => vOpt.isDefined // defined Val's have been expanded in tI
      }
      if (canContract) {
         tI
      } else {
         LocalDecl(dI, tI)
      }

    // this part is very difficult; the behavior given here is just a very simple solution; there is lots of research on how to do it better
    // in particular, if side-effects are possible, we must not interpret the body of a function before it is called

    case Lambda(x,a,t) =>
      // cannot evaluate a function recursively
      // instead, close it by expanding all references to the context
      val tI = Closer.closeTerm(context.and(Val(x,a,None)), t)
      Lambda(x,a,tI)

    case Apply(fun,arg) =>
      // if the function is a Lambda, define its argument variable and interpret the body
      val funI = interpretTerm(context, fun)
      val argI = interpretTerm(context, arg)
      funI match {
        case Lambda(x,a,t) =>
          interpretTerm(context.and(Val(x,a,Some(argI))), t)
        case _ => Apply(funI, argI) // should not happen
      }
  }
}

// *********************************************************************

/** recursively replaces every TermRef with its definition
 *
 * precondition: input is well-formed
 * postcondition: output does not refers to context for defined Val declarations
 */
object Closer {
  def closeDecl(context: Context, decl: Decl): Decl = decl match {
    case Val(x,a,vOpt) =>
      val vC = vOpt match {
        case Some(v) => Some(closeTerm(context, v))
        case None => None
      }
      Val(x, a, vC)
  }
  
  // replace all references to the context with their definition
  def closeTerm(context: Context, tm: Term): Term = tm match {
    case TermRef(n) =>
      context.get(n) match {
        case Some(d) => d match {
          case Val(_,_,Some(v)) => closeTerm(context, v) // recursion needed in case v is a function
          case _ => tm
        }
        case None => tm // impossible for well-formed terms
      }
    // nothing to do for literals
    case UnitLit() | IntLit(_) | BoolLit(_) => tm
    // apply operators: close all arguments left to right; if that returns only literals, apply the corresponding Scala operator
    case Operator(op, args) =>
      val argsC = args.map(a => closeTerm(context, a))
      Operator(op, argsC)
      
    case LocalDecl(d, t) =>
      val dC = closeDecl(context, d)
      val tC = closeTerm(context.and(dC), t)
      LocalDecl(dC, tC)

    case Lambda(x,a,t) =>
      val tC = closeTerm(context.and(Val(x,a,None)), t)
      Lambda(x,a,tC)
    case Apply(fun,arg) =>
      // if the function is a Lambda, define its argument variable and close the body
      val funC = closeTerm(context, fun)
      val argC = closeTerm(context, arg)
      Apply(funC, argC)
  }
}