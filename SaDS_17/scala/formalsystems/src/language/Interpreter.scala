package language

/** the environment abstracts from the underlying programming language or machine */
abstract class Environment {
  case class Error(msg: String) extends java.lang.Exception(msg)
  
  def add(t: Term): Location
  def update(loc: Location, t: Term): scala.Unit
  def get(loc: Location): Term
  def delete(loc: Location): scala.Unit
  
  def out(t: Term ): scala.Unit
  def in(): scala.Int
}

/** a simple environment that delegates to the underlying JVM */
class JVMEnvironment extends Environment {

  class JVMLocation(var value: Term) extends Location

  // the environment does not maintain the list of locations
  // that way the garbage collection of the JVM can be used to deallocate Location objects when our variables are not in scope anymore
  def add(t: Term) = new JVMLocation(t)

  def update(loc: Location, t: Term) {
     loc match {
       case loc: JVMLocation => loc.value = t
       case _ => throw Error("unknown location") // impossible
     }
  }
  
  def get(loc: Location): Term = {
     loc match {
       case loc: JVMLocation => loc.value
       case _ => throw Error("unknown location") // impossible
     }
  }
  
  // garbage collection is inherited from the JVM
  def delete(loc: Location) {}
  
  def out(t: Term ) {
    println(Printer.printTerm(t))
  }
  
  def in(): scala.Int = {
     System.in.read()
  }
}

/**
 * interpretN interprets all terms in object of non-terminal N
 * 
 * state-sensitive or state-affecting terms are interpreted relative to an environment 
 *  
 * precondition: input must be well-formed (Checker.checkN)
 */
class Interpreter(env: Environment) {  
  
  /** run-time errors */
  case class Error(msg: String) extends java.lang.Exception(msg)
  
  // interpret all value definitions in order
  def interpretContext(context: Context, c: Context): Context = {
    val declsI = interpretDeclList(context, c.decls, Nil)
    Context(declsI)
  }

  // auxiliary functions of interpretContext
  private def interpretDeclList(context: Context, decls: List[Decl], sofar: List[Decl]): List[Decl] = decls match {
    case Nil => sofar.reverse
    case hd::tl =>
      val hdI = interpretDecl(context, hd)
      interpretDeclList(context.and(hdI), tl, hdI::sofar)
  }
  
  // interpret the definition of a value
  def interpretDecl(context: Context, decl: Decl): Decl = decl match {
    // *************************** type theory
    case Val(x,aOpt,vOpt) =>
      val aI = aOpt.map(a => interpretType(context, a))
      val vI = vOpt map(v => interpretTerm(context, v))
      Val(x,aI,vI)
    case TypeDecl(a,vO) =>
      val vI = vO.map(v => interpretType(context, v))
      TypeDecl(a, vI)
    // *************************** programs
    case Var(x, aO, v) =>
      val vI = interpretTerm(context, v)
      val loc = env.add(vI) // store initial value in environment and use the location as the definition of x 
      val aI = aO.map(a => interpretType(context, a))
      Var(x, aI, loc)
    case Command(tm) =>
      val tmI = interpretTerm(context, tm)
      Command(tmI) // can actually be thrown away
    case RecursiveVal(n,a,v) =>
      val aI = interpretType(context, a)
      val vI = interpretTerm(context.and(Val(n,Some(a),None)), v)
      RecursiveVal(n,aI,vI)
    // *************************** data types
    case IDTDecl(a, cs) => decl  // no term in constructors
    case ADTDecl(a, fs) => decl  // no term in fields
  }
  
  // interpret a type by expanding all definitions (nothing else is needed)
  def interpretType(context: Context, tp: Type): Type = tp match {
    case b: BaseType => b
    case FunType(a,b) => FunType(interpretType(context, a), interpretType(context, b))
    case TypeRef(n) => context.get(n) match {
      case Some(d) => d match {
        case TypeDecl(_, aO) => aO match {
          case Some(a) => a
          case None => tp
        }
        case IDTDecl(_,_) => tp
        case ADTDecl(_,_) => tp
        case _ => throw Error("not a type") // impossible for well-formed types
      }
      case None =>
        throw Error("unknown name: " + n.name) // impossible for well-formed types
    }
  }

  /** throw for handling control flow operators */
  case class ControlFlowMessage(command: ControlFlowCommand) extends java.lang.Throwable
  
  // interpret a term by expanding all definitions and running functions
  def interpretTerm(context: Context, tm: Term): Term = tm match {
    case TermRef(n) =>
      context.get(n) match {
        // replace n with its definition
        case Some(d) => d match {
          case Val(_,_,vOpt) => vOpt match {
            case Some(v) => v
            case None => tm
          }
          case RecursiveVal(_,_,v) =>
            v
          case Command(_) =>
            throw Error("unexpected command") // impossible because commands are anonymous
          case Var(_,_,loc) => loc match {
            case loc: Location => env.get(loc)
            case _ => throw Error("not a location") // impossible for well-formed terms
          }
          case _: ADTDecl | _: IDTDecl | _: TypeDecl =>
            throw Error("not a term") // impossible for well-formed terms
        }
        case None =>
          throw Error("unknown name: " + n.name) // impossible for well-formed terms
      }
    // nothing to do for literals
    case UnitLit() | IntLit(_) | BoolLit(_) => tm
    // apply operators: interpret all arguments left to right; if that returns only literals, apply the corresponding Scala operator
    case Operator(op, args) =>
      // TODO short-circuit evaluation
      val argsI = args.map(a => interpretTerm(context, a))
      val tmI = Operator(op, argsI)
      tmI match {
        case Operator("==", List(a,b)) => BoolLit(a == b) //TODO better equality of functions
        case Operator("!=", List(a,b)) => BoolLit(a != b)
        case Operator(op, List(IntLit(i),IntLit(j))) =>
          op match {
            case "+" => IntLit(i+j)
            case "-" => IntLit(i-j)
            case "*" => IntLit(i*j)
            case "mod" =>
              val m = i % j
              val mFixed = if (m<0) m+j else m
              IntLit(mFixed)
            case "div" =>
              val d = i / j
              val dFixed = if (i % j < 0) d-1 else d
              IntLit(dFixed)
            case "<=" => BoolLit(i <= j)
            case ">=" => BoolLit(i >= j)
            case "<" => BoolLit(i < j)
            case ">" => BoolLit(i > j)
          }
        case Operator(op, List(BoolLit(b), BoolLit(c))) =>
          op match {
            case "&&" => BoolLit(b && c)
            case "||" => BoolLit(b || c)
          }
        case Operator("!", List(BoolLit(b))) => BoolLit(!b)
        case _ => tmI //TODO
      }
    case If(c,t,e) =>
      val cI = interpretBool(context, c)
      if (cI) {
        interpretTerm(context, t)
      } else {
        interpretTerm(context, e)
      }
    case LocalDecl(d, t) =>
      val dI = interpretDecl(context, d)
      val tI = interpretTerm(context.and(dI), t)
      // defined declarations can be thrown because they are expanded and thus do not occur in tI anymore
      val isDefined = d match {
        case Val(_,_,vOpt) => vOpt.isDefined
        case TypeDecl(_,vOpt) => vOpt.isDefined
        case Var(_,_,_) => true              // always defined
        case Command(_) => true              // Command's can never be referred to anyway
        case RecursiveVal(_,_,_) => true     // always defined
        // local data types are usually not used anyway
        case IDTDecl(_,_) => false
        case ADTDecl(_,_) => false
      }
      // for local mutable variables, we can deallocate the location
      dI match {
        case Var(_,_,l: Location) => env.delete(l)
        case _ =>
      }
      // return the term with or without the local declaration depending on isDefined
      if (isDefined) {
         tI
      } else {
         LocalDecl(dI, tI)
      }

    // this part is very difficult; the behavior given here is just a very simple solution; there is lots of research on how to do it better
    // in particular, if side-effects are possible, we must not interpret the body of a function before it is called

    case Lambda(x,aO,t) =>
      val aI = aO.map(a => interpretType(context, a))
      // cannot evaluate a function recursively
      // instead, close it by expanding all references to the context
      val tI = Closer.closeTerm(context.and(Val(x,aO,None)), t) // type is always present for checked terms
      Lambda(x,aI,tI)

    case Apply(fun,arg) =>
      // if the function is a Lambda, define its argument variable and interpret the body
      val funI = interpretTerm(context, fun)
      val argI = interpretTerm(context, arg)
      funI match {
        case Lambda(x,aO,t) =>
          try {
             interpretTerm(context.and(Val(x,aO,Some(argI))), t)
          } catch {case ControlFlowMessage(Return(r)) =>
             r
          }
        case _ => Apply(funI, argI) // should not happen
      }
      
    // ********************************
    case ConsApply(c, arg) =>
      ConsApply(c, interpretTerm(context, arg))
    case Match(t, cases) =>
      val tI = interpretTerm(context, t)
      tI match {
        case ConsApply(con, arg) =>
          cases.find(c => c.name == con) match {
            case Some(cas) =>
              interpretTerm(context.and(Val(cas.patvar, cas.argType, Some(arg))), cas.body)
            case None =>
              throw Error("missing case in pattern-match") // impossible for well-formed terms
          }
        case _ => Match(tI, cases) // should not happen
      }
    case New(a, defs) =>
      val asContext = defs.map(d => Var(d.name, d.tp, d.definition))
      val defsI = interpretContext(context, Context(asContext))
      Instance(a, defsI)
    case Instance(_,_) => tm
      
    case FieldAccess(t, f) =>
      val tI = interpretTerm(context, t)
      tI match {
        case Instance(a, defs) =>
          interpretTerm(context.and(defs.decls), TermRef(f))
        case _ => FieldAccess(tI, f) // should not happen
      }

    // ********************************
    case loc: Location =>
      env.get(loc)
    case Assignment(x, v) =>
      val loc = x match {
        case TermRef(n) =>
          context.get(n) match {
            case Some(Var(_,_,l:Location)) => l
            case None => throw Error("unknown assignment target: " + n.name) // impossible for well-formed terms
            case _ => throw Error("unexpected assignment target: " + n.name)
          }
        case loc: Location => loc
        case l => throw Error("unexpected assignment target") // impossible for well-formed terms  
      }
      env.update(loc, interpretTerm(context, v))
      UnitLit()
    case While(c,b) =>
      if (interpretBool(context,c)) {
        try {
          interpretTerm(context, b)
        } catch {
          case ControlFlowMessage(Break()) =>
            return UnitLit()
          case ControlFlowMessage(Continue()) =>
        }
        interpretTerm(context, While(c,b))
      } else {
        UnitLit()
      }
    case Try(t, h) =>
      try {
        interpretTerm(context, t)
      } catch {case ControlFlowMessage(Throw(e)) =>
        interpretTerm(context, Apply(h, e))
      }
    case Break() => throw ControlFlowMessage(Break())
    case Continue() => throw ControlFlowMessage(Continue())
    case Throw(e) =>
      val eI = interpretTerm(context, e)
      throw ControlFlowMessage(Throw(eI))
    case Return(t) =>
      val tI = interpretTerm(context, t)
      throw ControlFlowMessage(Return(tI))      
    case Print(t) =>
      val tI = interpretTerm(context, t)
      env.out(tI)
      UnitLit()
    case Read() =>
      IntLit(env.in())
  }
  
  /** special case of interpretTerm that produces a boolean literal, helpful for While, If etc. */
  private def interpretBool(context: Context, tm: Term): Boolean = {
    interpretTerm(context, tm) match {
      case BoolLit(b) => b
      case _ => throw Error("not a boolean literal")
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
    case TypeDecl(_,_) => decl

    case Command(tm) => 
      val tmC = closeTerm(context, tm)
      Command(tmC)
    case Var(x,a,v) =>
      val vC = closeTerm(context, v)
      Var(x,a,vC)
    case RecursiveVal(n,a,v) =>
      val vC = closeTerm(context, v)
      RecursiveVal(n,a,vC)

    case ADTDecl(a, fs) => decl  // no terms may occur in fields at this point
    case IDTDecl(a, cs) => decl  // no terms may occur in constructors at this point
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
    case If(c,t,e) => 
      val cC = closeTerm(context, c)
      val tC = closeTerm(context, t)
      val eC = closeTerm(context, e)
      If(cC,tC,eC)
      
    case LocalDecl(d, t) =>
      val dC = closeDecl(context, d)
      val tC = closeTerm(context.and(dC), t)
      LocalDecl(dC, tC)

    case Lambda(x,aO,t) =>
      val tC = closeTerm(context.and(Val(x,aO,None)), t)
      Lambda(x,aO,tC)
    case Apply(fun,arg) =>
      // if the function is a Lambda, define its argument variable and close the body
      val funC = closeTerm(context, fun)
      val argC = closeTerm(context, arg)
      Apply(funC, argC)

    case loc: Location =>
      loc
    case Assignment(x,v) =>
      val xC = closeTerm(context, x)
      val vC = closeTerm(context, v)
      Assignment(xC,vC)
    case While(c,b) =>
      val cC = closeTerm(context, c)
      val bC = closeTerm(context, b)
      While(cC,bC)
    case Print(t) =>
      val tC = closeTerm(context, t)
      Print(tC)
    case Read() =>
      Read()
    
    case Break() => Break()
    case Continue() => Continue()
    case Return(r) => Return(closeTerm(context, r))
    case Throw(e) => Return(closeTerm(context, e))
    case Try(t,h) => Try(closeTerm(context, t), closeTerm(context, h))

    case ConsApply(c, arg) => ConsApply(c, closeTerm(context, arg))
    case Match(t, cases) =>
      val tC = closeTerm(context, t)
      val casesC = cases.map {c =>
        val bodyC = closeTerm(context.and(Val(c.patvar, c.argType, None)), c.body)
        ConsCase(c.name, c.patvar, c.argType, bodyC)
      }
      Match(tC, casesC)
    case New(a, defs) =>
      val defsC = defs.map {d =>
        FieldDef(d.name, d.tp, closeTerm(context, d.definition))
      }
      New(a, defsC)
    case Instance(a, defs) => tm
    
    case FieldAccess(t, f) => FieldAccess(closeTerm(context, t), f)
  }
}