package language

object Checker {
  case class Error(msg: String) extends java.lang.Exception(msg)
  
  // every checking method succeeds or throws an exception with an error message
  
  def checkContext(context: Context, c: Context) {
    c.decls match {
      case Nil =>
      case hd::tl =>
        checkDecl(context, hd)
        checkContext(context.and(hd), Context(tl))
    }
  }
  
  // context |- d
  def checkDecl(context: Context, d: Decl) {d match {
    case Val(n, t, vOpt) =>
      checkType(context, t)
      vOpt match {
        case None =>
          // not allowed in user input, may only come up locally
          throw Error("uninitialized variable")
        case Some(v) =>
          checkTerm(context, v, t)
      }
  }}
  
  // context |- tp : type
  def checkType(context: Context, tp: Type){tp match {
    case TypeRef(n) =>
      // look up declaration of n in context 
      context.get(n) match {
        case Some(d) => d match {
          // check that n declares a type
          case _ =>
            throw Error("not a type: " + n.name)
        }
        case None =>
          throw Error("not defined: " + n.name)
      }
    case Int() | Bool() =>
      // nothing to do
    case FunType(f,t) =>
      checkType(context, f)
      checkType(context, t)
  }}
  
  // context |- tm : tp
  def checkTerm(context: Context, tm: Term, tp: Type) {
    val tmInfer = inferTypeOfTerm(context, tm)
    if (tmInfer != tp)
      throw Error("type mismatch: expected " + Printer.printType(tp) + "; " + "found: " + Printer.printType(tmInfer))
  }
  
  // check well-formedness of of tm by finding tp such that context |- tm : tp
  def inferTypeOfTerm(context: Context, tm: Term): Type = {
    tm match {
      case TermRef(n) =>
        context.get(n) match {
          case Some(d) => d match {
            // check that n declares a term
            case Val(_, tp, _) => tp
            case _ => throw Error("not a term: " + n.name)
          }
          case None =>
            throw Error("undeclared name: " + n.name)
        }
      case IntLit(_) =>
        Int()
      case LocalDecl(d, t) =>
        inferTypeOfTerm(context.and(d), t)
      case Lambda(x,tp,bd) =>
        val bdType = inferTypeOfTerm(context.and(Val(x, tp, None)), bd)
        FunType(tp, bdType)
      case Apply(fun,arg) =>
        val funType = inferTypeOfTerm(context, fun)
        funType match {
          case FunType(from,to) =>
            checkTerm(context, arg, from)
            to
          case _ =>
            throw Error("non-function applied to argument")
        }
      case Operator(op, args) =>
        val argTypes = args.map(a => inferTypeOfTerm(context, a))
        if (Operator.builtInInfixOperators.contains(op) && args.length == 2) {
          if (argTypes(0) == argTypes(1) && (op == "==" || op == "!=")) {
            Bool()
          } else {
            (argTypes(0),argTypes(1)) match {
              case (Int(),Int()) => op match {
                case "+" | "-" | "*" | "mod" | "div" => Int()
                case "<=" | ">=" => ??? // Bool
                case _ => throw Error("ill-typed operator application")
              }
              case (Bool(),Bool()) => op match {
                case "&&" | "||" => Bool()
                case _ => throw Error("ill-typed operator application")
              }
              case _ => throw Error("ill-typed operator application")
            }
          }
        } else {
          throw Error("unknown operator or wrong number of arguments")
        }
    }
  }
}