package language

/**
 * function checkN checks syntax trees of non-terminal N
 * 
 * every checking method succeeds without output or throws an exception with an error message
 */
object Checker {
  /** checking errors */
  case class Error(msg: String) extends java.lang.Exception(msg)

  // |- context
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
          checkTermAgainstType(context, v, t)
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
    case Int() | Bool() | Unit() =>
      // nothing to do
    case FunType(f,t) =>
      checkType(context, f)
      checkType(context, t)
  }}
  
  // tm is well-formed if we can infer tp such that context |- tm : tp
  def checkTerm(context: Context, tm: Term) {
    inferType(context, tm)
  }
  
  // context |- tm : tp
  def checkTermAgainstType(context: Context, tm: Term, tp: Type) {
    val tmInfer = inferType(context, tm)
    if (tmInfer != tp)
      throw Error("type mismatch: expected " + Printer.printType(tp) + "; " + "found: " + Printer.printType(tmInfer))
  }
  
  // infers the type tp such that context |- tm : tp
  def inferType(context: Context, tm: Term): Type = {
    tm match {
      // names
      case TermRef(n) =>
        context.get(n) match {
          case Some(d) => d match {
            // check that n declares a term
            case Val(_, tp, _) => tp
          }
          case None =>
            throw Error("undeclared name: " + n.name)
        }
      
      // base types
      case UnitLit() =>
        Unit()
      case IntLit(_) =>
        Int()
      case BoolLit(_) =>
        Bool()
      case Operator(op, args) =>
        // operator applications behave differently based on the operator
        val argTypes = args.map(a => inferType(context, a))
        if (! Operator.builtInInfixOperators.contains(op))
          throw Error("unknown operator")
        if (args.length == 2) {
          // (in)equality of terms of equal type
          if (argTypes(0) == argTypes(1) && (op == "==" || op == "!=")) {
            Bool()
          } else {
            (argTypes(0),argTypes(1)) match {
              // operators on integers
              case (Int(),Int()) => op match {
                case "+" | "-" | "*" | "mod" | "div" => Int()
                case "<=" | ">=" => Bool()
                case _ => throw Error("ill-typed operator application")
              }
              // operators on booleans
              case (Bool(),Bool()) => op match {
                case "&&" | "||" => Bool()
                case _ => throw Error("ill-typed operator application")
              }
              // other cases
              case _ => throw Error("ill-typed operator application")
            }
          }
        } else {
          throw Error("wrong number of arguments for operator " + op)
        }
      
      // local declarations
      case LocalDecl(d, t) =>
        inferType(context.and(d), t)

      // function types
      case Lambda(x,tp,bd) =>
        val bdType = inferType(context.and(Val(x, tp, None)), bd)
        FunType(tp, bdType)
      case Apply(fun,arg) =>
        val funType = inferType(context, fun)
        funType match {
          case FunType(from,to) =>
            checkTermAgainstType(context, arg, from)
            to
          case _ =>
            throw Error("non-function applied to argument")
        }
    }
  }
}