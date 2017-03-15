package language

/**
 * function checkN checks syntax trees of non-terminal N
 * 
 * every checking method succeeds without output or throws an exception with an error message
 */
object Checker {
  /** checking errors */
  case class Error(msg: String) extends java.lang.Exception(msg)

  // ************************************************************************** contexts
  
  // |- context
  def checkContext(context: Context, c: Context): Context = {
    val declsC = checkDeclList(context, c.decls, Nil)
    Context(declsC)
  }
  
  private def checkDeclList(context: Context, decls: List[Decl], sofar: List[Decl]): List[Decl] = {
    decls match {
      case Nil => sofar.reverse
      case hd::tl =>
        //println(Printer.printDecl(hd))
        val hdC = checkDecl(context, hd)
        checkDeclList(context.and(hdC), tl, hdC::sofar)
    }    
  }
  
  // ************************************************************************* declarations
  
  // context |- d
  def checkDecl(context: Context, d: Decl): Decl = {d match {
    case Val(n, aO, vOpt) =>
      aO.foreach(t => checkType(context, t))
      vOpt match {
        case None =>
          // not allowed by parser, but may be generated during checking
          Val(n, aO, vOpt)
        case Some(v) =>
          val (vC,vI) = inferOrCheckType(context, v, aO)
          Val(n, aO.orElse(Some(vI)), Some(vC))
      }
    case TypeDecl(n,aO) =>
       aO match {
         case Some(a) => checkType(context, a)
         case None => 
       }
       TypeDecl(n,aO)
    
    //******************** programs
    case Var(x,tOpt,v) =>
      tOpt.foreach(t => checkType(context, t))
      val (vC,vI) = inferOrCheckType(context, v, tOpt)
      Var(x, tOpt.orElse(Some(vI)), vC)

    case Command(t) =>
      val (tC,_) = inferOrCheckType(context, t, None)
      Command(tC)

    case RecursiveVal(n,a,v) =>
      checkType(context, a)
      val (vC,_) = inferOrCheckType(context.and(Val(n,Some(a),None)), v, Some(a))
      RecursiveVal(n,a,vC)      
      
    //******************** data types
    case IDTDecl(n, cons) =>
      val asContext = cons.map(c =>
        Val(c.name, Some(FunType(c.argType, TypeRef(n))), None)
      )
      checkContext(context.and(TypeDecl(n, None)), Context(asContext))
      d

    case ADTDecl(n, fields) =>
      val asContext = fields.map(f =>
        Val(f.name, Some(f.tp), None)
      )
      checkContext(context.and(TypeDecl(n, None)), Context(asContext))
      d
  }}

  // ************************************************************************* types
  
  // context |- tp : type
  def checkType(context: Context, tp: Type) {tp match {
    case TypeRef(n) =>
      // look up declaration of n in context 
      context.get(n) match {
        case Some(d) => d match {
          // check that n declares a type
          case TypeDecl(_,_) =>
          case IDTDecl(_,_) =>
          case ADTDecl(_,_) =>
          case _ =>
            throw Error("not a type: " + n.name)
        }
        case None =>
          throw Error("not defined: " + n.name)
      }
    case b: BaseType =>
      // nothing to do
    case FunType(f,t) =>
      checkType(context, f)
      checkType(context, t)
  }}
  
  // ************************************************************************* terms
  
  // tm is well-formed if we can infer tp such that context |- tm : tp
  def checkTerm(context: Context, tm: Term) {
    inferOrCheckType(context, tm, None)
  }
  
  // if expected == Some(tp):             check context |- tm : tp
  // if expected == None    : find tp such that context |- tm : tp
  // returns the pair (tmC,tp)
  // tmC is like tm but possible with some improvement (i.e., type inference of variables)
  def inferOrCheckType(context: Context, tm: Term, expected: Option[Type]): (Term,Type) = {
    // tmC is the improved version of tm; tmI is its inferred type
    val (tmC, tmI) = tm match {
      // names
      case TermRef(n) =>
        context.get(n) match {
          case Some(d) => d match {
            // check that n declares a term
            case Val(_, tpO, _) => tpO match {
              case Some(tp) => (tm,tp)
              case None => throw Error("name with unknown type: " + n.name) // should be impossible
            }
            // ***********************
            case Var(_, tpO, _) => tpO match {
              case Some(tp) => (tm,tp)
              case None => throw Error("name with unknown type: " + n.name) // should be impossible
            }
            case RecursiveVal(_, a, _) => (tm, a)
            case _ => throw Error("not a term: " + n.name)
          }
          case None =>
            throw Error("undeclared name: " + n.name)
        }
      
      // base types
      case UnitLit() =>
        (tm,Unit())
      case IntLit(_) =>
        (tm,Int())
      case BoolLit(_) =>
        (tm, Bool())
      case Operator(op, args) =>
        // operator applications behave differently based on the operator
        val argsTypes = args.map(a => inferOrCheckType(context, a, None))
        val (argsC,types) = argsTypes.unzip 
        if (! Operator.builtInInfixOperators.contains(op))
          throw Error("unknown operator")
        val tp = if (args.length == 2) {
          // (in)equality of terms of equal type
          if (types(0) == types(1) && (op == "==" || op == "!=")) {
            Bool()
          } else {
            (types(0),types(1)) match {
              // operators on integers
              case (Int(),Int()) => op match {
                case "+" | "-" | "*" | "mod" | "div" => Int()
                case "<=" | ">=" | ">" | "<" => Bool()
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
        } else if (args.length == 1) {
          (op, types(0)) match {
            case ("!", Bool()) => Bool()
            case _ => throw Error("ill-typed operator application")
          }
        } else {
          throw Error("wrong number of arguments for operator " + op)
        }
        (Operator(op,argsC),tp)
      case If(c,t,e) => 
        val (cC,_) = inferOrCheckType(context, c, Some(Bool()))
        val (tC,tI) = inferOrCheckType(context, t, None)
        val (eC,eI) = inferOrCheckType(context, e, None)
        leastUpperBound(context, List(tI, eI)) match {
          case None => throw Error("types of branches are incompatible")
          case Some(lub) => (If(cC,tC,eC), lub)
        }

      // local declarations
      case LocalDecl(d, t) =>
        val dC = checkDecl(context, d)
        val (tC,tI) = inferOrCheckType(context.and(dC), t, expected)
        (LocalDecl(dC,tC), tI)

      // function types
      case Lambda(x,tpO,bd) =>
        expected match {
          case Some(FunType(a,b)) =>
            val (bdC, _) = inferOrCheckType(context.and(Val(x, Some(a), None)), bd, Some(b))
            tpO match {
              case Some(tp) => if (a != tp) throw Error("function domain does not match expected type")
              case _ =>
            }
            (Lambda(x, Some(a), bdC), FunType(a, b))
          case _ => tpO match {
            case None => throw Error("cannot infer type of variable in function")
            case Some(tp) =>
              val (bdC, bdType) = inferOrCheckType(context.and(Val(x, Some(tp), None)), bd, None)
              (Lambda(x, Some(tp), bdC), FunType(tp, bdType))
          }
        }

      case Apply(fun,arg) =>
        // turn constructor applications into ConsApply
        fun match {
          case TermRef(n) =>
            context.decls.reverseIterator.exists {
              case d if d.name == n => true
              case IDTDecl(_,cons) if cons.exists(_.name == n) =>
                return inferOrCheckType(context, ConsApply(n, arg), expected)
              case _ => false
            }
          case _ =>
        }
        // now the actual code for this case
        val (funC, funType) = inferOrCheckType(context, fun, None)
        funType match {
          case FunType(from,to) =>
            val (argC, _) = inferOrCheckType(context, arg, Some(from))
            (Apply(funC,argC), to)
          case _ =>
            throw Error("non-function applied to argument")
        }
      
      //******************** programs
      case loc: Location =>
        throw Error("locations may not occur statically")
      case Assignment(x, v) => x match {
        case TermRef(n) => context.get(n) match {
          case Some(Var(_,aO,_)) =>
            val (vC,_) = inferOrCheckType(context,v,aO)
            (Assignment(x,vC),Unit())
          case Some(_) =>
            throw Error("assignment to non-variable")
          case None =>
            throw Error("unknown assignment target: " + n.name)
        }
        case _ =>
          throw Error("assignment to non-name")
      }
      
      case While(cond, body) =>
        val (condC, _) = inferOrCheckType(context, cond, Some(Bool()))
        val (bodyC, _) = inferOrCheckType(context, body, None)
        (While(condC, bodyC), Unit())
        
      case Print(tm) =>
        val (tmC,_) = inferOrCheckType(context, tm, None)
        (Print(tmC), Unit())
      case Read() => (tm, Int()) // we only read integers for simplicity
      
      // control flow
      case Break() =>
        //TODO
        (tm, Void())
      case Continue() =>
        //TODO
        (tm, Void())
      case Return(r) =>
        //TODO
        val (rC, _) = inferOrCheckType(context, r, None)
        (Return(rC), Void())
      case Throw(e) =>
        //TODO
        val (eC, _) = inferOrCheckType(context, e, None)
        (Throw(eC), Void())
      case Try(t,h) =>
        //TODO
        val (tC, tI) = inferOrCheckType(context, t, None)
        val (hC, _) = inferOrCheckType(context, h, None)
        (Try(tC, hC), Void())        

      //******************** data types
      case ConsApply(con, arg) =>
        Util.mapFind(context.decls.reverse)(d => d match {
          case IDTDecl(a, cs) => cs.find(c => c.name == con) flatMap {
            case Cons(_, argType) =>
              val (argC,_) = inferOrCheckType(context, arg, Some(argType))
              Some((ConsApply(con, argC), TypeRef(a)))
          }
          case _ => None
        }).getOrElse(throw Error("unknown constructor " + con.name))
      case Match(t, cases) =>
        val (tC,tI) = inferOrCheckType(context, t, None)
        tI match {
          case TypeRef(a) =>
            context.get(a) match {
              case Some(IDTDecl(_, cons)) =>
                 var unmatchedCons = cons
                 val (casesC,casesI) = cases.map(cas => cons.find(con => con.name == cas.name) match {
                   case Some(cons) =>
                      unmatchedCons = unmatchedCons diff List(cons)
                      val (bC,bI) = inferOrCheckType(context.and(Val(cas.patvar, Some(cons.argType), None)), cas.body, expected)
                      (ConsCase(cas.name, cas.patvar, Some(cons.argType), bC), bI)
                   case None => throw Error("unknown constructor")
                 }).unzip
                 if (unmatchedCons.nonEmpty)
                   throw Error("match non-exhaustive, missing cases for " + unmatchedCons.map(_.name.name).mkString(", "))
                 val tmC = Match(tC,casesC)
                 expected match {
                   case Some(tpE) =>
                     (tmC,tpE)
                   case None =>
                      leastUpperBound(context, casesI) match {
                        case Some(lub) => (tmC,lub)
                        case None => throw Error("cases do not agree in type")
                      }
                 }
              case Some(_) => throw Error("not an IDT " + a.name)
              case None => throw Error("unknown type " + a.name)
          }
          case _ => throw Error("not an atomic type")
        }
      case New(a, defs) =>
        context.get(a) match {
          case Some(ADTDecl(_,fields)) =>
             var undefinedFields = fields
             val defsC = defs.map(df => fields.find(field => field.name == df.name) match {
               case Some(field) =>
                  undefinedFields = undefinedFields diff List(field)
                  val (fC,_) = inferOrCheckType(context, df.definition, Some(field.tp))
                  FieldDef(field.name, Some(field.tp), fC)
               case None => throw Error("unknown field")
             })
             if (undefinedFields.nonEmpty)
               throw Error("class implementation non-exhaustive, missing definitions for " + undefinedFields.map(_.name.name).mkString(", "))
             (New(a, defsC), TypeRef(a))
          case Some(_) => throw Error("not an ADT " + a.name)
          case None => throw Error("unknown type " + a.name)
        }
      case Instance(a,dfs) =>
        println("Warning: instances should not occur statically")
        (tm, TypeRef(a))
      case FieldAccess(t, field) =>
        val (tC, tI) = inferOrCheckType(context, t, None)
        tI match {
          case TypeRef(a) =>
            context.get(a) match {
              case Some(ADTDecl(_, fs)) => fs.find(f => f.name == field) match {
                 case Some(Field(_, fieldType)) =>
                    (FieldAccess(tC,field),fieldType)
                 case None => throw Error("unknown field")
              }
              case Some(_) => throw Error("not a class type " + a.name)
              case None => throw Error("unknown type " + a.name)
          }
          case _ => throw Error("not an atomic type")
        }
    }
    
    // ************* finally check that tmI is compatible with (i.e., subype of) the expected type (if given) 
    expected.foreach(tpE =>
      if (leastUpperBound(context, List(tmI, tpE)) != Some(tpE))
        throw Error("type mismatch: expected " + Printer.printType(tpE) + "; " + "found: " + Printer.printType(tmI))
    )
    // return the improved term and its inferred type
    (tmC,tmI)
  }

  /**
   * the smallest type greater than all arguments
   * even without subtyping, this is needed to handle the type Void
   */
  private def leastUpperBound(context: Context, tps: List[Type]): Option[Type] = {
    tps.filter(tp => tp != Void()) match {
      case Nil => Some(Void())
      case hd::tl => if (tl.forall(tp => tp == hd)) Some(hd) else None
    }
  }
  
}

object Util {
  def mapFind[A,B](l: Iterable[A])(f: A => Option[B]): Option[B] =
    if (l.isEmpty) None else f(l.head) orElse mapFind(l.tail)(f)
}