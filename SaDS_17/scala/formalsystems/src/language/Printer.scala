package language

/* function printN prints syntax trees for non-terminal N */
object Printer {
   // mkString(sep) of a List[String]: concatenate with separator sep
   def printContext(c: Context): String = "\n" + c.decls.map(printDecl).mkString("\n")
      
   def printDecl(d: Decl): String = d match {
     case Val(n, a, vOpt) =>
       val vOptS = vOpt match {
         case Some(v) => " = " + printTerm(v)
         case None => ""
       }
       "val " + printName(n) + " : " + printType(a) + vOptS
     // ***************************
     case Command(t) => printTerm(t)
     case Var(n,a,v) =>
       "var " + printName(n) + " : " + printType(a) + " = " + printTerm(v)
   }
   
   def printName(n: Name) = n.name

   def printType(t: Type): String = t match {
     case TypeRef(n) => printName(n)
     case Unit() => "unit"
     case Int() => "int"
     case Bool() => "bool"
     case FunType(f,t) => "(" + printType(f) + " -> " + printType(t) + ")"
     // ***************************
     case LocationType(a) => printType(a) + "*" 
   }
   
   def printTerm(t: Term): String = t match {
     case TermRef(n) => n.name
     case UnitLit() => "()"
     case IntLit(v) => v.toString
     case BoolLit(v) => v.toString
     case Operator(op, args) =>
       val argsS = args.map(printTerm)
       if (args.length == 1) {
         op + " " + argsS(0)
       }
       if (args.length == 2) {
         "(" + argsS(0) + " " + op + " " + argsS(1) + ")"
       } else {
         "(" + op + " " + argsS.mkString(" ") + ")"
       }
     case LocalDecl(d,t) => "{" + printDecl(d) + "; " + printTerm(t) + "}"
     case Lambda(x,a,t) => printName(x) + ": " + printType(a) + " => " + printTerm(t)
     case Apply(f,a) => printTerm(f) + "(" + printTerm(a) + ")" 

     // ***************************
     case loc: Location => "*" + printName(loc.name) + "[" + printTerm(loc.value) + "]"
     case Assignment(x, v) => printTerm(x) + " = " + printTerm(v)
     case While(c,b) => "while (" + printTerm(c) + ")" + printTerm(b)
     case Print(t) => "print(" + printTerm(t) + ")"
   }
}