package language

// an object is just a class in which all fields are static

object Printer {
   // mkString(sep) of a List[String]: concatenate with separator sep
   def printContext(c: Context): String = "\n" + c.decls.map(printDecl).mkString("\n")
      
   def printDecl(d: Decl): String = d match {
     case Val(n, t, vOpt) =>
       val vOptS = vOpt match {
         case Some(v) => " = " + printTerm(v)
         case None => ""
       }
       "val " + printName(n) + " : " + printType(t) + vOptS
   }
   
   def printName(n: Name) = n.name

   def printType(t: Type): String = t match {
     case TypeRef(n) => printName(n)
     case Int() => "int"
     case Bool() => "bool"
     case FunType(f,t) => printType(f) + " -> " + printType(t)
   }
   
   def printTerm(t: Term): String = t match {
     case TermRef(n) => n.name
     case IntLit(i) => i.toString
     case Operator(op, args) =>
       if (args.length == 2) {
         "(" + printTerm(args(0)) + " " + op + " " + printTerm(args(1)) + ")"
       } else {
         op + " " + args.map(printTerm).mkString(" ")
       }
     case LocalDecl(d,t) => "{" + printDecl(d) + "; " + printTerm(t) + "}"
     case Lambda(x,a,t) => printName(x) + ": " + printType(a) + " => " + printTerm(t)
     case Apply(f,a) => printTerm(f) + " @ " + "(" + printTerm(a) + ")" 
     case _ => "TODO"
   }
}