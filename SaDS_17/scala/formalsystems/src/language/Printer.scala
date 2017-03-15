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
       val aS = a match {
         case None => ""
         case Some(tp) => " : " + printType(tp)
       }
       "val " + printName(n) + aS + vOptS
     case TypeDecl(n,aO) =>
       val aS = aO match {
         case None => ""
         case Some(a) => " = " + printType(a)
       }
       "type " + n.name + aS

     // *************************** programs
     case Var(n,aO,v) =>
       val aS = aO match {
         case None => ""
         case Some(tp) => " : " + printType(tp)
       }
       "var " + printName(n) + aS + " = " + printTerm(v)
     case Command(t) => printTerm(t)

     case RecursiveVal(n,a,v) => printName(n) + " : " + printType(a) + " = " + printTerm(v)
     
     // *************************** data types
     case IDTDecl(a, cs) =>
       val csS = cs.map(c => printName(c.name) + "(" + printType(c.argType) + ")")
       "data " + a.name + " { " + csS.mkString(" | ") + " }"
     case ADTDecl(a, fs) =>
       val fsS = fs.map(f => printName(f.name) + " : " +  printType(f.tp))
       "class " + a.name + " { " + fsS.mkString(", ") + " }"
   }
   
   def printName(n: Name) = n.name

   def printType(t: Type): String = t match {
     case TypeRef(n) => printName(n)
     case Unit() => "unit"
     case Void() => "void"
     case Int() => "int"
     case Bool() => "bool"
     case FunType(f,t) => "(" + printType(f) + " -> " + printType(t) + ")"
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
     case If(c,t,e) => "if (" + printTerm(c) + ") " + printTerm(t) + " else " + printTerm(e) 

     case LocalDecl(d,t) => "{" + printDecl(d) + "; " + printTerm(t) + "}"
 
     case Lambda(x,a,t) =>
       val aS = a match {
         case Some(tp) => ": " + printType(tp)
         case None => ""
       }
       printName(x) + aS + " => " + printTerm(t)
     case Apply(f,a) => printTerm(f) + "(" + printTerm(a) + ")" 

     // *************************** programs
     case loc: Location => "<location:" + loc.hashCode + ">"
     case Assignment(x, v) => printTerm(x) + " = " + printTerm(v)
     case While(c,b) => "while (" + printTerm(c) + ")" + printTerm(b)
     case Print(t) => "print(" + printTerm(t) + ")"
     case Read() => "read"

     // control flow
     case Break() => "break"
     case Continue() => "continue"
     case Return(r) => "return " + printTerm(r)
     case Throw(r) => "throw " + printTerm(r)
     case Try(t,h) => "try " + printTerm(t) + " catch " + printTerm(h)

     // *************************** data types
     case ConsApply(c,a) => printName(c) + "(" + printTerm(a) + ")"
     case Match(t,cases) =>
       val casesS = cases.map {c =>
         val tpS = c.argType.map(tp => " : " + printType(tp)).getOrElse("") 
         printName(c.name) + "(" + c.patvar.name + tpS + ")" + " => " + printTerm(c.body)
       }
       "match " + printTerm(t) + "{" + casesS  .mkString(" | ") + "}"
     case New(a, defs) =>
       val defsS = defs.map {df =>
         val tS = df.tp.map(tp => " : " + printType(tp)).getOrElse("")
         "val " + printName(df.name) + tS + " = " + printTerm(df.definition)
       }
       "new " + a.name + "{" + defsS.mkString(" , ") + "}"
     case Instance(a,defs) => "<instance of type " + printName(a) + "@" + t.hashCode + ">"
     case FieldAccess(t, f) => printTerm(t) + "." + printName(f)
   }
}