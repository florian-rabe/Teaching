/*
 one abstract class per non-terminal

   for every non-terminal
     one extending class per production

   for every production
     one constructor argument per non-terminal on rhs
*/

abstract class N {};

class one extends N {
    one() {};
}
class zero extends N {
    zero() {};
}
class plus extends N {
    public N left;
    public N right;
    plus(N left, N right) {
        this.left = left;
        this.right = right;
    }
}

/* in Scala, the Java boilerplate disappears and the principle is clearer
   class plus(left: N, right: N) extends N
*/

class times extends N {
    public N left;
    public N right;
    times(N left, N right) {
        this.left = left;
        this.right = right;
    }
}

abstract class F {};

class equal extends F {
    public N left;
    public N right;
    equal(N left, N right) {
        this.left = left;
        this.right = right;
    }
}
class leq extends F {
    public N left;
    public N right;
    leq(N left, N right) {
        this.left = left;
        this.right = right;
    }
}

class main {
    public static void main(String[] args) {
        N example = new times(
            new plus(new one(), new one()),
            new plus(new one(), new plus(new one(), new one()))
        );
        System.out.println(Semantics.semN(example));
    }
}

/* Semantics:
    as for SML
*/

class Semantics {
   public static String semN(N x) {
       if (x instanceof zero) {
           return "";
       } else if (x instanceof one) {
           return "|";
       } else if (x instanceof plus) {
           plus p = (plus) x;
           return semN(p.left) + semN(p.right);
       } else if (x instanceof times) {
           times p = (times) x;
           return semN(p.left).replace("|", semN(p.right));
       } else {
           // forbidden case, raise error
           return null;
       }
   }

   public static Boolean semF(F x) {
       if (x instanceof equal) {
           equal p = (equal) x;
           return semN(p.left) == semN(p.right);
       } else if (x instanceof leq) {
           leq p = (leq) x;
           return semN(p.left).startsWith(semN(p.right));
       } else {
           // forbidden case, raise error
           return false;
       }
   }
}

