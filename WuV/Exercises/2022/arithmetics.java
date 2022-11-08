abstract class NonTerminal {
    // every class has a print method for printing out the object in the syntax using terminal symbols
    public abstract String print();
}

// one abstract class for every non-terminal
abstract class N extends NonTerminal {
    // one parsing function for every non-terminal mapping strings to objects of the corresponding type (possibly reporting parsing errors)
    // they would look like this if we implemented them
    // public static N parse(String in) {};
}

abstract class F extends NonTerminal {
    // public static F parse(String in) {};
}

// one concrete subclass for every production, using the lhs non-terminal as the superclass
// need to invent name for each production
class Sum extends N {
    // one field for every constructor argument/for non-terminal occurring on the rhs
    N left;   // need to invent names for the occurrences
    N right;
    // constructor that takes the fields and stores them in the class
    public Sum(N l, N r) {
        this.left  = l;
        this.right = r;
    }
    
    public String print() {
        return left.print() + "+" + right.print();
    };
    
    // every concrete class must override the equals method to make sure the same objects constructed twice are actually equal
    @Override public boolean equals(Object thatO) {
       if (thatO instanceof Sum) {
         Sum that = (Sum) thatO;
         return this.left.equals(that.left) && this.right.equals(this.right);
       } else
         return false;
    }
    
    // When overriding equals, we also have to override the hashCode methods with reasonable implementations, e.g.,
    // This is omitted below.
    @Override public boolean hashCode() {
       return this.left.hashCode() + this.right.hashCode();
    }
}

class Product extends N {
    // one field for every constructor argument/for non-terminal occurring on the rhs
    N left;   // need to invent names for the occurrences
    N right;
    // constructor that takes the fields and stores them in the class
    public Product(N l, N r) {
        this.left  = l;
        this.right = r;
    }
    
    public String print() {
        return left.print() + "*" + right.print();
    };
    
    // every concrete class must override the equals method to make sure the same objects constructed twice are actually equal
    @Override public boolean equals(Object thatO) {
       if (thatO instanceof Product) {
         Product that = (Product) thatO;
         return this.left.equals(that.left) && this.right.equals(this.right);
       } else
         return false;
    }
}


class Zero extends N {
    public Zero() {}
    public String print() {
        return "0";
    };
    // every concrete class must override the equals method to make sure the same objects constructed twice are actually equal
    @Override public boolean equals(Object thatO) {
       if (thatO instanceof Zero) {
         Zero that = (Zero) thatO;
         return true;
       } else
         return false;
    }
}

class One extends N {
    public One() {}
    public String print() {
        return "1";
    };
    // every concrete class must override the equals method to make sure the same objects constructed twice are actually equal
    @Override public boolean equals(Object thatO) {
       if (thatO instanceof One) {
         One that = (One) thatO;
         return true;
       } else
         return false;
    }
    
}

class Equals extends F {
    // one field for every constructor argument/for non-terminal occurring on the rhs
    N left;   // need to invent names for the occurrences
    N right;
    // constructor that takes the fields and stores them in the class
    public Equals(N l, N r) {
        this.left  = l;
        this.right = r;
    }
    
    public String print() {
        return left.print() + "=" + right.print();
    };
    
    // every concrete class must override the equals method to make sure the same objects constructed twice are actually equal
    @Override public boolean equals(Object thatO) {
       if (thatO instanceof Product) {
         Equals that = (Equals) thatO;
         return this.left.equals(that.left) && this.right.equals(this.right);
       } else
         return false;
    }
}

class LessEq extends F {
    // one field for every constructor argument/for non-terminal occurring on the rhs
    N left;   // need to invent names for the occurrences
    N right;
    // constructor that takes the fields and stores them in the class
    public LessEq(N l, N r) {
        this.left  = l;
        this.right = r;
    }
    
    public String print() {
        return left.print() + "<=" + right.print();
    };
    
    // every concrete class must override the equals method to make sure the same objects constructed twice are actually equal
    @Override public boolean equals(Object thatO) {
       if (thatO instanceof Product) {
         LessEq that = (LessEq) thatO;
         return this.left.equals(that.left) && this.right.equals(this.right);
       } else
         return false;
    }
}


public class Test {
    public static void main(String[] args) {
        // construct some example objects
        N x = new Sum(new Zero(), new One());
        N y = new Sum(new Zero(), new One());
        F z = new LessEq(x,y);
        // test that have overridden equals correctly
        System.out.println(x.equals(y));
        // run the printer methods
        System.out.println(z.print());
    }
}
