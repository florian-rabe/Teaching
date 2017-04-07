package datastructures

abstract class Iterator[A] {
  def hasNext: Boolean
  def getNext: A
  
  def length: Int = if (!hasNext) 0 else {
    val a = getNext
    1+length
  }

  def get(n: Int): Option[A] = if (!hasNext) None else {
    val a = getNext
    if (n == 0) Some(a) else get(n-1)
  }
  
  def find(p: A => Boolean): Option[A] = if (!hasNext) None else {
    val a = getNext
    if (p(a)) Some(a) else find(p)
  }
  
  def contains(x: A): Boolean = if (!hasNext) false else {
    val a = getNext
    x == a || contains(x)
  }

  def indexOf(x: A): Option[Int] = if (!hasNext) None else {
    val a = getNext
    if (x == a) Some(0) else {
      indexOf(x) match {
        case None => None
        case Some(i) => Some(1+i)
      }
    }
  }
  
  def forall(p: A => Boolean): Boolean = if (!hasNext) true else {
    val a = getNext
    p(a) && forall(p)
  }

  def exists(p: A => Boolean): Boolean = if (!hasNext) false else {
    val a = getNext
    p(a) || exists(p)
  }
  
  def foreach(f: A => Unit): Unit = if (!hasNext) () else {
    val a = getNext
    f(a)
    foreach(f)
  }

  def map[B](f: A => B): Iterator[B] = {
    val i = this
    new Iterator[B] {
      def hasNext = i.hasNext
      def getNext = {
        val a = i.getNext
        f(a)
      }
    }
  }
  
  def concat(second: Iterator[A]) = {
    val first = this
    new Iterator[A] {
      def hasNext = first.hasNext || second.hasNext
      def getNext = if (first.hasNext) first.getNext else second.getNext
    }
  }
  
  def filter(p: A => Boolean) = {
    val i = this
    new Iterator[A] {
      /** stores the next element */
      private var next: Option[A] = None
      /** iterates through i until an element satisfies p, then stores that element in next */
      private def advanceToNext {
         if (next.isEmpty && i.hasNext) {
           val a = i.getNext
           if (p(a)) {
             next = Some(a)
           } else {
             advanceToNext
           }
         }
      }
      def hasNext = {
        advanceToNext
        next.isDefined
      }
      def getNext = {
        advanceToNext
        next.get
      }
    }
  }

}

abstract class Iterable[A] {
   def iterator: Iterator[A]
   
   def length = iterator.length
   def get(n: Int) = iterator.get(n)
   def find(p: A => Boolean) = iterator.find(p)
   def contains(x: A) = iterator.contains(x)
   def indexOf(x: A) = iterator.indexOf(x)
   def forall(p: A => Boolean) = iterator.forall(p)
   def exists(p: A => Boolean) = iterator.exists(p)
   def foreach(f: A => Unit) = iterator.foreach(f)
   
   def map[B](f: A => B) = iterator.map(f)
   
   def empty = ! iterator.hasNext
}