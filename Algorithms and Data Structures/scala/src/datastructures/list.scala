package datastructures

abstract class List[A] extends Iterable[A] {
  // precondition: length > 0
  def head: A
  // precondition: length > 0
  def tail: List[A]
}

abstract class MutableList[A] extends List[A] {
  // precondition: 0 <= n <= length
  def insert(n: Int, a: A): Unit
  // precondition: 0 <= n < length
  def update(n: Int, a: A): Unit
  // precondition: 0 <= n < length
  def delete(n: Int): Unit
  
  def revert: Unit
}

abstract class ImmutableList[A] extends List[A] {
  // precondition: 0 <= n <= length
  def insert(n: Int, a: A): ImmutableList[A]
  // precondition: 0 <= n < length
  def update(n: Int, a: A): ImmutableList[A]
  // precondition: 0 <= n < length
  def delete(n: Int): ImmutableList[A]

  def revert: List[A]
}

// ***************************** implementation of immutable lists as inductive data type

case class EmptyList() extends Exception

// a Scala specialty: the data type is declared as a 'sealed abstract class' 
sealed abstract class InductiveList[A] extends ImmutableList[A] {
  def head: A = this match {
    case Nil() => throw EmptyList()
    case Cons(hd, tl) => hd
  }
  def tail: InductiveList[A] = this match {
    case Nil() => throw EmptyList()
    case Cons(hd, tl) => tl
  }
  
  def revert = {
    var rev: InductiveList[A] = Nil[A]
    foreach(x => {rev = Cons(x, rev)})
    rev
  }
  
  def insert(n: Int, a: A): InductiveList[A] = if (n == 0) Cons(a, this) else Cons(head, tail.insert(n-1,a))
  def update(n: Int, a: A): InductiveList[A] = if (n == 0) Cons(a, tail) else Cons(head, tail.update(n-1,a))
  def delete(n: Int)      : InductiveList[A] = if (n == 0) tail          else Cons(head, tail.delete(n-1))
  
  def iterator = {
    val l = this
    new Iterator[A] {
      private var rest = l
      def hasNext = rest != Nil()
      def getNext = {
        val a = rest.head
        rest = rest.tail
        a
      }
    }
  }
}

// a Scala specialty: the constructors are declared as 'case classes' that extend the data type 
case class Nil[A]() extends InductiveList[A]
case class Cons[A](hd: A, tl: InductiveList[A]) extends InductiveList[A]


// ***************************** implementation of mutable lists as singly-linked list

// first == null for the empty List
class SLL[A](var first: SLLElement[A]) extends MutableList[A] {
  private def elemAt(n: Int): SLLElement[A] = if (n == 0) first else elemAt(n-1).next
  
  def head: A = if (first == null) throw EmptyList() else first.data
  def tail: SLL[A] = if (first == null) throw EmptyList() else new SLL(first.next)

  def revert {
    var previous: SLLElement[A] = null
    var current: SLLElement[A] = first
    while (current != null) {
      val h = current.next
      current.next = previous
      previous = current
      current = h
    }
    first = previous
  }
  
  def insert(n: Int, a: A) {
    if (n == 0) {
      val newElem = new SLLElement(a, first)
      first = newElem
    } else {
      val before = elemAt(n-1)
      val newElem = new SLLElement(a, before.next)
      before.next = newElem
    }
  }
  def delete(n: Int) {
    if (n == 0) {
      first = first.next
    } else {
      val before = elemAt(n-1)
      before.next = before.next.next
    }
  }
  def update(n: Int, a: A) {
    elemAt(n).data = a
  }
  
  def prepend(a: A) = {
    val newElem = new SLLElement(a, first)
    first = newElem
  }

  def append(a: A) = {
    var previous: SLLElement[A] = null
    var current = first
    // loop invariant: previous occurs immediately before current (with previous == null or current == null for the two edge cases)
    while (current != null) {
      previous = current
      current = current.next
    }
    // now previous is last element, current == null
    val newElem = new SLLElement(a, null)
    previous.next = newElem
  }

  def iterator = new Iterator[A] {
    private var nx: SLLElement[A] = first
    def hasNext = nx != null
    def getNext = {
      val e = nx.next
      nx = e.next
      e.data
    }
  }
}

// next == null for the last element
class SLLElement[A](var data: A, var next: SLLElement[A])


// ***************************** implementation of mutable lists as doubly-linked list

// first == null and last == null for the empty List
class DLL[A](var first: DLLElement[A], var last: DLLElement[A]) extends MutableList[A] {
  private def elemAt(n: Int): DLLElement[A] = if (n == 0) first else elemAt(n-1).next
  
  def head: A = if (first == null) throw EmptyList() else first.data
  def tail: DLL[A] = if (first == null) throw EmptyList() else new DLL(first.next, last)

  def revert {
    var current: DLLElement[A] = first
    while (current != null) {
      val h = current.prev
      current.prev = current.next
      current.next = h
      current = current.prev
    }
    val h = first
    first = last
    last = h
  }
  
  def insert(n: Int, a: A) {
    if (n == 0) {
      val newElem = new DLLElement(a, null, first)
      first.prev = newElem
      first = newElem
    } else {
      val before = elemAt(n-1)
      val after = before.next
      val newElem = new DLLElement(a, before, after)
      before.next = newElem
      after.prev = newElem
    }
  }
  def delete(n: Int) {
    if (n == 0) {
      first = first.next
      first.prev = null
    } else {
      val before = elemAt(n-1)
      val after = before.next.next
      before.next = after
      after.prev = before
    }
  }
  def update(n: Int, a: A) {
    elemAt(n).data = a
  }
  
  // same as insert(length, a) but O(1)
  def insertLast(a: A) = {
    val newElem = new DLLElement(a, last, null)
    last.next = newElem
    last = newElem
  }
  
  // same as delete(length-1) but O(1)
  def deleteLast {
    last.prev.next = null
    last = last.prev 
  }
  
  def iterator = new Iterator[A] {
    private var nx: DLLElement[A] = first
    def hasNext = nx != null
    def getNext = {
      val e = nx.next
      nx = e.next
      e.data
    }
  }
}

// next == null for the last element
class DLLElement[A](var data: A, var prev: DLLElement[A], var next: DLLElement[A])
