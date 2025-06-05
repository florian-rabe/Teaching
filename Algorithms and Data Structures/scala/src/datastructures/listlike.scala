package datastructures

// ***************************** stacks and their implementation backed by an immutable list

abstract class Stack[A] extends Iterable[A] {
  def top: Option[A]
  // precondition: !empty
  def pop: A
  def push(a: A): Unit
  
}

class ListStack[A] extends Stack[A] {
  private var elements: InductiveList[A] = Nil[A]
  
  def top: Option[A] = if (elements.empty) None else Some(elements.head) 
  def pop: A = {
    val hd = elements.head
    elements = elements.tail
    hd
  }
  def push(a: A) {
    elements = Cons(a, elements)
  }

  def iterator = elements.iterator 
}

// ***************************** queues and their implementation backed by a doubly-linked or two singly-linked lists

abstract class Queue[A] extends Iterable[A] {
  def enqueue(a: A): Unit
  // precondition: !empty
  def dequeue: A
}

class OneListQueue[A] extends Queue[A] {
  private var elements: DLL[A] = new DLL(null,null)

  def enqueue(a: A) {
    elements.insertLast(a)
  }
  def dequeue = {
    val a = elements.first.data
    elements.delete(0)
    a
  }
  
  def iterator = elements.iterator 
}

class TwoListsQueue[A] extends Queue[A] {
  private var forward: InductiveList[A] = Nil[A]
  private var backward: InductiveList[A] = Nil[A]

  def enqueue(a: A) {
    forward = Cons(a, forward)
  }
  def dequeue = {
    if (backward.empty) {
      backward = forward.revert
      forward = Nil[A]
    }
    val a = backward.head
    backward = backward.tail
    a
  }
  
  def iterator = forward.iterator.concat(backward.revert.iterator) 
}

// ***************************** priority queues and their implementation backed by a heap

abstract class PriorityQueue[A](ord: TotalOrder[A]) extends Queue[A]

class HeapQueue[A](ord: TotalOrder[A]) extends PriorityQueue[A](ord) {
  private val heap = new TreeHeap(ord, ???)
  def enqueue(a: A) = heap.insert(a)
  def dequeue = heap.extract
  
  def iterator = heap.iterator
}


// ***************************** circular buffers and their implementation backed by an array

case class BufferOverflow() extends Exception
case class BufferUnderflow() extends Exception

// precondition: capacity > 0
abstract class Buffer[A](capacity: Int) extends Queue[A]

// ': Manifest' is a technicality of JVM and Scala necessary to create the array, ignore it 
class CircularBuffer[A: Manifest](capacity: Int) extends Buffer[A](capacity) {
  private val elements: Array[A] = new Array[A](capacity)
  // 0 <= begin < capacity
  private var begin: Int = 0
  // 0 <= size <= capacity
  private var size: Int = 0
  
  // precondition: size < capacity
  def enqueue(a: A) {
    if (size == capacity) throw BufferOverflow()
    elements((begin+size) % capacity) = a
    size = size + 1
  }
  // precondition: size > 0
  def dequeue = {
    if (size == 0) throw BufferUnderflow()
    val a = elements(begin)
    size = size - 1
    begin = (begin+1) % capacity
    a
  }
  
  def iterator = new Iterator[A] {
    private var i = 0
    def hasNext = i < size
    def getNext = {
      val a = elements((begin+i)%capacity)
      i = i + 1
      a
    }
  }
}
