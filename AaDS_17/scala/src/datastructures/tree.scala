package datastructures

class Tree[A](val data: A, val children: InductiveList[Tree[A]]) extends Iterable[A] {
  // iterator on nodes in BFS order
  def bfs: Iterator[Tree[A]] = {
    val t = this
    new Iterator[Tree[A]] {
      private val queue = new OneListQueue[Tree[A]]
      queue.enqueue(t)
      def hasNext = !queue.empty
      def getNext = {
         val n = queue.dequeue
         n.children.foreach(x => queue.enqueue(x))
         n
      }
    }
  }
  
  // iterator on nodes in DFS order
  def dfs = {
    val t = this
    new Iterator[Tree[A]] {
      private val stack = new ListStack[Tree[A]]
      stack.push(t)
      def hasNext = !stack.empty
      def getNext = {
         val n = stack.pop
         n.children.revert.foreach(x => stack.push(x))
         n
      }
    }
  }
  
  // iterator on data in BFS order
  def iterator = bfs.map(t => t.data)
}

// ***************************** heaps and their implementation backed by a tree


abstract class Heap[A](ord: TotalOrder[A]) extends Iterable[A] {
  def find: A
  def insert(a: A): Unit
  def extract: A
}

class TreeHeap[A](ord: TotalOrder[A], root: A) extends Heap[A](ord) {
  private var elements = new Tree(root, Nil[Tree[A]])
  def find = elements.data
  def insert(a: A) = ??? //TODO
  def extract: A = ???   //TODO
  
  def iterator = elements.iterator
}