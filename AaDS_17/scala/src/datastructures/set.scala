package datastructures

abstract class Set[A] extends Iterable[A] {
   def size = iterator.length
}

abstract class MutableSet[A] extends Set[A] {
  def insert(x: A): Unit
  def delete(x: A): Unit
}

abstract class ImmutableSet[A] extends Set[A] {
  def insert(x: A): ImmutableSet[A]
  def delete(x: A): ImmutableSet[A]
}

// ***************************** implementation of mutable sets as lists

class ListSet[A] extends MutableSet[A] {
  private var elements = new SLL[A](null)
  
  /** prepend if not already present, Theta(n) */
  def insert(x: A) {
    if (contains(x))
      elements.prepend(x)
  }
  
  def delete(x: A) {
    elements.indexOf(x) match {
      case None => // nothing to do
      case Some(i) => elements.delete(i)
    }
  }
  
  def iterator = elements.iterator
}

abstract class HashSet[A](numBuckets: Int) extends Set[A] {
  /** postcondition: 0 < hash(x) < numBuckets */
  def hash(x: A): Int
  
  private var buckets = new Array[ListSet[A]](numBuckets)
  
  def insert(x: A) = buckets(hash(x)).insert(x)
  def delete(x: A) = buckets(hash(x)).delete(x)

  /** the default definition searches in the entire iterator; need to override it to be efficient */
  override def contains(x: A) = buckets(hash(x)).contains(x)
  
  def iterator = {
    // concatenate the iterators of all buckets
    var iter = buckets(0).iterator
    Range(1,numBuckets).foreach(i => iter = iter.concat(buckets(i).iterator))
    iter
  }
}

// ***************************** implementation of sets via characteristic functions, not iterable because they may infinite

class CharacteristicFunctionSet[A](charFun: A => Boolean) {
   
   def contains(x: A) = charFun(x)

   def insert(a:A) = new CharacteristicFunctionSet[A](x => x == a || this.contains(x))
   def delete(a:A) = new CharacteristicFunctionSet[A](x => x != a && this.contains(x))
   
   def union(that: CharacteristicFunctionSet[A]) = new CharacteristicFunctionSet[A](x => this.contains(x) || that.contains(x))
   def intersect(that: CharacteristicFunctionSet[A]) = new CharacteristicFunctionSet[A](x => this.contains(x) && that.contains(x))
}