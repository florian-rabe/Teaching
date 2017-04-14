package datastructures

/**
 * @tparam B the data attached to edges
 * 
 * all nodes are identified by integers i such that 0 <= i < numNodes
 */
class EdgeLabeledGraph[B]() {
   /** the number of nodes, initially the graph is empty */
   private var numNodes = 0
   
   /* outgoing.get(i) is the adjacency list of node i, containing a pair (b, j) for every edge i --- b ---> j */ 
   private var outgoing = new SLL[SLL[(B,Int)]](null)
   /* incoming.get(j) is the backwards adjacency list of node j, containing a pair (i, b) for every edge i --- b ---> j */ 
   private var incoming = new SLL[SLL[(Int,B)]](null)
   
   def addNode() {
      numNodes = numNodes + 1
      // add empty adjacency lists for the new node
      outgoing.append(new SLL(null))
      incoming.append(new SLL(null))
   }
   
   /** checks if a node is in this graph */
   def validNode(i: Int) = 0 <= i && i < numNodes

   /** precondition: validNode(from) && validNode(to) */
   def addEdge(from: Int, b: B, to: Int) {
      // TODO
   }
   
   /**
    * precondition: validNode(from) && validNode(to)
    * postcondition: returns b for the edge from --- b ---> to, if such an edge exists
    */
   def getEdge(from: Int, to: Int): Option[B] = {
     None // TODO
   }
   
   /** precondition: validNode(from) */
   def getOutgoing(from: Int): datastructures.Iterator[(B,Int)] = {
     outgoing.get(from) match {
       case Some(l) => l.iterator
       case None => null // impossible by precondition
     }
   }
   
   /** precondition: validNode(to) */
   def getIncoming(to: Int): datastructures.Iterator[(Int,B)] = {
     incoming.get(to) match {
       case Some(l) => l.iterator
       case None => null // impossible by precondition
     }
   }
}

/** graphs whose edges are labeled with integers */
class WeightedGraph extends EdgeLabeledGraph[Int]