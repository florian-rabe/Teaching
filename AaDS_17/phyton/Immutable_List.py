from List import ImmutableList, List, List_Iterator #imports the parent classes
End = Exception("End of the List!")
Empty = Exception("Nil list is always Empty!")

class Imm_L_I(List_Iterator): #creates an iterator for the list
    def Next(self):
        #complexity O(1)
        if self.hasNext():
            data = self.iter.getHead()
            self.iter = self.iter.getTail()
            return data
        raise End

    def hasNext(self):
        #complexity O(1)
        if self.iter.getTail():
            return True
        return False

class Nil(ImmutableList): #Nil is the Empty list(think as a base case)
    def emptyList(self):
        #complexity O(1)
        return True
    def getEl(self,n):
        #complexity O(1)
        return None         #returns None since Nil is empty
    def getTail(self):
        #complexity O(1)
        return None
##    def setTail(self,tail):
##        return Empty
##    def setHead(self,n):
##        raise Empty
    def reverse(self, tail):
        #complexity O(1)
        return tail
    def getHead(self):
        #complexity O(1)
        return None
    def print(self):
        #complexity O(1)
        print()
    def deletFirst(self):
        raise Empty
##    def concate(self, l):
##        return Empty
    def iterator(self):
        #complexity O(1)
        iter = Imm_L_I()
        iter.copy(self) 
        return iter #returns an empty iterator
    

class Cons(ImmutableList): #Cons is the constructor of the list
    def __init__(self,data, tail):
        #complexity O(1)
        self.__head = data #stores the data
        self.__tail = tail #points to the next node
        self.__iter = Imm_L_I()
        self.__iter.copy(self)

    def emptyList(self):
        #complexity O(1)
        return False
    def getEl(self,n):
        #complexity O(n)
        if n < 0:
            raise Exception("Index cannot be less than Zero!")
        elif self.__tail is None:
            raise Exception("Out of bound!")
        else:
            if n == 0:
                return self.__head
            else:
                return self.__tail.getEl(n-1)
    def getTail(self):
        #complexity O(1)
        return self.__tail

    def reverse(self, tail):  #tail is the list we are storing the reverse of the list
        #complexity O(n)
        tail = Cons(self.getHead(), tail)
        return self.getTail().reverse(tail) #recursion stops when self.__tail reaches
                                            #to Nil()
##    def setTail(self, t):
##        #complexity O(1)
##        self.__tail = t
##    def setHead(self, n):
##        #complexity O(1)
##        self.__head = n
##    def concate(self, l):
##        #complexity O(n)
##        tmp = self.__tail.reverse(Nil())
##        tmp = tmp.reverse(l)
##        tmp = Cons(self.__head, tmp)
##        return tmp
        
    
    def getHead(self):
        #complexity O(1)
        return self.__head

    def print(self):
        #complexity O(n)
        if self.getTail():
            print(self.getHead(), end = " ")
            self.getTail().print() # recursion stops when self.__tail reaches
                                   # to Nil()

    def deletFirst(self):
        #complexity O(1)
        if self.__head == None:
            raise Empty
        else:
            self.__head = self.__tail.getHead()
            self.__tail = self.__tail.getTail()
    
    def iterator(self):
        #complexity O(1)
        return self.__iter #returns iterator object for the list




##c = Cons(2,Cons(3,Cons(4,Cons(0, Nil()))))
##d = Cons(7,Cons(5,Cons(10,Cons(0, Nil()))))
##n = Nil()
##iter_n = n.iterator()
##iter_c = c.iterator()
##print("printing Nil()")
##while iter_n.hasNext():
##    print(iter_n.Next(), end = " ")
##print()
##print("printing c usinf iterator")
##while iter_c.hasNext():
##    print(iter_c.Next(), end = " ")
##print()
##print("printing c")
##c.print()
##print("printing the reverse of the c")
##c.reverse(Nil()).print()
##print("printing the second el of the c")
##el1 = c.getEl(2)
##print(el1)
##
##print("printing first el deleted c")
##c.deletFirst()
##iter_c.init()
##while iter_c.hasNext():
##    print(iter_c.Next(), end = " ")
##print()
####print("printing d")
####d.print()
####print("combining c with d")
####c = c.concate(d)
####c.print()
##print("printing c with iterator")
##iter_c.init()
##while iter_c.hasNext():
##    print(iter_c.Next(), end = " ")
##print()
##print("deleting first elements of the c")
##for i in range(2):
##    c.deletFirst()
####    d.deletFirst()
##iter_c.init()
##while iter_c.hasNext():
##    print(iter_c.Next(), end = " ")
##print()
##iter_c.init()
##print("adding new element to c")
##c = Cons(12,c)
##iter_c = c.iterator()
##while iter_c.hasNext():
##    print(iter_c.Next(), end = " ")
##print()
