from Iterator import Iterator, Iterable
from Option import Unit, Option
from Immutable_List import Imm_L_I, Nil, Cons
from abc import ABCMeta, abstractmethod




class Set(Iterable): #abstract class Set
    __metaclass__ = ABCMeta


    @abstractmethod
    def insert(self, n): #adds the n if n is not in the set
        pass

    @abstractmethod
    def delete(self, n): #delets the n if it is in the set
        pass

    @abstractmethod
    def constains(self, n): #checks is n in the set or not
        pass

            

class BitVector_Iterator(Iterator):

    def __init__(self):
        #complexity O(1)
        self.iter = None
        self.index = 0

    def init(self):
        #complexity O(1)
        self.index = 0

    def copy(self,item):
        #complexity O(1)
        self.iter = item

    def Next(self):
        #complexity O(1)
        if self.hasNext():
            self.index += 1
            if self.iter.contains(self.index-1):
                return self.index-1
            return "_"

    def hasNext(self):
        #complexity O(1)
        return self.index < len(self.iter.getVector())
            
        
class BitVector(Set):
    def __init__(self, n):
        #complexity O(n) creation of vector takes linear time
        self.__vector = [False]*n
        self.__iter = BitVector_Iterator()
        self.__iter.copy(self)
    def getVector(self):
        #complexity O(1)
        return self.__vector

    def insert(self, el):
        #complexity O(1)
        x = Unit()
        if not self.contains(el):
            self.__vector[el] = True
            return x
        print("Element already exist!" , el)
        return x

    def contains(self, el):
        #complexity O(1)
        return self.__vector[el]

    def delete(self,el):
        #complexity O(1)
        x = Option()
        if self.contains(el):
            x.setEl(el)
            self.__vector[el] = False
            return x
        print("Element is not in the set", el)
        return x

    def iterator(self):
        #complexity O(1)
        return self.__iter

class ListSet(Set): #IMM List
    def __init__(self):
        #complexity O(1)
        self.__elements = Nil() 
        self.__iter = Imm_L_I() #iterator object for the listset

    def insert(self, n):
        #complexity O(n)
        x = Unit()
        if not self.contains(n):
            self.__elements = Cons(n, self.__elements) 
            self.__iter.copy(self.__elements) # copies the address of the object     
            return x
        print("Element already exist!" , n)
        return x

    def contains(self, n):
        #complexity O(n)
        self.__iter.init()
        if self.__elements.emptyList(): #checks is the set empty or not
            return False
        while self.__iter.hasNext():
            m = self.__iter.Next()
            if n is m:  
                return True
        self.__iter.init()
        return False

    def delete(self,n):
        #complexity O(n)
        x = Option()
        while self.__iter.hasNext():
            if n is self.__iter.iter.getHead():
                self.__iter.iter.deletFirst() #delets the element from the list 
                self.__iter.init()
                x.setEl(n)
                return x
            self.__iter.Next()
        print("Element is not in the set", n)
        return x

    def iterator(self):
            self.__iter.init()
            return self.__iter 
        



    


##s = ListSet()
##for i in range(10):
##    s.insert(i)
##    s.insert(i)
##it = s.iterator()
##print("Printing set")
##while it.hasNext():
##     print(it.Next(), end = " ")
##print()
##it.init()
##for i in range(3):
##    s.delete(i)
##s.delete(9)
##print("Printing new set")
##while it.hasNext():
##     print(it.Next(), end = " ")
##print()
##for i in range(9):
##    s.insert(i)
##it.init()
##print("Printing new set")
##while it.hasNext():
##     print(it.Next(), end = " ")
##print()   
##
##v = BitVector(10)
##for i in range(10):
##    v.insert(i)
##it = v.iterator()
##while it.hasNext():
##    print(it.Next(), end = " ")
##print()
##for i in range(5):
##    v.delete(i)
##it.init()
##while it.hasNext():
##    print(it.Next(), end = " ")
##print()
