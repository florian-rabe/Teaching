from abc import ABCMeta, abstractmethod #imports abstract class library
from Iterator import Iterable, Iterator 
Empty = Exception("Nil list is always Empty!")



#iterator for list inherits from Iterator class
class List_Iterator(Iterator):
    def __init__(self):
        self.iterH = self.iter = None

    def copy(self, item):
        self.iterH = self.iter = item

    def init(self): #sets the iter object to the beginning of the list
        self.iter = self.iterH        

    @abstractmethod
    def Next(self):
        pass

    @abstractmethod
    def hasNext(self):
        pass

    
#List class inherits from Iterable class
class List(Iterable):
    __metaclass__ = ABCMeta



class ImmutableList(List):
    __metaclass__ = ABCMeta
    @abstractmethod
    def emptylist(self): #checks is list empty or not
        pass

    @abstractmethod
    def getEl(self,n): #returns the element at position n
        pass
    @abstractmethod
    def getTail(self): #returns the tail of the list
        pass

##    @abstractmethod
##    def setTail(sefl,tail): #sets the tail of the list
##        pass

##    @abstractmethod
##    def setHead(self,n): #sets the first element of the list
##        pass

    @abstractmethod
    def getHead(self): #returns to first element of the list
        pass

    @abstractmethod
    def reverse(self): #returns a new list which is the reverse of the list
        pass

    @abstractmethod
    def print(self):    #prints the list whitout using iterator
        pass

    @abstractmethod
    def iterator(self): #creates iterator for the list
        pass

    @abstractmethod
    def deleteFirst(self): #delets the first element if possible
        pass

##    @abstractmethod
##    def concate(self, l):
##        pass



class MutableList(List):
    __metaclass__ = ABCMeta
    @abstractmethod
    def insert(self, n): #prepends the n
        pass

    @abstractmethod
    def reverse(self):  #reverse the list
        pass

    @abstractmethod
    def delete(self, n): #deletes the element n in the list
        pass

    @abstractmethod
    def setHead(self, head): #sets the head
        pass

    @abstractmethod
    def getHead(self):  #return to Head of the list
        pass

    @abstractmethod
    def getData(self): #returns to first element in the list
        pass

    @abstractmethod
    def print(self): #prints the elements of the list without using iterator
        pass

    @abstractmethod
    def iterator(self): #creates an iterator for the list
        pass

    
