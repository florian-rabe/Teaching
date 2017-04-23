from Immutable_List import Nil, Cons, Imm_L_I
from abc import ABCMeta, abstractmethod 
from Option import Option, Unit
from Iterator import Iterable




class Stack(Iterable): #abstract class for stack
    __metaclass__ = ABCMeta

    @abstractmethod
    def push(self, el):
        pass

    @abstractmethod
    def pop(self):
        pass

class IL_Stack(Stack): #stack with immutable list
    def __init__(self):
        self.__elements = Nil() #immutable list but stack class mutable
        self.__iter = Imm_L_I()

    def push(self, el): #adds new element in to the list
        #complexity O(1)
        newS = Cons(el, self.__elements)
        self.__elements = newS
        self.__iter.copy(self.__elements)
        x = Unit()
        return x #returns Unit

    
    def top(self): #returns the top element
        #complexity O(1)
        x = Option()
        if self.__elements.emptyList():
            x.setOp(False)
        else:
            x.setEl(self.__elements.getEl(0))
        return x #returns empty option if list is empty

    def pop(self): #pops the first elment if possile
        #complexity O(1)
        x = Option()
        if self.__elements.emptyList():
            raise Exception("Stack is Empty") #raises exception if list is empty
        else:
            x.setEl(self.__elements.getEl(0));
            self.__elements.deletFirst() #deletes the First element if possible
        return x 

    def iterator(self): #creates iterator for the stack
        #complexity O(1)
        return self.__iter

class Queue(Iterable): #abstract class for queue
    __metaclass__ = ABCMeta

    @abstractmethod
     def enqueue(self, el):
        pass

    @abstractmethod
    def dequeue(self):
        pass

class IL_Queue(Queue): #queue with immutable list
    def __init__(self):
        self.__fill = Nil() 
        self.__empty = Nil()
##        self.__concate = Nil()
##        self.__iter = Imm_L_I()


    def enqueue(self, el): #adds a new element into the list
        #complexity O(1)
        newQ = Cons(el, self.__fill)
        self.__fill = newQ
        x = Unit()
        return x #returns Unit object

    def dequeue(self): #deletes the last elemet in the fill
        #complexity O(n) if empty is empty else complexity O(1)
        if self.__empty.emptyList():
            if self.__fill.emptyList():
                raise Exception("Queue is Empty")
            self.__empty = self.__fill.reverse(Nil()) #takes linear time
            self.__fill = Nil()
        op = Option()
        if self.isEmpty(): 
            op.setOp(False)

        else:
            op.setEl(self.__empty.getEl(0))
            self.__empty.deletFirst()
        return op #returns the value
    
    def isEmpty(self): #checks the queue is empty or not
        #complexity O(1)
        return self.__fill.emptyList() and self.__empty.emptyList()

    def print(self): #prints the list
        #complexity O(n)
        print("printing fill")
        self.__fill.print()
        print("printing empty")
        self.__empty.print()


##    def iterator(self):
##        iter = Imm_L_I()
##        try:
##            if self.__fill.emptyList():
##                self.__concate = self.__empty
##            else:
##                self.__concate = self.__concate.concate(self.__fill)
##                self.__concate = self.__concate.concate(self.__empty)
##        iter.copy(self.__concate)
##        return iter
        
        
            
        
            
##print("Stack")       
##s = IL_Stack()
##for i in range(10):
##    s.push(i)
##print("printing stack with iterator")
##it = s.iterator()
##while it.hasNext():
##    print(it.Next(), end = " ")
##print()
##it.init()
##print("popping elements")
##for i in range(5):
##    s.pop().print()
##for i in range(10,13):
##    s.push(i)
##print("printing new stack with iterator")    
##while it.hasNext():
##    print(it.Next(), end = " ")
##print()
##
##print("Queue")
##q = IL_Queue()
##print("Printing queue")
##for i in range(10):
##    q.enqueue(i)
##q.print()
##print("deleting elements")
##for i in range(5):
##    q.dequeue().print()
##print("printing the new queue")
##q.print()
