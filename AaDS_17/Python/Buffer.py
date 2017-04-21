from Iterator import Iterator
from Option import Option, Unit
from Stack_Queue_Immutable import Queue #import abstract class Queue


class Buffer_Iterator(Iterator): #iterator for buffer class

    def __init__(self):
        #complexity O(1)
        self.iter = None
        self.i = 0

    def copy(self, item):
        #complexity O(1)
        self.iter = item
    
    def Next(self):
        #complexity O(1) in phyton index operator takes constant time
        if self.hasNext():
            el = self.iter.container[(self.iter.begin+self.i)%self.iter.capacity]
            self.i += 1
            return el
    def hasNext(self):
        #complexity O(1)
        return self.i < self.iter.size
    
    def init(self):
        #complexity O(1)
        self.i = 0

            
        

class Buffer(Queue):
    def __init__(self, n):
        #complexity O(n) because of the creation of the container
        self.x = Option()#empty option
        self.capacity = n
        self.container = [self.x.getEl()]*n #lenght n list 
        self.begin = self.size = 0

    
    def enqueue(self, el):
        #complexity O(1)
        if self.size is self.capacity:
            raise Exception("BufferOverFlow")
        self.container.insert((self.begin+self.size)%self.capacity,el)
        self.size += 1
        x = Unit()
        return x

    def dequeue(self):
        #complexity O(1)
        if self.size is 0:
            raise Exception("BufferUnderFlow")
        self.x = Option(self.container[self.begin])
        self.size -= 1
        self.begin += 1
        return self.x

    def iterator(self):
        #complexity O(1)
        iter = Buffer_Iterator()
        iter.copy(self)
        return iter
        

##b = Buffer(10)
##it = b.iterator()
##for i in range(10):
##    b.enqueue(i)
##while it.hasNext():
##    print(it.Next(), end = " ")
##print()
##
##for i in range(3):
##    b.dequeue()
##it.init()
##while it.hasNext():
##    print(it.Next(), end = " ")
##print()  
