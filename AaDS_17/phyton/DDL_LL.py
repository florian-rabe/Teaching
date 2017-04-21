from List import MutableList, List_Iterator
End = Exception("End of the List!")

class Mm_L_I(List_Iterator): #iterator for going forward
    def Next(self):
        #complexity O(1)
        if self.hasNext():
            data = self.iter.data
            self.iter = self.iter.next
            return data
        raise End

    def hasNext(self):
        #complexity O(1)
        if self.iter:
            return True
        return False

class DDL_I(Mm_L_I): #iterator for going backward for DDL
    def Prev(self):
        #complexity O(1)
        if self.hasPrev():
            data = self.iter.data
            self.iter = self.iter.prev
            return data
        raise End
    def hasPrev(self):
        #complexity O(1)
        if self.iter:
            return True
        return False


class Node: #helper class
    def __init__(self, _data = None, _next = None, _prev = None):
        #complexity O(1)
        self.data = _data
        self.next = _next
        self.prev = _prev


class LinkedList(MutableList): 
    def __init__(self):
        #complexity O(1)
        self.__head = None


    def insert(self, data):
        #complexity O(1)
        newNode = Node(data, self.__head)
        self.__head = newNode


    def setHead(self, head):
        #complexity O(1)
        self.__head = head

    def getHead(self):
        #complexity O(1)
        return self.__head

    def getData(self):
        #complexity O(1)
        return self.__data
    
    def delete(self, data):
        #complexity O(n)
        current = self.__head
        if current.data == data: #check is it the first element or not
            current = current.next
            return data
        while current.next:
            if current.next.data is data: #if we are checking the current then
                                          #we couldnt delete the current because
                                          #we dont have access to the previous current
                current.next = current.next.next
                return data
            current = current.next
        return "Node is not in the List!"

    def reverse(self):
        #complexity O(n)
        current = self.__head
        p = next = None
        while current:
            next = current.next
            current.next = p
            p = current
            current = next
        self.__head = p

    def print(self):
        #complexity O(n)
        tmp = self.__head
        if tmp:
            while tmp:
                print(tmp.data, end = " ")
                tmp = tmp.next
            print()
            return
        print("None")

    def iterator(self):
        #complexity O(1)
        iter = Mm_L_I()
        iter.copy(self.__head)
        return iter
        
class DoubleLinkedList(MutableList):
    def __init__(self):
        #complexity O(1)
        self.__head = self.__tail = None
         
    def insert(self, data):
        #complexity O(1)
         if self.__head is None:
             newNode = Node(data)
             self.__head = newNode
             self.__tail = newNode
         else:
             newNode = Node(data, None, self.__tail) #we are inserting at the back
             newNode.prev.next = newNode
             self.__tail = newNode
    
    def delete(self, data):
        #complexity O(n)
        current = self.__head
        while current:
            if current.data is data:
                if current.prev: #checks is data the first element
                    if current.next: #check is data the last elements
                        current.prev.next = current.next
                        current.next.prev = current.prev
                    else: #if it is the last element
                        current.prev.next = None
                        self.__tail = current.prev
                    return data
                else: #if it is the first elemet
                    self.__head = crurrent.next
                    self.__head.prev = None
                    return data
            current = current.next
        return "Node is not in the List!"

    def getHead(self):
        #complexity O(1)
        return self.__head


    def getData(self):
        #complexity O(1)
        return self.__head.data

    def reverse(self):
        #complexity O(n)
        current = self.__head
        #changing the direction of the next and prev pointers
        while current:  
            tmp = current.prev 
            current.prev = current.next
            current.next = tmp
            current = current.prev
        tmp = self.__head
        self.__head = self.__tail
        self.__tail = tmp
            
  
    def print(self):
        #complexity O(n)
        tmp = self.__head
        while tmp: #printing list forward
            print(tmp.data, end = " ")
            tmp = tmp.next
        print()
        tmp = self.__tail
        while tmp: #printing list backward
            print(tmp.data, end = " ")
            tmp = tmp.prev
        print()

    def iterator(self):
        #complexity O(1)
        iter = DDL_I()
        iter.copy(self.__head)
        return iter






##List = LinkedList()
##dList = DoubleLinkedList() 
##for i in range(10):
##    List.insert(i)
##    dList.insert(i)
##
##print("Printing Linked List")
##List.print()
##print("Printing Linked List reversed")
##List.reverse()
##List.print()
##print("Printing Double Linked List")
##dList.print()
##
##itL = List.iterator()
##itdl = dList.iterator()
##print("Printing Linked List with Iterator")
##while itL.hasNext():
##    print(itL.Next(), end = " ")
##print()
##print("printing first 5 elements of the DLL with iterator")
##for i in range(5):
##    print(itdl.Next(), end = " ")
##print()
##print("Printing Double Linked List backwards with Iterator")
##while itdl.hasPrev():
##    print(itdl.Prev(), end = " ")
##print()
##itdl.init()
##print("Printing Double Linked List forward with Iterator")
##while itdl.hasNext():
##    print(itdl.Next(), end = " ")
##print()        
