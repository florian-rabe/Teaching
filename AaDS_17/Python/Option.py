

class Unit: #empty class
    pass


class Option:
    def __init__(self,x=None):
        #complexity O(1)
        if x:
            self.__op = True 
        else:
            self.__op = False
        self.__el = x 
        
    def setOp(self, b):
        #complexity O(1)
        self.__op = b

    def setEl(self, el):
        #complexity O(1)
        self.__el = el
        self.__op = True

    def getEl(self):
        #complexity O(1)
        if self.isDefined():
            return self.__el
        return None

    def isDefined(self):
        #complexity O(1)
        return self.__op

    def print(self):
        #complexity O(1)
        print(self.__el)
        
