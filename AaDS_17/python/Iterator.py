#mother classes
from abc import ABCMeta, abstractmethod



class Iterator:
    __metaclass__ = ABCMeta
    @abstractmethod
    def hasNext(self):
        pass

    @abstractmethod
    def Next(self):
        pass

    @abstractmethod
    def copy(self, item):
        pass

##    @abstractmethod
##    def concate(self, it):
##        pass



class Iterable:
    __metaclass__ = ABCMeta

    @abstractmethod
    def iterator(self):
        pass

    
