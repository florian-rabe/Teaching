#ifndef ITERATOR_H
#define ITERATOR_H
#include <iostream>

//Class for unit
class Unit{
};

//Abstract base class for iterators
template<class T>
class Iterator{
    public:
        //Pure virtual methods and non-virtual destructor
        virtual ~Iterator(){};
        virtual bool hasNext() = 0;
        virtual T getNext() = 0;
};
//Abstract base class for iterables
template<class T>
class Iterable{
    public:
        //Pure virtual methods and non-virtual destructor
        virtual ~Iterable(){};
        virtual Iterator<T>* iterator() = 0;
};

//Foreach function
//Complexity Theta(l)
template<class T> Unit foreach(Iterator<T> *it, Unit (*func)(T)){
    while(it->hasNext()){
        func(it->getNext());
    }
    delete it;
    Unit x;
    return x;
}
#endif
