#ifndef SET_H
#define SET_H
#include <iostream>
#include "iterator.h"
#include "immutable_linked_list.h"

//Abstract class for set iterators
template<class T>
class setIterator : public virtual Iterator<T>{
    public:
        //Pure virtual methods and non-virtual destructor
        virtual ~setIterator(){};
        virtual bool hasNext() = 0;
        virtual T getNext() = 0;
};
//Abstract class for sets
template<class T>
class Set : public virtual Iterable<T>{
    public:
        //Pure virtual methods and non-virtual destructor
        virtual ~Set(){};
        virtual setIterator<T>* iterator() = 0;
        virtual Unit insert(T) = 0;
        virtual Unit Delete(T) = 0;
        virtual bool contains(T) = 0;
};
#endif // SET_H
