#ifndef LIST_SET_H
#define LIST_SET_H
#include <iostream>
#include "set.h"

//Class for list set iterators
template<class T>
class ListSetIterator : public virtual setIterator<T>{
    private:
        //Current portion of the list backing the set to iterate
        ImmutableList<T> *elements;
    public:
        //Methods
        ListSetIterator(ImmutableList<T>*);
        ~ListSetIterator(){};
        bool hasNext();
        T getNext();
        ImmutableList<T>* getEls();
};
//Class for list sets
template<class T>
class ListSet : public virtual Set<T>{
    private:
        //List backing the set
        ImmutableList<T> *elements;
    public:
        //Methods
        ListSet();
        ~ListSet();
        ListSetIterator<T>* iterator();
        Unit insert(T);
        Unit Delete(T);
        bool contains(T);
        ImmutableList<T>* getElements();
};

//Constructor for list set iterators
//Complexity O(1)
template<class T> ListSetIterator<T>::ListSetIterator(ImmutableList<T> *lst){
    this->elements = lst;
}
//hasNext fro list set iterators
//Complexity O(1)
template<class T> bool ListSetIterator<T>::hasNext(){
    return !(this->elements->emptyList());
}
//getNext for list set iterators
//Complexity O(1)
template<class T> T ListSetIterator<T>::getNext(){
    //Assume hasNext was called before
    T els;
    els = elements->getEl(0);//Get current
    this->elements = this->elements->getTail();//Move on to next element
    return els;
}
//Returns the remaining part of the list the iterator has to go through
//Complexity O(1)
template<class T> ImmutableList<T>* ListSetIterator<T>::getEls(){
    return this->elements;
}

//Constructor for list sets
//Complexity O(1)
template<class T> ListSet<T>::ListSet(){
    this->elements = new Nil<T>();
}
//Returns iterator to beginning of list set
//Complexity O(1)
template<class T> ListSetIterator<T>* ListSet<T>::iterator(){
    ListSetIterator<T> *iter = new ListSetIterator<T>(this->elements);
    return iter;
}
//Inserts an element at the end of the list set
//Complexity Theta(l)
template<class T> Unit ListSet<T>::insert(T el){
    if(!this->contains(el)){//Theta(l)
        ListSetIterator<T> *it = this->iterator();
        //If the list was empty, insert at beginning
        if(!it->hasNext()){
            ImmutableList<T> *els = new Cons<T>(el, it->getEls()->getTail());
            this->elements = els;
        }
        else while(it->hasNext()){//If not, traverse list to the end and insert
            if(it->getEls()->getTail()->emptyList()){
                ImmutableList<T> *els = new Cons<T>(el, it->getEls()->getTail());
                it->getEls()->setTail(els);
                break;
            }
            it->getNext();
        }
        delete it;
    }
    Unit w;//Return unit
    return w;
}
//Deletes the element el from the list set
//Complexity Theta(l)
template<class T> Unit ListSet<T>::Delete(T el){
    if(this->contains(el)){//Theta(l)
        ImmutableList<T> *lst = this->elements;
        //If it is in the first position
        if(lst->getEl(0) == el){
            this->elements = this->elements->getTail();
            delete lst;
            lst = NULL;
        }//If it is not, go to position
        else while(lst->getTail()->getEl(0) != el){
            lst = lst->getTail();
        }
        if(lst != NULL){
            //Delete and free memory of element
            ImmutableList<T> *lst2 = lst->getTail();
            lst->setTail(lst->getTail()->getTail());
            delete lst2;
        }
    }
    Unit w;//Return unit
    return w;
}
//Returns true if the set contains the element el
//Complexity Theta(l)
template<class T> bool ListSet<T>::contains(T el){
    ListSetIterator<T> *iter = this->iterator();
    while(iter->hasNext()){
        if(iter->getNext() == el){
            return true;
        }
    }
    return false;
}
//Destructor for list sets
//Complexity Theta(l)
template<class T> ListSet<T>::~ListSet(){
    ImmutableList<T> *els;
    //Deletes all elements
    while(!this->elements->emptyList()){
        els = this->elements;
        this->elements = this->elements->getTail();
        delete els;
    }
    delete this->elements;
}
//Returns underlying list
//Complexity O(1)
template<class T> ImmutableList<T>* ListSet<T>::getElements(){
    return this->elements;
}
#endif // LIST_SET_H
