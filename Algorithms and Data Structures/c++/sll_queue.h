#ifndef SLL_QUEUE_H
#define SLL_QUEUE_H
#include <iostream>
#include "immutable_linked_list.h"
#include "option.h"

//Class for unit data structure
class Unit{
};

//Class for queue backed by two immutable linked lists
template<class T>
class SLLQueue{
    private:
        ImmutableList<T> *first;
        ImmutableList<T> *second;
    public:
        SLLQueue();
        ~SLLQueue();
        Unit enqueue(T);
        MyOption<T> dequeue();
        bool isEmpty();
};
//Constructor for queue
//Complexity O(1)
template<class T> SLLQueue<T>::SLLQueue(){
    Nil<T> *x = new Nil<T>();
    this->first = x;
    Nil<T> *y = new Nil<T>();
    this->second = y;
}
//Destructor for queue
//Complexity Theta(n)
template<class T> SLLQueue<T>::~SLLQueue(){
    while(!this->isEmpty()){
        this->dequeue();
    }
    delete(first);
    delete(second);
}
//ENQUEUE
//Complexity O(1)
template<class T> Unit SLLQueue<T>::enqueue(T el){
    //Allocates memory for new element in second list
    Cons<T> *x = new Cons<T>(el, this->second);
    this->second = x;
    Unit y;
    return y;
}
//DEQUEUE
//Best case O(1), worst case Theta(n)
template<class T> MyOption<T> SLLQueue<T>::dequeue(){
    //In case this is satisfied we have Theta(n)
    if(first->emptyList()){
        if(second->emptyList()){
            throw "Queue is empty!";
        }
        first = rev(second);
        second = new Nil<T>();
    }
    //This takes constant time O(1)
    MyOption<T> option;
    if(this->isEmpty()){
        option.setOp(false);
    }
    else{
        option.setEl(first->getEl(0));
        //Taking out element
        ImmutableList<T> *y;
        y = first;
        first = first->getTail();
        //Deallocating its memory
        delete(y);
    }
    return option;
}
//Returns true if the queue is empty
//Complexity O(1)
template<class T> bool SLLQueue<T>::isEmpty(){
    bool cond = false;
    if(first->emptyList() && second->emptyList()){
        cond = true;
    }
    return cond;
}
#endif // SLL_QUEUE_H
