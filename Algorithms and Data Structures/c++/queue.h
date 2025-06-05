#ifndef QUEUE_H
#define QUEUE_H
#include <iostream>
#include "doubly_linked_list.h"
#include "option.h"

//Class for unit data structure
class Unit{
};

//Class for queues backed by a doubly-linked list
template<class T>
class Queue{
    private:
        MyDList<T> elements;
    public:
        Queue();
        ~Queue();
        Unit enqueue(T);
        MyOption<T> dequeue();
        bool isEmpty();
};

//Constructor for queues (empty)
template<class T> Queue<T>::Queue(){
}
//Destructor for queues
//Complexity Theta(n)
template<class T> Queue<T>::~Queue(){
    while(!this->isEmpty()){
        this->dequeue();
    }
}
//ENQUEUE
//Complexity O(1)
template<class T> Unit Queue<T>::enqueue(T el){
    Unit x;
    this->elements.append(el);
    return x;
}
//DEQUEUE
//Complexity O(1)
template<class T> MyOption<T> Queue<T>::dequeue(){
    MyOption<T> option;
    if(this->isEmpty()){
        option.setOp(false);
    }
    else{
        //Complexity O(1) because we access first element with getElement(0)
        option.setEl(this->elements.getElement(0));
        this->elements.remFirst();
    }
    return option;
}
//Returns true if queue is empty
//Complexity O(1)
template<class T> bool Queue<T>::isEmpty(){
    return (this->elements.getHead() == NULL);
}
#endif // QUEUE_H
