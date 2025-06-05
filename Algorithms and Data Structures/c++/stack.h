#ifndef STACK_H
#define STACK_H
#include <iostream>
#include "immutable_linked_list.h"
#include "option.h"

//Class unit for empty elements
class Unit{
};

//Class for Stacks
template<class T>
class Stack{
    private:
        //Mutable field for immutable Linked List
        ImmutableList<T>* elements;
    public:
        //Methods
        Stack();
        ~Stack();
        Unit push(T);
        MyOption<T> top();
        MyOption<T> pop();
};

//Constructor for stacks
//Complexity O(1)
template<class T> Stack<T>::Stack(){
    //Memory allocation for new list
    Nil<T> *nil = new Nil<T>();
    this->elements = nil;
}
//Destructor for stacks
//Complexity Theta(n)
template<class T> Stack<T>::~Stack(){
    //Frees memory for all elements within stack
    while(this->top().isDefined()){
        this->pop();
    }
    delete(elements);
}
//PUSH
//Complexity O(1)
template<class T> Unit Stack<T>::push(T el){
    //Allocates memory for new element
    Cons<T> *x = new Cons<T>(el, this->elements);
    this->elements = x;
    Unit y;
    return y;
}
//TOP
//Complexity O(1)
template<class T> MyOption<T> Stack<T>::top(){
    //Returns the element at the top as an option
    MyOption<T> x;
    //If the list is nil, the option returns false
    if(elements->emptyList()){
        x.setOp(false);
    }
    else{
        //The function getEl will have complexity O(1)
        //  when called for 0
        x.setEl(elements->getEl(0));
    }
    return x;
}
//POP
//Complexity O(1)
template<class T> MyOption<T> Stack<T>::pop(){
    //Returns the element at the top as an option
    MyOption<T> x;
    if(elements->emptyList()){
        x.setOp(false);
    }
    else{
        //As above, the function getEl will have complexity O(1)
        //  when called for 0
        x.setEl(elements->getEl(0));
        //Popping the element out
        ImmutableList<T> *y;
        y = elements;
        elements = elements->getTail();
        //Deallocating its memory
        delete(y);
    }
    return x;
}
#endif // STACK_H
