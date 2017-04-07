#ifndef IMMUTABLE_LINKED_LIST_H
#define IMMUTABLE_LINKED_LIST_H
#include <iostream>

//Abstract class for lists
template<class T>
class ImmutableList{
    public:
        //Pure virtual methods
        virtual ~ImmutableList(){};
        virtual bool emptyList() = 0;
        virtual T getEl(int) = 0;
        virtual ImmutableList<T>* getTail() = 0;
        virtual void setTail(ImmutableList<T>*) = 0;
        virtual void setHead(T) = 0;
        template<class Q>
        friend ImmutableList<T>* rev(ImmutableList<T>*);
};
//Class for Nil constructor for lists (empty list)
template<class T>
class Nil : public ImmutableList<T>{
    public:
        //Methods
        bool emptyList();
        T getEl(int);
        ImmutableList<T>* getTail();
        void setTail(ImmutableList<T>*);
        void setHead(T);
};
//Class for Cons constructor for lists
template<class T>
class Cons : public ImmutableList<T>{
    private:
        //Parameters
        T head;
        ImmutableList<T>* tail;
    public:
        //Methods
        Cons(T, ImmutableList<T>*);
        bool emptyList();
        T getEl(int);
        ImmutableList<T>* getTail();
        void setTail(ImmutableList<T>*);
        void setHead(T);
};
//Constructor for Cons
//Complexity O(1)
template<class T> Cons<T>::Cons(T hd, ImmutableList<T>* tl){
    this->head = hd;
    this->tail = tl;
}
//Returns if list is empty for Nil (always true)
//Complexity O(1)
template<class T> bool Nil<T>::emptyList(){
    return true;
}
//Returns if list is empty for Cons (always false)
//Complexity O(1)
template<class T> bool Cons<T>::emptyList(){
    return false;
}
//Returns element a for empty lists (will always give an error - list is empty)
//Complexity O(1)
template<class T> T Nil<T>::getEl(int a){
    throw "Cannot get non-existent element! Class Nil is empty!";
}
//Returns element a for non-empty lists
//Complexity Theta(a)
template<class T> T Cons<T>::getEl(int a){
    //Throw exception if a is negative
    if(a < 0){
        throw "Cannot get non-existent element!";
    }
    else if(a == 0){
        return this->head;
    }
    //Recursion to find element
    else{
        return this->tail->getEl(a - 1);
    }
}
//Returns the tail of an empty list (none - empty list too)
//Complexity O(1)
template<class T> ImmutableList<T>* Nil<T>::getTail(){
    return this;
}
//Returns the tail of a list
//Complexity O(1)
template<class T> ImmutableList<T>* Cons<T>::getTail(){
    return this->tail;
}
//Auxiliary function for reversing a list
//Complexity Theta(length(x))
template<class T> ImmutableList<T>* rev_aux(ImmutableList<T> *x, ImmutableList<T> *res){
    if(x->emptyList()){
        return res;
    }
    else{
        ImmutableList<T> *y = new Cons<T>(x->getEl(0), res);
        return rev_aux(x->getTail(), y);
    }
}
//Returns reversed list
//Complexity Theta(n)
template<class T> ImmutableList<T>* rev(ImmutableList<T> *lst){
    Nil<T> *x = new Nil<T>();
    return rev_aux(lst, x);
}
//Sets the tail of Nil, returns error
//Complexity O(1)
template<class T> void Nil<T>::setTail(ImmutableList<T> *lst){
    throw "Error! Attempting to change tail of an empty list!";
 }
//Sets the tail of an immutable list
//Complexity O(1)
 template<class T> void Cons<T>::setTail(ImmutableList<T> *lst){
    this->tail = lst;
 }
 //Sets the head of Nil, returns error
 //Complexity O(1)
 template<class T> void Nil<T>::setHead(T el){
    throw "Error! Attempting to change head of an empty list!";
 }
 //Sets the head of Cons
 //Complexity O(1)
 template<class T> void Cons<T>::setHead(T el){
    this->head = el;
 }
#endif // IMMUTABLE_LINKED_LIST_H
