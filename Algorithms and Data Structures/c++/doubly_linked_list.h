#ifndef DOUBLY_LINKED_LIST_H
#define DOUBLY_LINKED_LIST_H
#include <iostream>

//Class template for doubly linked lists
template<class T>
class MyDList;

//Class for nodes of doubly linked lists
template<class T>
class DLNode{
    friend class MyDList<T>;
    private:
        //Elements
        T value;
        DLNode *next;
        DLNode *prev;
    public:
        //Methods
        DLNode(T);
        DLNode(const DLNode<T> &);
        DLNode();
};

//Class for Doubly linked lists
template<class T>
class MyDList{
    private:
        //Elements
        DLNode<T> *head;
        DLNode<T> *last;
    public:
        //Methods
        MyDList(DLNode<T>*, DLNode<T>*);
        MyDList();
        MyDList(const MyDList<T> &);
        ~MyDList();
        void copyList(const MyDList<T> &);
        void emptyList();
        void append(T);
        void preappend(T);
        void remFirst();
        DLNode<T>* getHead();
        int length();
        T getElement(int);
};

//Parametric Constructor for nodes
//Complexity O(1)
template<class T> DLNode<T>::DLNode(T x){
    this->value = x;
    this->prev = NULL;
    this->next = NULL;
}
//Copy Constructor for nodes
//Complexity O(1)
template<class T> DLNode<T>::DLNode(const DLNode<T> &nd){
    this->value = nd.value;
    this->next = NULL;
    this->prev = NULL;
}
//Empty Constructor for nodes
//Complexity O(1)
template<class T> DLNode<T>::DLNode(){
    this->next = NULL;
    this->prev = NULL;
}
//Parametric Constructor for doubly linked lists
//Complexity O(1)
template<class T> MyDList<T>::MyDList(DLNode<T> *hd, DLNode<T> *tl){
    this->head = hd;
    this->last = tl;
}
//Empty Constructor for doubly linked lists
//Complexity O(1)
template<class T> MyDList<T>::MyDList(){
    this->head = NULL;
    this->last = NULL;
}
//Copy Constructor for doubly linked lists
//Complexity Theta(n)
template<class T> MyDList<T>::MyDList(const MyDList<T> &lst){
    this->copyList();
}
//Destructor for doubly linked lists
//Complexity Theta(n)
template<class T> MyDList<T>::~MyDList(){
    this->emptyList();
}

//Copies elements of one list into another
//Complexity Theta(n)
template<class T> void MyDList<T>::copyList(const MyDList<T> &lst){
    if(lst.head != NULL){
        DLNode<T> *a;
        //Copies each element
        for(a = lst.head; a != NULL; a = a->next){
            this->append(a->value);
        }
    }
}
//Empties a list
//Complexity Theta(n)
template<class T> void MyDList<T>::emptyList(){
    //Deallocates memory for all nodes
    DLNode<T> *node = this->head;
    while(node != NULL){
        this->head = this->head->next;
        this->head->next->prev = NULL;
        delete(node);
        node = this->head;
    }
}
//Appends an element to the end of a doubly linked list
//Complexity O(1)
template<class T> void MyDList<T>::append(T el){
    //Memory allocation for new node
    DLNode<T> *nd = new DLNode<T>(el);
    if(nd == NULL){
        throw "Error allocating memory!";
    }
    nd->next = NULL;
    //Change of pointers for both cases (list empty/non-empty)
    if(this->head == NULL){
        this->head = nd;
        this->last = nd;
        nd->prev = NULL;
    }
    else{
        this->last->next = nd;
        nd->prev = this->last->next;
        this->last = nd;
    }
}
//Appends an element to the beginning of a doubly linked list
//Complexity O(1)
template<class T> void MyDList<T>::preappend(T el){
    //Memory allocation for new node
    MyDList<T> *nd = new DLNode<T>(el);
    if(nd == NULL){
        throw "Error allocating memory!";
    }
    nd->prev = NULL;
    //Change of pointers for both cases (list empty/non-empty)
    if(this->head == NULL){
        nd->next = NULL;
        this->last = nd;
    }
    else{
        nd->next = this->head;
    }
    this->head = nd;
}
//Gets the element in position a
//Complexity Theta(a)
template<class T> T MyDList<T>::getElement(int a){
    //If a is negative - error
    if(a < 0){
        throw "Error!\nTrying to access a non-existent element.";
    }
    int i;
    DLNode<T> *nd;
    nd = this->head;
    for(i = 0; i < a; i++){
        //If element a is bigger than the length of the list - error
        if(nd == NULL){
            throw "Error!\nTrying to access a non-existent element.";
        }
        nd = nd->next;
    }
    return nd->value;
}
//Returns the length of the list
//Complexity Theta(n)
template<class T> int MyDList<T>::length(){
    int n = 0;
    DLNode<T> *a;
    for(a = this->head; a != NULL; a = a->next){
        n++;
    }
    return n;
}
//Remove the first element from a doubly linked list
//Complexity O(1)
template<class T> void MyDList<T>::remFirst(){
    if(this->head == NULL){
        throw "Error!\nTrying to remove a non-existent element.";
    }
    else{
        DLNode<T> *nd;
        //Changing pointers
        nd = this->head;
        this->head = nd->next;
        if(this->head != NULL){
            this->head->prev = NULL;
        }
        else{
            this->last = NULL;
        }
        //Deallocating memory
        delete(nd);
    }
}
//Returns the pointer to the first element of the list
//Complexity O(1)
template<class T> DLNode<T>* MyDList<T>::getHead(){
    return this->head;
}
#endif // DOUBLY_LINKED_LIST_H
