#ifndef LISTS_H
#define LISTS_H
#include <iostream>

template<typename T>
class MyList<T>;

//Data Structure definition for Polymorphic Nodes
template<typename T>
class Node{
    friend class MyList<T>;
    private:
        T value;
        Node *next;
    public:
        Node(T, Node*);
        Node(const Node<T> &);
        Node();
};
//Data Structure definition for polymorphic Lists
template<typename T>
class MyList{
    private:
        Node<T> *head;
        MyList(Node<T>*);
    public:
        MyList();
        MyList(const MyList<T> &);
        ~MyList();
        void print();
        void preappend(T);
        void append(T);
        void delFirst();
        void emptyList();
        void reverse();
        void copyList(const MyList<T> &);
        void swapEl(int, int);
        void copyArray(T*, int);
        T* toArray();
        Node<T>* getEl(int);
        int length();
        bool isEmpty();
        MyList<T>* sublist(int,int);
        T getElement(int);
        template<class Q>
        friend MyList<Q>* concat(MyList<Q>*, MyList<Q>*);
};

//NODES

//Parametric constructor for Nodes
//Complexity O(1)
template<typename T> Node<T>::Node(T val, Node<T> *nd){
    this->value = val;
    this->next = nd;
}
//Constructor for Empty Node
//Complexity O(1)
template<typename T> Node<T>::Node(){
    this->next = NULL;
}
//Copy constructor for Node
template<typename T> Node<T>::Node(const Node<T> &nd){
    this->next = NULL;
    this->value = nd.value;
}

//LISTS

//Parametric constructor for Lists
//Complexity O(1)
template<typename T> MyList<T>::MyList(Node<T> *hd){
    this->head = hd;
}
//Constructor for Empty List
//Complexity O(1)
template<typename T> MyList<T>::MyList(){
    this->head = NULL;
}
//Copy Constructor for lists
//Complexity Theta(length)
template<typename T> MyList<T>::MyList(const MyList<T> &lst){
    this->copyList(lst);
}
//Destructor for Lists
//Complexity Theta(l)
template<typename T> MyList<T>::~MyList(){
    this->emptyList();
}

//Functions

//Copies a List
//Complexity Theta(l)
template<typename T> void MyList<T>::copyList(const MyList<T> &lst){
    if(lst.head != NULL){
        Node<T> *a;
        //Copies each element
        for(a = lst.head; a != NULL; a = a->next){
            this->preappend(a->value);
        }
        this->reverse();
    }
}
//Empties a list
//Complexity Theta(l)
template<typename T> void MyList<T>::emptyList(){
    //Deallocates memory of all Nodes
    Node<T> *node = this->head;
    while(node != NULL){
        this->head = this->head->next;
        delete(node);
        node = this->head;
    }
}
//Pre-appends an element to a list
//Complexity Theta(1)
template<typename T> void MyList<T>::preappend(T el){
    //Allocating memory for new element
    Node<T> *node = new Node<T>;
    if(node == NULL){
        std::cout << "Error allocating memory" << std::endl;
        exit(-1);
    }
    //Setting value and position at front of the list
    node->value = el;
    if(this->head == NULL){
        node->next = NULL;
    }
    else{
        node->next = this->head;
    }
    this->head = node;
}
//Appends an element to the end of the list
//Complexity Theta(length)
template<typename T> void MyList<T>::append(T el){
    //If list is empty, same as pre-append
    if(this->head == NULL){
        this->preappend(el);
        return;
    }
    //Allocating memory for new element
    Node<T> *node = new Node<T>;
    if(node == NULL){
        std::cout << "Error allocating memory" << std::endl;
        exit(-1);
    }
    Node<T> *a;
    node->value = el;
    node->next = NULL;
    //Go through the list
    for(a = this->head; a != NULL; a = a->next){
        //Append element
        if(a->next == NULL){
            a->next = node;
            break;
        }
    }
}
//Prints all elements in a list separated by newline
//Complexity Theta(length)
template<typename T> void MyList<T>::print(){
    Node<T> *a;
    for(a = this->head; a != NULL; a = a->next){
        std::cout << a->value << "\n";
    }
    std::cout << std::endl;
}
//Concatenates two lists and returns a new one
//Complexity Theta(length(a) + length(b))
template<typename T> MyList<T>* concat(MyList<T> *a, MyList<T> *b){
    MyList<T> lst;
    MyList<T> *retList;
    Node<T> *c;
    //First pre-appends all elements of both lists
    //This is Theta(length(a))
    for(c = a->head; c != NULL; c = c->next){
        lst.preappend(c->value);
    }
    //This is theta(length(b))
    for(c = b->head; c != NULL; c = c->next){
        lst.preappend(c->value);
    }
    //Now reverts list and returns
    //Theta(length(a) + length(b))
    retList = lst.reverse();
    return retList;
}
//Reverts the list (mutable)
//Complexity Theta(length)
template<typename T> void MyList<T>::reverse(){
    Node<T> *prevNode;
    Node<T> *currNode;
    Node<T> *nextNode;
    currNode = this->head;
    nextNode = NULL;
    prevNode = NULL;
    while(currNode != NULL){
        nextNode = currNode->next;
        currNode->next = prevNode;
        prevNode = currNode;
        currNode = nextNode;
    }
    this->head = prevNode;
}
//Returns the length of a list
//Complexity Theta(l)
template<typename T> int MyList<T>::length(){
    int length = 0;
    Node<T> *a;
    for(a = this->head; a != NULL; a = a->next){
        length++;
    }
    return length;
}
//Checks if a list is empty
//Complexity O(1)
template<typename T> bool MyList<T>::isEmpty(){
    return (this->head == NULL);
}
//Deletes the first element in a list
//Complexity O(1)
template<typename T> void MyList<T>::delFirst(){
    if(this->head == NULL){
        std::cout << "Deleting error!\nTrying to delete a non-existing element." << std::endl;
        return;
    }
    if(this->head->next == NULL){
        delete(this->head);
        this->head = NULL;
    }
    else{
        Node<T> *nd;
        nd = this->head;
        this->head = this->head->next;
        delete(nd);
    }
}
//Gets an element in a list
//Complexity Theta(n), where n is the element's position
template<typename T> T MyList<T>::getElement(int a){
    int n = this->length;
    int i;
    if(a >= n){
        std::cout << "Error!\nTrying to access a non-existing element." << std::endl;
        T x = -1;
        return x;
    }
    Node<T> *nd;
    nd = this->head;
    for(i = 0; i <= a; i++){
        if(i == a){
            return nd->value;
        }
        nd = nd->next;
    }
}
//Creates a sublist from list between position parameters
//Worst-case complexity Theta(b)
template<typename T> MyList<T>* MyList<T>::sublist(int a, int b){
    int n = this->length();
    if(a >= n || b >= n || a > b){
        std::cout << "Sublist error!\nWrong function parameters." << std::endl;
        return this;
    }
    else{
        Node<T> *nb;
        MyList<T> *lst;
        lst = new MyList<T>;
        nb = this->head;
        int i;
        for(i = 0; i < a; i++){
            nb = nb->next;
        }
        for(; i <= b; i++){
            lst->preappend(nb->value);
            nb = nb->next;
        }
        lst->reverse();
        return lst;
    }
}
template<typename T> Node<T>* MyList<T>::getEl(int a){
    if(a == 0){
        return this->head;
    }
    Node<T> *nd;
    nd = this->head;
    int i;
    for(i = 0; i < a; i++){
        nd = nd->next;
    }
    return nd;
}
template<typename T> void MyList<T>::swapEl(int a, int b){
    if(a == b){
        return;
    }
    int n = this->length();
    if(a >= n || b >= n){
        std::cout << "Swapping error!\nAttempting to swap non-existing element." << std::endl;
        return;
    }
    Node<T> *first;
    Node<T> *second;
    Node<T> *prevFirst;
    Node<T> *prevSecond;
    Node<T> *temp;
    prevFirst = NULL;
    prevSecond = NULL;
    first = this->head;
    second = this->head;
    //Make a always smaller
    if(a > b){
        int c;
        c = a;
        a = b;
        b = c;
    }
    int i;
    //O(n)
    for(i = 0; i <= a; i++){
        if(i+1 == a){
            prevFirst = first;
        }
        if(i == a){
            break;
        }
        first = first->next;
    }
    for(i = 0; i <= b; i++){
        if(i+1 == b){
            prevSecond = second;
        }
        if(i == b){
            break;
        }
        second = second->next;
    }
    //If the elements are contiguous
    //O(1)
    if(a+1 == b){
        first->next = second->next;
        second->next = first;
        if(a == 0){
            this->head = second;
        }
        else{
            prevFirst->next = second;
        }
    }
    //If there are elements in between
    //O(1)
    else{
        temp = first->next;
        prevSecond->next = first;
        first->next = second->next;
        second->next = temp;
        if(a == 0){
            this->head = second;
        }
        else{
            prevFirst->next = second;
        }
    }
}
template<typename T> T* MyList<T>::toArray(){
    int n = this->length;
    T *arr;
    arr = new T[n];
    int i;
    Node<T> *a;
    a = this->head;
    for(i = 0; i < n; i++){
        arr[i] = a->value;
        a = a->next;
    }
    return arr;
}
template<typename T> void MyList<T>::copyArray(T *arr, int n){
    int i;
    this->emptyList();
    for(i = n-1; i >= 0; i--){
        this->prepend(arr[i]);
    }
}
#endif //LISTS_H
