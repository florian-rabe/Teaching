#ifndef BUFFER_H
#define BUFFER_H
#include <iostream>
#define BUF_UNDERFLOW "Buffer Underflow!"
#define BUF_OVERFLOW "Buffer Overflow!"

template<class T>
class Buffer{
    private:
        int n;
        int Begin;
        int Size;
        T *elements;
    public:
        Buffer(int n);
        ~Buffer();
        void enqueue(T);
        T dequeue();
};

//Constructor for buffer of n size
template<class T> Buffer<T>::Buffer(int n){
    this->n = n;
    this->Begin = 0;
    this->Size = 0;
    elements = new T[n];
}
//Destructor for Buffer
//Deallocates memory of array
template<class T> Buffer<T>::~Buffer(){
    delete(this->elements);
}

//Enqueue Function
//Adds element to end of buffer
template<class T> void Buffer<T>::enqueue(T el){
    if(this->Size == n){
        throw BUF_OVERFLOW
    }
    else{
        elements[(this->Begin + this->Size)%(this->n)] = el;
        this->Size++;
    }
}

//Dequeue Function
//Retrieves Element from Start of Buffer
template<class T> T Buffer<T>::dequeue(){
    if(this->Size == 0){
        throw BUF_UNDERFLOW;
    }
    else{
        T x = elements[this->Begin];
        this->Begin = (this->Begin + 1)%(this->n);
        this->Size--;
    }
}
#endif // BUFFER_H
