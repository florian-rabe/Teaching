#ifndef OPTION_H
#define OPTION_H
#include <iostream>

//Class for options
template<class T>
class MyOption{
    private:
        //Parameters
        T x;
        bool op;
    public:
        //Methods
        MyOption();
        MyOption(T);
        void setOp(bool);
        void setEl(T);
        bool isDefined();
        T getEl();
        //Printing
        template<class Q>
        friend std::ostream& operator<<(std::ostream&, const MyOption<Q> &);
};

//Empty constructor for options
//Complexity O(1)
template<class T> MyOption<T>::MyOption(){
    this->op = false;
}
//Parametric constructor for options
//Complexity O(1)
template<class T> MyOption<T>::MyOption(T el){
    this->op = true;
    this->el = el;
}
//Setter for the boolean
//Complexity O(1)
template<class T> void MyOption<T>::setOp(bool a){
    this->op = a;
}
//Setter for the element (sets boolean to true)
//Complexity O(1)
template<class T> void MyOption<T>::setEl(T a){
    this->op = true;
    this->x = a;
}
//Returns the boolean
//Complexity O(1)
template<class T> bool MyOption<T>::isDefined(){
    return this->op;
}
//Returns the element
//Complexity O(1)
template<class T> T MyOption<T>::getEl(){
    return this->x;
}
//Operator overloading of printing (<<) for options
//Complexity O(1)
template<class T> std::ostream& operator<<(std::ostream& os, const MyOption<T> &y){
    if(y.op){
        os << y.x;
    }
    else{
        os << "NONE";
    }
    return os;
}
#endif // OPTION_H
