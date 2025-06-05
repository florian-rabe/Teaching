#ifndef ORDER_H
#define ORDER_H
#include <iostream>

//Abstract data structure for orders
template<class T>
class Ord{
    public:
        //Virtual methods
        virtual bool lessOrEqual(T, T);
};

//Instances of order
class IntSmaller : public Ord<int> {
    public:
        bool lessOrEqual(int, int);
};
class IntGreater : public Ord<int> {
    public:
        bool lessOrEqual(int, int);
};
class Divisible : public Ord<int> {
    public:
        bool lessOrEqual(int, int);
};
class Lexicographic : public Ord<std::string> {
    public:
        bool lessOrEqual(std::string, std::string);
};

//DECLARATIONS
bool IntSmaller::lessOrEqual(int x, int y){
    return x <= y;
}
bool IntGreater::lessOrEqual(int x, int y){
    returnx x >= y;
}
bool Divisible::lessOrEqual(int x, int y){
    return y%x == 0;
}
bool Lexicographic::lessOrEqual(std::string x, std::string y) {
    return x.compare(y) <= 0;
}

#endif //ORDERS_H
