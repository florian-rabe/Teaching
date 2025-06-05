#ifndef HASH_SET_H
#define HASH_SET_H
#include <iostream>
#include "list_set.h"

//Class for iterator for hash sets
template<class T>
class HashSetIterator : public virtual setIterator<T>{
    private:
        //array of list sets from hash set
        ListSet<T> **bucket;
        //current list set iterator
        ListSetIterator<T> *iter;
        int m;
        int currBucket;
    public:
        //Methods
        HashSetIterator(ListSet<T>**, int);
        ~HashSetIterator(){delete this->iter;};
        bool hasNext();
        T getNext();
};
//Hash Set class
template<class T>
class HashSet : public virtual Set<T>{
    protected:
        //array of list sets
        ListSet<T> **bucket;
        int m;
    public:
        //Methods
        HashSet(int);
        ~HashSet();
        virtual int hash(T) = 0;
        bool contains(T);
        Unit insert(T);
        Unit Delete(T);
        HashSetIterator<T>* iterator();
};
//Hash Set with last digit hash
class LastDigitHashSet : public HashSet<int>{
    public:
        //Methods (all O(1))
        LastDigitHashSet() : HashSet<int>(10){};
        ~LastDigitHashSet(){};
        int hash(int x){return x%10;};
};
//Constructor for Hash Set Iterators
//Complexity O(1)
template<class T> HashSetIterator<T>::HashSetIterator(ListSet<T> **buck, int x){
    this->bucket = buck;
    this->m = x;
    this->currBucket = 0;
    this->iter = new ListSetIterator<T>(this->bucket[this->currBucket]->getElements());
}
//hasNext for Hash Set Iterators
//Worst-case complexity Theta(m)
template<class T> bool HashSetIterator<T>::hasNext(){
    //if the list set iterator has next, return true
    if(this->iter->hasNext()){
        return true;
    }
    else{
        //if not, change bucket until it has one, and return false if we run out of buckets
        if(this->currBucket+1 < this->m){
            delete this->iter;
            this->iter = new ListSetIterator<T>(this->bucket[this->currBucket+1]->getElements());
            this->currBucket = this->currBucket + 1;
            return this->hasNext();
        }
        else{
            return false;
        }
    }
}
//getNext for Hash Set Iterators
//Worst-case complexity Theta(m)
template<class T> T HashSetIterator<T>::getNext(){//called based on hasNext being true
    //if the list set iterator has next, return that next
    if(this->iter->hasNext()){
        return this->iter->getNext();
    }
    else{
        //if not, change the bucket until it has one
        while(!this->iter->hasNext()){
            delete this->iter;
            this->iter = new ListSetIterator<T>(this->bucket[this->currBucket]->getElements());
            this->currBucket++;
        }
        return this->iter->getNext();//return it
    }
}
//Constructor for HashSet
//Complexity Theta(x)
template<class T> HashSet<T>::HashSet(int x){
    this->m = x;
    this->bucket = new ListSet<T>*[x];
    int i;
    //Allocates memory for all buckets
    for(i = 0; i < x; i++){
        this->bucket[i] = new ListSet<T>;
    }
}
//Destructor for HashSet
//Complexity Theta(m)
template<class T> HashSet<T>::~HashSet(){
    int i;
    //frees memory of all list sets, bucket per bucket
    for(i = 0; i < m; i++){
        delete this->bucket[i];
    }
    delete[] this->bucket;
}
//Contains for HashSets
//Complexity Theta(n), where n is position within bucket
template<class T> bool HashSet<T>::contains(T el){
    return bucket[hash(el)]->contains(el);
}
//Insert for HashSets
//Complexity Theta(n), where n is size of bucket
template<class T> Unit HashSet<T>::insert(T el){
    return bucket[hash(el)]->insert(el);
}
//Delete for HashSets
//Complexity Theta(n), where n is position within bucket
template<class T> Unit HashSet<T>::Delete(T el){
    return bucket[hash(el)]->Delete(el);
}
//Returns iterator for HashSets
//Complexity O(1)
template<class T> HashSetIterator<T>* HashSet<T>::iterator(){
    HashSetIterator<T> *iter = new HashSetIterator<T>(this->bucket , this->m);
    return iter;
}
#endif // HASH_SET_H
