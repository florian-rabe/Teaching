#include <iostream>

#ifndef SORT_H
#define SORT_H

/*
    Algorithms are not ideal, since they work directly on the list.
    It is better to first convert to array.
*/

//Swap two elements in an array
template<typename T> void swapThem(T *arr, i, j){
    T h = arr[i];
    arr[i] = arr[j];
    arr[j] = h;
}
//BubbleSort Algorithm
template<typename T> void BubbleSort(MyList<T> *lst, Ord<T> *ord){
    int n = lst->length();
    if(n < 2){
        return;
    }
    T *arr;
    arr = lst->toArray();
    bool sorted = false;
    //Repeats until it is fully sorted
    while(!sorted){
        sorted = true;
        int i;
        //Go through list
        for(i = 0; i < (n-1); i++){
            //Swap elements if they are out of place
            if(!(ord->lessOrEqual(arr[i],arr[i+1]))){
                sorted = false;
                swapThem(arr, i, i+1);
            }
        }
    }
    lst->copyArray(arr, n);
    delete(arr);
}

//Merge function
template<typename T> MyList<T>* Merge(MyList<T> *x, MyList<T> *y, Ord<T> *ord){
    MyList<T> *xRest;
    MyList<T> *yRest;
    xRest = new MyList<T>(*x);
    yRest = new MyList<T>(*y);
    MyList<T> *res;
    res = new MyList<T>();
    bool takeFromX;
    //Checking if memory was correctly allocated
    if(res == NULL || xRest == NULL || yRest == NULL){
        std::cout << "Error allocating memory" << std::endl;
        exit(-1);
    }
    //Repeat until all elements are taken
    while(!(xRest->isEmpty()) || !(yRest->isEmpty())){
        takeFromX = false;
        //Check if we should remove one from X or from Y lists
        if(yRest->isEmpty() && !xRest->isEmpty()){
            takeFromX = true;
        }
        if(!yRest->isEmpty() && !xRest->isEmpty()){
            if(ord->lessOrEqual(xRest->head->value,yRest->head->value)){
                takeFromX = true;
            }
        }
        //If head of xRest is smaller
        if(takeFromX){
            //Pre-appending of elements (for efficiency)
            res->preappend(xRest->head->value);
            xRest->delFirst();
        }
        else{
            res->preappend(yRest->head->value);
            yRest->delFirst();
        }
    }
    //Free memory for lists which we will not use again
    delete(xRest);
    delete(yRest);
    //Revert new list and return (for efficiency)
    res->reverse();
    return res;
}
//Primitive MergeSort Algorithm (returns new list - immutable)
template<typename T> MyList<T>* MergeSortBas(MyList<T> *x, Ord<T> *ord){
    //Get length of list to sort
    int n = x->length();
    //If it has 1 element or it is empty, return it (base case)
    if(n < 2){
        return x;
    }
    else{
        int k;
        k = (int)(n/2);
        MyList<T> *l;
        MyList<T> *r;
        MyList<T> *m;
        //Some kind of "Divide and conquer"
        l = MergeSortBas(x->sublist(0,k-1), ord);
        r = MergeSortBas(x->sublist(k,n-1), ord);
        //Merge together new lists
        m = Merge(l,r, ord);
        delete(l);
        delete(r);
        //Return merged list
        return m;
    }
}
//Implementing MergeSort Algorithm with mutable lists
template<typename T> void MergeSort(MyList<T> *x, Ord<T> *ord){
    MyList<T> *y;
    //Sets to y the sorted list x
    y = MergeSortBas(x, ord);
    //Empties x and copies elements of y into it
    x->emptyList();
    x->copyList(*y);
    //Empties y and frees its memory
    y->emptyList();
    delete(y);
}

#endif //SORT_H
