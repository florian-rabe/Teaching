from math import sqrt

phi = (1+sqrt(5))/2

def fib(n):
    if n <= 1:
        return n
    p = 0
    c = 1
    i = 1
    while i < n:
        x = p+c
        p = c
        c = x
        i = i+1
    return c

def fibF(n):
    return round(phi**n/sqrt(5))

def fibExp(n):
    if n <= 1:
        return n
    else:
        return fibExp(n-1)+fibExp(n-2)

i = 0
while fib(i) == fibF(i):
    i = i+1

print(i,fib(i),fibF(i))

print(fibExp(i))
