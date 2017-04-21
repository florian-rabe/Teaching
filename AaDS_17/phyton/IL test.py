from Immutable_List import Cons, Nil


c = Cons(2,Cons(3,Cons(4,Cons(0, Nil()))))
d = Cons(7,Cons(5,Cons(10,Cons(0, Nil()))))
n = Nil()
iter_n = n.iterator()
iter_c = c.iterator()
print("printing Nil()")
while iter_n.hasNext():
    print(iter_n.Next(), end = " ")
print()
print("printing c usinf iterator")
while iter_c.hasNext():
    print(iter_c.Next(), end = " ")
print()
print("printing c")
c.print()
print("printing the reverse of the c")
c.reverse(Nil()).print()
print("printing the second el of the c")
el1 = c.getEl(2)
print(el1)

print("printing first el deleted c")
c.deletFirst()
iter_c.init()
while iter_c.hasNext():
    print(iter_c.Next(), end = " ")
print()
##print("printing d")
##d.print()
##print("combining c with d")
##c = c.concate(d)
##c.print()
print("printing c with iterator")
iter_c.init()
while iter_c.hasNext():
    print(iter_c.Next(), end = " ")
print()
print("deleting first elements of the c")
for i in range(2):
    c.deletFirst()
##    d.deletFirst()
iter_c.init()
while iter_c.hasNext():
    print(iter_c.Next(), end = " ")
print()
iter_c.init()
print("adding new element to c")
c = Cons(12,c)
iter_c = c.iterator()
while iter_c.hasNext():
    print(iter_c.Next(), end = " ")
print()



