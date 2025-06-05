import random

def binary_search(A, n, x):
	p = 0;
	r = n
	while p <= r:
		q = int((p+r)/2)
		print(A[p:r+1])
		if A[q] == x:
			return q
		elif A[q] != x and A[q] > x:
			r = q - 1
		elif A[q] != x and A[q] < x:
			p = q + 1
	return -1

list10 = []
list25 = []
list50 = []
list100 = []

for i in range(10):
	list10.append(random.randrange(1, 100))
for i in range(25):
	list25.append(random.randrange(1, 100))
for i in range(50):
	list50.append(random.randrange(1, 100))
for i in range(100):
	list100.append(random.randrange(1, 100))

list10.append(58)
list25.append(58)
list50.append(58)
list100.append(58)

list10.sort()
list25.sort()
list50.sort()
list100.sort()

#print(list10)
print(binary_search(list25, len(list25), 58))