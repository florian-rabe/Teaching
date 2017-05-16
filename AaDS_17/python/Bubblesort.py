import random

def randomList(size, low, up):
	l = []
	for i in range(size):
		l.append(random.randrange(low, up))
	print("Before sorting:")
	print(l)
	bubblesort2(l)
	print("After sorting:")
	print(l)

def bubblesort(A):
	for i in range(len(A)-2):
		for j in range(len(A)-1, i, -1):
			if A[j] < A[j-1]:
				temp = A[j]
				A[j] = A[j-1]
				A[j-1] = temp
			# print(A)
	#return A

def bubblesort2(A):
	isSorted = False
	while not isSorted:
		isSorted = True
		for i in range(len(A)-1):
			if not (A[i] <= A[i+1]):
				isSorted = False
				temp = A[i]
				A[i] = A[i+1]
				A[i+1] = temp

for i in range(10):
	randomList(10, 1, 100)



# for i in range(len(list10), 0, -1):
# 	print(i)