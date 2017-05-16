import random

def insertion_sort(A, n):
	for i in range(1, n):
		print(A)
		key = A[i]
		j = i - 1
		while j >= 0 and A[j] > key:
			A[j + 1] = A[j]
			j = j - 1
		A[j + 1] = key
	return A


list10 = []
for i in range(10):
	list10.append(random.randrange(1, 100))

# print(list10)
# index_smallest = find_smallest(list10, len(list10), 0)
# print("list10[", index_smallest, "] = ", list10[index_smallest], sep='')
list10 = insertion_sort(list10, len(list10))
print("\nThe last print\n")
print(list10)