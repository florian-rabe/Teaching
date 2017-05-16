from math import *
from time import *

def naive_fibonacci(x):
	if x <= 1:
		return x
	else:
		return naive_fibonacci(x-1) + naive_fibonacci(x-2)

def linear_fibonacci(x):
	if x <= 1:
		return x
	else:
		previous = 0
		current = 1
		i = 1
		while i < x:
			next_val = current + previous
			previous = current
			current = next_val
			i += 1
		return current

def inexact_fibonacci(x):
	phi = ((1 + sqrt(5)) / 2)
	# Both of the lines below will print "float".
	# print(type(phi))
	# print(type(phi ** x / sqrt(5)))
	return round(phi ** x / sqrt(5))

print("Naive Fibonacci\n")

for x in range(1,10):
	print("Fibonacci(", x, "): ", naive_fibonacci(x), sep='')

print("\n---\n\nLinear Fibonacci\n")

for x in range(1,10):
	print("Fibonacci(", x, "): ", linear_fibonacci(x), sep='')

print("\n---\n\nInexact Fibonacci\n")

for x in range(1,10):
	print("Fibonacci(", x, "): ", inexact_fibonacci(x), sep='')

print("\n---\n\nFind the value for which the inexact algorithm returns an incorrect result:\n")

counter = 0

while linear_fibonacci(counter) == inexact_fibonacci(counter):
	counter += 1

print(counter)
print("\n---\n\n")

counter = 0

while True:
	t1 = perf_counter()
	#t1 = time()
	naive_fibonacci(counter)
	t2 = perf_counter()
	#t2 = time()
	print("For naive_fibonacci(", counter, "): ", round(t2-t1), " seconds", sep='')
	if (t2 - t1) > 10:
		counter -= 1
		break
	counter += 1

print("The largest number that can be computed under 10 seconds using naive_fibonacci: ", counter)
print("\n\n---\n\n")


"""

# I find it better to start counting from 1 million otherwise it takes too long.
# The result should be somewhere between 1,000,000 and 1,000,100
counter = 1000000

while True:
	t1 = perf_counter()
	#t1 = time()
	linear_fibonacci(counter)
	t2 = perf_counter()
	#t2 = time()
	#print("For linear_fibonacci(", counter, "): ", round(t2-t1), " seconds", sep='')
	if (t2 - t1) > 10:
		counter -= 1
		break
	counter += 1


print("The largest number that can be computed under 10 seconds using linear_fibonacci: ", counter)

"""