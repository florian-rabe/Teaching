"""
Because I programmed it in python, this might seem slightly different than the code 
in the assignment sheet. I wrote a function increment() to simulate the function
 f() in the assignment sheet. I did not use the function name "map()" as in the 
 assignment sheet becasue it is a reserved keyword in python.
"""

def mapper(x, f):
	arr = []
	return mapAux(x, f, arr)

def mapAux(x, f, result):
	if len(x) == 0:
		return result
	else:
		result.append(f(x))
	return mapAux(x[1:], f, result)

def increment(x):
	x[0] += 1
	return x[0]

A = [10, 15, 20, 25, 30]

print(mapper(A, increment))



