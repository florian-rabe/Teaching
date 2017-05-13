from itertools import permutations

def board(vector):

	for column in vector:
		s = ['[ ]'] * len(vector)
		s[column] = '[Q]'
		print(''.join(s))
	print()

n = 8
columns = range(n)
for vector in permutations(columns):
	if n == len(set(vector[i]+i for i in columns)) == len(set(vector[i]-i for i in columns)):
		board(vector)