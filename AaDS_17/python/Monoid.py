class BinOp:
	def op(self, x, y):
		pass

class Monoid(BinOp):
	def e(self):
		pass

class Addition(Monoid):
	def op(self, x, y):
		return x+y

	def e(self):
		return 0

class Multiplication(Monoid):
	def op(self, x, y):
		return x*y

	def e(self):
		return 1

class Concetenation(Monoid):
	def op(self, x, y):
		return x+y

	def e(self):
		return ""

class Maximum(Monoid):
	def op(self, x, y):
		if x <= y:
			return y
		else:
			return x

	def e(self):
		return 0

class Matrix22:
	def __init__(self, a, b, c, d):
		self.values = [[a, b], [c , d]]

class Matrix22Multiplication(Monoid):
	def op(self, x, y):
		add = Addition()
		mult = Multiplication()
		result = [[None, None],[None, None]]
		for rows_x in range(0, 2):
			for columns_y in range(0, 2):
				total = add.e()
				for rows_y in range(0, 2):
					total = add.op(total, mult.op(x.values[rows_x][rows_y], y.values[rows_y][columns_y]))
				result[rows_x][columns_y] = total
		return Matrix22(result[0][0], result[0][1], result[1][0], result[1][1])

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

def sqmult(Matrix22, n):
	if n == 0:
		return Matrix22(1, 0, 0, 1)
	elif n < 0:
		return
	elif n == 1:
		return Matrix22
	else:
		if n % 2 == 0:
			return sqmult(Matrix22Multiplication().op(Matrix22, Matrix22), n/2)
		else:
			return Matrix22Multiplication().op(Matrix22, sqmult(Matrix22Multiplication().op(Matrix22, Matrix22), (n-1)/2))


m1 = Matrix22(1, 2, 3, 4)
m2 = Matrix22(2, 0, 1, 2)
print(Matrix22Multiplication().op(m1, m2).values)

m3 = Matrix22(2, 0, 1, 2)
m4 = Matrix22(1, 2, 3, 4)
print(Matrix22Multiplication().op(m3, m4).values)

print(Matrix22Multiplication().op(Matrix22(1, 1, 1, 0), Matrix22Multiplication().op(Matrix22(1, 1, 1, 0), Matrix22(1, 1, 1, 0))).values)
print(sqmult(Matrix22(1, 1, 1, 0), 4).values)
