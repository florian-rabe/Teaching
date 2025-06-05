def sqmult(x, n):
	if n < 0:
		return sqmult(1/x, -n)
	elif n == 0:
		return 1
	elif n == 1:
		return x
	else:
		if n % 2 == 0:
			return sqmult(x * x, n/2)
		else:
			return x * sqmult(x * x, (n-1)/2)

def sqmult_v2(x, n):
	if n == 0:
		return 1
	elif n < 0:
		return sqmult_v2(1/x, -n)
	else:
		r = sqmult_v2(x, int(n/2))
		sq = r * r
		if n%2 == 0:
			return sq
		else:
			return x*sq

def power(x, n):
	if n < 0:
		return power(1/x, (-n)-1)
	elif n == 0:
		return 1
	else:
		return x * power(x, n-1)

print(sqmult(3, -2))
print(sqmult_v2(4, 3))
print(sqmult_v2(4, -3))
print(power(4, 3))