import random

def printAllKLengthRec(lst, prefix, n, k, container):
	if k == 0:
		container.append(prefix)
		return
	for i in range(0, n):
		newprefix = prefix + lst[i]
		printAllKLengthRec(lst, newprefix, n, k-1, container)

def printAllKLength(lst, k, container):
	n = len(lst)
	printAllKLengthRec(lst, "", n, k, container)

def generate_password(n):
	passwd = ""
	for i in range(0, n):
		passwd += chr(random.randrange(33, 128))
	return passwd

def crack_password(password, container):
	for element in container:
		if element == password:
			return element

def main():
	possible_characters = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
	l1 = list(possible_characters)
	l2 = list()
	printAllKLength(l1, 3, l2)
	passwd = generate_password(3)
	print(passwd)
	print(crack_password("ABC", l2))
	print(crack_password(passwd, l2))

	# print("\nFirst test\n")
	# l1 = list("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
	# l2 = list()
	# k = 3;
	# printAllKLength(l1, k, l2)
	# #print(l2)

	# print("\nSecond test\n")
	# l3 = list("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
	# l4 = list()
	# k = 1
	# printAllKLength(l3, k, l4)
	# #print(l4)
	# print("\n")

if __name__ == "__main__":
	main()