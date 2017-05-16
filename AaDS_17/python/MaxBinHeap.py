class MaxBinHeap:
	def __init__(self):
		# Python lists are implemented as arrays.
		self.heapList = []
		self.currentSize = 0

	def parent(self, i):
		return (i-1)//2

	def left(self, i):
		return 2*i+1

	def right(self, i):
		return 2*i+2

	def minChild(self, i):
		if self.right(i) > self.currentSize-1:
			return self.left(i)
		else:
			if self.heapList[self.left(i)] <= self.heapList[self.right(i)]:
				return self.left(i)
			else:
				return self.right(i)

	def maxChild(self, i):
		if self.right(i) > self.currentSize-1:
			return self.left(i)
		else:
			if self.heapList[self.left(i)] > self.heapList[self.right(i)]:
				return self.left(i)
			else:
				return self.right(i)

	def insert(self, k):
		self.heapList.append(k)
		self.currentSize = self.currentSize + 1
		if self.currentSize > 1:
			self.percUp(self.currentSize)

	def extract(self):
		if self.currentSize == 0:
			raise ValueError("Nothing to extract")
		else:
			temp = self.heapList[0]
			self.heapList[0] = self.heapList[self.currentSize-1]
			self.currentSize = self.currentSize - 1
			self.heapList.pop()
			self.percDown(0)
			return temp

	def find(self):
		return self.heapList[0]

	def percUp(self, i):
		# Filters items upwards
		self.current = i-1
		while self.parent(self.current) >= 0:
			if self.heapList[self.current] > self.heapList[self.parent(self.current)]:
				tmp = self.heapList[self.parent(self.current)]
				self.heapList[self.parent(self.current)] = self.heapList[self.current]
				self.heapList[self.current] = tmp
			self.current = self.parent(self.current)

	def percDown(self, i):
		# Filters items downwards
		while 2*i+1 <= self.currentSize-1:
			mc = self.maxChild(i)
			if self.heapList[i] < self.heapList[mc]:
				tmp = self.heapList[i]
				self.heapList[i] = self.heapList[mc]
				self.heapList[mc] = tmp
			i = mc

	def print(self):
		print(self.heapList)



bh = MaxBinHeap()
bh.insert(33)
bh.print()
bh.insert(17)
bh.print()
bh.insert(27)
bh.print()
bh.insert(14)
bh.print()
bh.insert(11)
bh.print()
bh.insert(18)
bh.print()
bh.insert(19)
bh.print()
bh.insert(21)
bh.print()
bh.insert(9)
bh.print()
bh.insert(5)
bh.print()

bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()
bh.extract()
bh.print()