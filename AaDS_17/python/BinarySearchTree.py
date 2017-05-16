class TreeNode(object):
	def __init__(self, data, left=None, right=None, parent=None):
		self.data = data
		self.leftChild = left
		self.rightChild = right
		self.parent = parent

	def hasLeftChild(self):
		return self.leftChild != None

	def hasRightChild(self):
		return self.rightChild != None

	def isLeftChild(self):
		return self.parent != None and self.parent.leftChild == self

	def isRightChild(self):
		return self.parent != None and self.parent.rightChild == self

	def isRoot(self):
		return self.parent == None

	def isLeaf(self):
		return self.rightChild == None or self.leftChild == None

	def hasAnyChildren(self):
		return self.rightChild != None or self.leftChild != None

	def hasBothChildren(self):
		return self.rightChild != None and self.leftChild != None

	def replaceNodeData(self, key, lc, rc):
		self.key = key
		self.leftChild = lc
		self.rightChild = rc
		if self.hasLeftChild():
			self.leftChild.parent = self
		if self.hasRightChild():
			self.rightChild.parent = self


class BinarySearchTree(TreeNode):

	def __init__(self, newroot, rc=None, lc=None, parent=None):
		super().__init__(newroot)

	def insert(self, data):
		newnode = BinarySearchTree(data)
		if self.data > newnode.data:
			if self.leftChild is None:
				newnode.parent = self
				self.leftChild = newnode
				# print(self.data)
			else:
				self.leftChild.insert(newnode.data)
		else:
			if self.rightChild is None:
				newnode.parent = self
				self.rightChild = newnode
			else:
				self.rightChild.insert(newnode.data)

	def contains(self, value):
		current = self 
		s = []
		while True:
			if current is not None:
				s.append(current)
				if current.data == value:
					return True
				current = current.leftChild 
			else:
				if(len(s)>0):
					current = s.pop()
					current = current.rightChild 
				else:
					return False

	def findMinNode(self, Tree):
		if Tree.hasLeftChild() and Tree.hasRightChild():
			Tree.findMinNode(Tree.leftChild)
		elif Tree.hasLeftChild() and not Tree.hasRightChild():
			Tree.findMinNode(Tree.leftChild)
		elif not Tree.hasLeftChild() and Tree.hasRightChild():
			print("return type for findMinNode", type(Tree))
			return Tree
		else:
			print("return type for findMinNode", type(Tree))
			return Tree

	def findMaxNode(self, Tree):
		 if Tree.hasLeftChild() and Tree.hasRightChild():
		 	Tree.findMaxNode(Tree.rightChild)
		 elif Tree.hasLeftChild() and not Tree.hasRightChild():
		 	return Tree
		 elif not Tree.hasLeftChild() and Tree.hasRightChild():
		 	Tree.findMaxNode(Tree.rightChild)
		 else:
		 	return Tree

	def delete(self, value):
		current = self 
		s = []
		done = 0
		while not done:
			if current is not None:
				s.append(current)
				if current.data == value:
					if current.isRoot():
						if not current.hasAnyChildren():
							raise ValueError("You cannot delete the root which has no children!")
						else:
							if current.hasLeftChild() and not current.hasRightChild():
								self.data = current.leftChild.data
								current.leftChild.parent = None
								current.leftChild = None
								self.leftChild = None
							elif not current.hasLeftChild() and current.hasRightChild():
								self.data = current.rightChild.data
								current.rightChild.parent = None
								current.rightChild = None
								self.rightChild = None
							else:
								self.data = current.leftChild.data
								current.leftChild.parent = None
								current.leftChild = None
								self.leftChild = None
					else:
						if current.hasLeftChild() and current.hasRightChild():
							temp = current
							temp.findMinNode(temp.rightChild)
							current.leftChild.parent = temp
							temp.leftChild = current.leftChild
							if current.isLeftChild():
								current.parent.leftChild = current.rightChild
								current.rightChild.parent = current.parent
							else:
								current.parent.rightChild = current.rightChild
								current.rightChild.parent = current.parent
						elif current.hasLeftChild() and not current.hasRightChild():
							if current.isLeftChild():
								current.parent.leftChild = current.leftChild
								current.leftChild.parent = current.parent
							else:
								current.parent.rightChild = current.leftChild
								current.leftChild.parent = current.parent
						elif not current.hasLeftChild() and current.hasRightChild():
							if current.isLeftChild():
								current.parent.leftChild = current.rightChild
								current.rightChild.parent = current.parent
							else:
								current.parent.rightChild = current.rightChild
								current.rightChild.parent = current.parent
						else:
							if current.isLeftChild():
								current.parent.leftChild = None
							else:
								current.parent.rightChild = None
					done = 1
				else:
					current = current.leftChild 
			else:
				if(len(s)>0):
					current = s.pop()
					current = current.rightChild 
				else:
					done = 1

	def DFS(self):
		current = self 
		s = []
		done = 0
		while(not done):
			if current is not None:
				s.append(current)
				current = current.leftChild 
			else:
				if(len(s)>0):
					current = s.pop()
					print(current.data)
					current = current.rightChild 
				else:
					done = 1

	def __iter__(self):
		self.nextHelper()
		self.counter = 0
		return self

	def __next__(self):
		if self.counter == len(self.iterHelp):
			raise StopIteration
		else:
			self.counter += 1
			return self.iterHelp[self.counter-1]

	def nextHelper(self):
		current = self 
		self.iterHelp = []
		s = []
		done = 0
		while(not done):
			if current is not None:
				s.append(current)
				current = current.leftChild 
			else:
				if(len(s)>0):
					current = s.pop()
					self.iterHelp.append(current)
					current = current.rightChild 
				else:
					done = 1

"""
							100
						   /   \
						  /     \
						 /       \
						/         \
					   /           \
					  /             \
					 /               \
					50               200
				   /  \             /   \
			      /    \           /     \
			    25      75       150     300
		       /  \    /  \      / \    /   \
		     10   35  60  90   130 170 250  400

"""

mytree = BinarySearchTree(100)
mytree.insert(50)
mytree.insert(200)
mytree.insert(25)
mytree.insert(75)
mytree.insert(150)
mytree.insert(300)
mytree.insert(10)
mytree.insert(35)
mytree.insert(60)
mytree.insert(90)
mytree.insert(130)
mytree.insert(170)
mytree.insert(250)
mytree.insert(400)

print()
print("Root:", mytree.data)
print("Root's left child:", mytree.leftChild.data)
print("Root's right child:", mytree.rightChild.data)
print("Root's left child's parent:", mytree.leftChild.parent.data)
print("Root's left child's left child's:", mytree.leftChild.leftChild.data)
print("Root's left child's left child's parent:", mytree.leftChild.leftChild.parent.data)
print("Root's right child's right child:", mytree.rightChild.rightChild.data)
print("Root's right child' parent:", mytree.rightChild.parent.data)
print("Root's right child's right child's parent:", mytree.rightChild.rightChild.parent.data)
print("Root's right child's right child's right child:", mytree.rightChild.rightChild.rightChild.data)
print("Root's right child's right child's left child:", mytree.rightChild.rightChild.leftChild.data)
print("Root's right child's right child's left child's parent:", mytree.rightChild.rightChild.leftChild.parent.data)
print("Root has left child's left child's left child:", mytree.leftChild.leftChild.leftChild.hasLeftChild())
print()


print("Printing via depth-first traversal")
mytree.DFS()
print()

print("Printing with iterator")
for node in mytree:
	print(node.data)

print()

print("Printing with iterator")
for node in mytree:
	print(node.data)

print()

print("Printing with iterator")
for node in mytree:
	print(node.data)

print()
mytree.delete(200)
print()
print("Printing via DFS: ")
mytree.DFS()

print()
print(mytree.rightChild.data)

print("mytree.contains(200) is", mytree.contains(200))
print("mytree.contains(300) is", mytree.contains(300))
print("mytree.contains(100) is", mytree.contains(100))
print("mytree.contains(10) is", mytree.contains(10))
print("mytree.contains(90) is", mytree.contains(90))
print("mytree.contains(400) is", mytree.contains(400))

mytree.delete(130)
mytree.delete(170)
mytree.delete(10)
mytree.delete(35)
mytree.delete(60)
mytree.delete(90)
mytree.delete(150)
mytree.delete(25)
mytree.delete(75)
mytree.delete(250)
mytree.delete(400)

print()
mytree.DFS()
print()
print("Root:", mytree.data)
print("Root's left child:", mytree.leftChild.data)
print("Root's left child's parent:", mytree.leftChild.parent.data)
print("Root's right child:", mytree.rightChild.data)
print("Root's right child's parent:", mytree.rightChild.parent.data)
print("mytree.hasAnyChildren()", mytree.hasAnyChildren())
print("mytree.hasBothChildren()", mytree.hasBothChildren())
print("mytree.leftChild.hasAnyChildren()", mytree.leftChild.hasAnyChildren())
print("mytree.rightChild.hasAnyChildren()", mytree.rightChild.hasAnyChildren())
print()


mytree.delete(100)
mytree.DFS()
print()
print("Root:", mytree.data)
# print("Root's left child:", mytree.leftChild.data)
print("Root's right child:", mytree.rightChild.data)
print("mytree.hasAnyChildren()", mytree.hasAnyChildren())
print("mytree.hasBothChildren()", mytree.hasBothChildren())
# print("mytree.leftChild.hasAnyChildren()", mytree.leftChild.hasAnyChildren())
# print("Root's left child's right child:", mytree.leftChild.rightChild.data)
# print("mytree.rightChild.hasAnyChildren()", mytree.rightChild.hasAnyChildren())
print()

mytree.delete(50)
mytree.DFS()
print()
print("Root:", mytree.data)
# print("Root's left child:", mytree.leftChild.data)
# print("Root's right child:", mytree.rightChild.data)
print("mytree.hasAnyChildren()", mytree.hasAnyChildren())
print("mytree.hasBothChildren()", mytree.hasBothChildren())
# print("mytree.leftChild.hasAnyChildren()", mytree.leftChild.hasAnyChildren())
# print("Root's left child's right child:", mytree.leftChild.rightChild.data)
# print("mytree.rightChild.hasAnyChildren()", mytree.rightChild.hasAnyChildren())
print()