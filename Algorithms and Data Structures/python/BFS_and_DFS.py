class Node:
	def __init__(self, key):
		self.data = key 
		self.left = None
		self.right = None


def DFS(root):
	current = root 
	s = []
	done = 0
	while(not done):
		if current is not None:
			s.append(current)
			current = current.left 
		else:
			if(len(s)>0 ):
				current = s.pop()
				print (current.data)
				current = current.right 
			else:
				done = 1
 
def BFS(root):
	h = getHeight(root)
	for i in range(1, h+1):
		printGivenLevel(root, i)
 
def printGivenLevel(root , level):
	if root is None:
		return
	if level == 1:
		print (root.data)
	elif level > 1 :
		printGivenLevel(root.left , level-1)
		printGivenLevel(root.right , level-1)

def getHeight(node):
	if node is None:
		return 0
	else :
		lheight = getHeight(node.left)
		rheight = getHeight(node.right)
 
		if lheight > rheight :
			return lheight+1
		else:
			return rheight+1
 
root = Node(1)
root.left = Node(2)
root.right = Node(3)
root.left.left = Node(4)
root.left.right = Node(5)

print("BFS:")
BFS(root)
print("DFS:")
DFS(root)