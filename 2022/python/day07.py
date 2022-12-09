class Node():
    def __init__(self, n, c, sz):
        self.n = n
        self.c = c
        self.sz = sz

root = Node(
    "/", [
            Node("a", [Node("e", [], 584)], 29116 + 2557 + 62596),
            Node("d", [], 4060174 + 8033020 + 5626152 + 7214296)
    ], 14848514 + 8504156)


lessThen10K = 0
def getSize(node):
    global lessThen10K
    
    csz = sum([getSize(c) for c in node.c]) 
    dirsz = node.sz + csz
 
    if dirsz < 100000:
        lessThen10K += dirsz
    
    return dirsz

getSize(root)
print(lessThen10K)