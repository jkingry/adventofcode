import numpy as np
# input data
ins = [[-618, -824, -621], [-537, -823, -458],
       [-447, -329, 318], [404, -588, -901]]  # <- points
out = [[686, 422, 578], [605, 423, 415], [
    515, 917, -361], [-336, 658, 858]]  # <- mapped to
# calculations
l = len(ins)
B = np.vstack([np.transpose(ins), np.ones(l)])
print ("B = ", B)
D = 1.0 / np.linalg.det(B)
print ("D = ", D)


def entry(r, d): return np.linalg.det(
    np.delete(np.vstack([r, B]), (d+1), axis=0))


M = [[(-1)**i * D * entry(R, i) for i in range(l)] for R in np.transpose(out)]
A, t = np.hsplit(np.array(M), [l-1])
print(t)
t = np.transpose(t)[0]
# output
print(l-1)
print("WTF matrix:\n", np.array(M))
print("Affine transformation matrix:\n", A)
print("Affine transformation translation vector:\n", t)
# unittests
print("TESTING:")
for p, P in zip(np.array(ins), np.array(out)):
  image_p = np.dot(A, p) + t
  result = "[OK]" if np.allclose(image_p, P) else "[ERROR]"
  print(p, " mapped to: ", image_p, " ; expected: ", P, result)

p = np.array([544, -627, -890])
P = np.array([-476, 619, 847])
image_p=np.dot(A, p) + t
result="[OK]" if np.allclose(image_p, P) else "[ERROR]"
print(p, " mapped to: ", image_p, " ; expected: ", P, result)

p = np.array([544, -627, -890, 1])
P = np.array([-476, 619, 847, 1])
image_p2 = np.dot(np.array(M), p)
print(p, " mapped to: ", image_p2, " ; expected: ", P, "??")

q = np.vstack([np.transpose(ins), np.ones(l)])
print ("q = ", q)
q_prime = np.vstack([np.transpose(out), np.ones(l)])
print ("q_prime = ", q_prime)

trans = np.dot(np.linalg.inv(q), q_prime)
print("trans = ", trans)

print(np.dot(q, trans))
qv = np.array([544, -627, -890, 1])
print(np.dot(qv, trans))
