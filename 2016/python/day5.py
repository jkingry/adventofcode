import hashlib

def findSuffix(s):
    i = 0
    while True:
        candidate = s + str(i)
        hash_object = hashlib.md5(candidate.encode())
        if hash_object.hexdigest()[:5] == "00000":
            return str(i)
        i += 1

# Example usage
input_string = "example"
suffix = findSuffix(input_string)
result = hashlib.md5((input_string + suffix).encode()).hexdigest()
print(f"Input: {input_string}, Suffix: {suffix}, MD5: {result}")