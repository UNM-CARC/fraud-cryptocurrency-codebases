def iter_string_compare(s, t):
    C, s, t = {}," " + s, " " + t
    for j in range(len(t)):
        C[0, j] = j
    for i in range(1, len(s)):
        C[i, 0] = i
    for i in range(1, len(s)):
        for j in range(1, len(t)):
            if s[i] == t[j]: c_match = C[i-1, j-1]
            else: c_match = C[i-1, j-1] + 1
            c_ins = C[i, j-1] + 1
            c_del = C[i-1, j] + 1
            c_min = min(c_match, c_ins, c_del)
            C[i, j] = c_min
    return C[i, j]

with open('code_analysis/data/prime-number.ast', 'r') as file:
    data = file.read()
x = len(data)

#print(data)
out = iter_string_compare(data, data)
print(out)
