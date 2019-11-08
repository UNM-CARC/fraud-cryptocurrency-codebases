#!/usr/bin/python

import sys, getopt
import argparse

def iter_substring_match(s, t, m, n):
    C, s, t = {}," " + s, " " + t
    for j in range(n):
        C[0, j] = 0
    for i in range(1, m):
        C[i, 0] = i
    for i in range(1, m):
        for j in range(1, n):
            if s[i] == t[j]: c_match = C[i-1, j-1]
            else: c_match = C[i-1, j-1] + 1
            c_ins = C[i, j-1] + 1
            c_del = C[i-1, j] + 1
            c_min = min(c_match, c_ins, c_del)
            C[i, j] = c_min
    finj = min([(C[i, k], k) for k in range(1, n - 1)])
    return finj

""" https://www.geeksforgeeks.org/longest-common-substring-dp-29/ """
def LCSubStr(X, Y, m, n): 
      
    # Create a table to store lengths of 
    # longest common suffixes of substrings.  
    # Note that LCSuff[i][j] contains the  
    # length of longest common suffix of  
    # X[0...i-1] and Y[0...j-1]. The first 
    # row and first column entries have no 
    # logical meaning, they are used only 
    # for simplicity of the program. 
      
    # LCSuff is the table with zero  
    # value initially in each cell 
    LCSuff = [[0 for k in range(n+1)] for l in range(m+1)] 
      
    # To store the length of  
    # longest common substring 
    result = 0 
  
    # Following steps to build 
    # LCSuff[m+1][n+1] in bottom up fashion 
    for i in range(m + 1): 
        for j in range(n + 1): 
            if (i == 0 or j == 0): 
                LCSuff[i][j] = 0
            elif (X[i-1] == Y[j-1]): 
                LCSuff[i][j] = LCSuff[i-1][j-1] + 1
                result = max(result, LCSuff[i][j]) 
            else: 
                LCSuff[i][j] = 0
    return result 

def substrings(tree, l):
    n = 80
    return [tree[i:i+n] for i in range(0, l, n)]


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("tree1")
    parser.add_argument("tree2")

    args = parser.parse_args()
    out = 0
    print(args.tree1)
    print(args.tree2)

    with open(args.tree1, 'r') as file1:
        tree1 = file1.read()
    with open(args.tree2, 'r') as file2:
        tree2 = file2.read()

    #print(data)

    subs = substrings(tree1, len(tree1))
    for i in range(0, len(subs)):
        #out = iter_substring_match(subs[i], tree2, len(tree1), len(tree2))
        out += LCSubStr(subs[i], tree2, len(subs[i]), len(tree2))
    print(len(tree1))
    print(len(tree2))
    print(out)

#    for i in range(0, len(subs)):
#        print(subs[i])

if __name__ == "__main__":
   main(sys.argv[1:])
