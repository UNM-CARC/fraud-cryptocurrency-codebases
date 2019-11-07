#!/usr/bin/python

import sys, getopt
import argparse

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

def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("tree1")
    parser.add_argument("tree2")

    args = parser.parse_args()
    print(args.tree1)
    print(args.tree2)
    #inputfile = ''
    #try:
    #   opts, args = getopt.getopt(argv,"hi::",["ifile=","ofile="])
    #except:
    #    print('edit_dist.py ')

    with open('misc/simple.ast', 'r') as file1:
        tree1 = file1.read()
    with open('misc/simple2.ast', 'r') as file2:
        tree2 = file2.read()

    #x = len(data)

    #print(data)
    out = LCSubStr(tree1, tree2, len(tree1), len(tree2))
    print(len(tree1))
    print(len(tree2))
    print(out)

if __name__ == "__main__":
   main(sys.argv[1:])
