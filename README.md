# fraud-cryptocurrency-codebases

This repository contains source code and data associated with a project to analyzeL cryptocurrency code bases and determine potential source code sharing between cryptocurrency implementations. In particular, it includes:
  * Code to identify cryptocurrency ipementations from coinmarketcap
  * Code to download and scrape identified cryptocurrencies from GitHub
  * Code to analyze code similarity between cryptocurrentcy repositories using
    three different methods:
    * Hash-based source code comparison
    * Hash-based source code comparison after white space removal
    * Longest common substring of Clang source code parse trees
  * Data resulting from an all-pairs comparision of approximately 60 different 
    using these analysis methods

Source code downloads and scraping is driven from the file Main.hs in the app directory,
and is generally run using the Haskell 'stack' project management tool with dependencies
managed from the file stack.yaml. Different portions of Main.hs can be uncommented 
depending on the analysis to be performed.

Data from the parse tree analysis resides in the data/ directory as a CSV file for each pair
of comparisons. Each line of the file is a comparison of two source files, one from each cryptocurrency repository, followed by the length of the parse tree in each source file and, finally, the length of the longest common substring between the two parse trees.
