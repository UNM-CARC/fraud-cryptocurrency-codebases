// A school method based C++ program to check if a 
// number is prime 
#include <bits/stdc++.h> 
//#include <iostream>
using namespace std; 
  
bool isPrime(int n) 
{ 
    // Corner case 
    if (n <= 1) 
        return false; 
  
    // Check from 2 to n-1 
    for (int i = 2; i < n; i++) 
        if (n % i == 0) 
            return false; 
  
    return true; 
} 
  
// Driver Program to test above function 
int main() 
{ 
    int n;
    //bool isPrime = true;
    cout << "Positive integer please: ";
    cin >> n;
    if (isPrime(n))
        cout << "Prime number";
    else
        cout << "Not a prime number";
    return 0;
    //isPrime(11) ? cout << " true\n" : cout << " false\n"; 
    //isPrime(15) ? cout << " true\n" : cout << " false\n"; 
    //return 0; 

} 
