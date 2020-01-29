#include <iostream>
using namespace std;

int main()
{
    /* variable definition and initialization */   
    int n, i, c = 0;
    
    /* Get user input */    cout << "Enter any number n: "; cin>>n;
    
    /*logic*/    for (i = 1; i <= n; i++)
    {
        if (n % i == 0)
        {
           c++;
        }
    }
    if (c == 2)
    {
       cout << "n is a Prime number" << endl;
    }
    else
    {
         cout << "n is not a Prime number" << endl; 
    }
    return 0;    
}
