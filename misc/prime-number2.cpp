#include <iostream>

using namespace std;

bool prime(int n) {
  int i;
  bool prime = true;
  for(i = 2; i <= n / 2; ++i)
  {
    if(n % i == 0)
    {
      prime = false;
      break;
    }
  }
  return prime;
}

int main() {
    int n;
    bool isPrime = true;
    cout << "Positive integer please: ";
    cin >> n;
    isPrime = prime(n); 
    if (isPrime)
        cout << "Prime number";
    else
        cout << "Not a prime number";
    return 0;
}
