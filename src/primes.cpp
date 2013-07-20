#include<iostream>
#include<bitset>

/*
 * Efficient implementation of the sieve of Eratosthenes using little space.
 */

#define N_MAX 1000000

using namespace std;

int main(int argc, char* argv[])
{
    bitset<N_MAX> is_prime;

	// initialize values to 1
	is_prime.flip();

    // 0 and 1 are not prime
	is_prime.set(0,0);
	is_prime.set(1,0);

	// start at 2 and knock out multiples thereof
	for(unsigned int i=2;i<N_MAX;++i)
	{
		if(is_prime[i])
		{
			for(unsigned int j=i*i;j<N_MAX;j+=i)
			{
				is_prime.set(j,0);
			}

            cout << i << '\t';
		}
	}
    cout << endl;

	return 0;
}
