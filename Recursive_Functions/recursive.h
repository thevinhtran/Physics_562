/**
 *      Library of recursive functions
 **/ 

#include <iostream> 
#include <cmath> 

double factorial(int n){
    if (n == 1 || n == 0){
        return 1.0; 
    }
    else{
        return n*factorial(n-1);
    }
}

double Hermite(double x, int n){
    // calculates nth Hermite polynomial evaluated at x value x 

    if (n == 0){
        return 1.0;
    }
    else if (n == 1){
        return 2.0 * x; 
    }
    else{
        return 2*x*Hermite(x,n-1) - 2*(n-1)*Hermite(x, n-2); 
    }
}

double normalized_Hermite(double x, int n){
    return 1/sqrt(pow(2,n)*factorial(n))*1/pow(M_PI,1/4)*exp(-x*x/2.0)*Hermite(x,n);
}


double Chebyshev(double x, int n){
    // returns nth Chebyshev polynomial evaluated at x 
    if ( n == 0){
        return 1.0;
    }
    else if (n == 1){
        return x; 
    }
    else{
        return 2.0*x*Chebyshev(x,n-1) - Chebyshev(x,n-2); 
    }
}


double Legendre(double x, int n){
    if (n < 0){
        std::cout << "Error! n is less than 0" << std::endl; 
    }
    else if (n == 0){
        return 1.0; 
    }
    else{
        return 1/n*(2*(n-1) + 1)*x*Legendre(x, n-1) - 
            (n-1)*Legendre(x, n-2); 
    }
}