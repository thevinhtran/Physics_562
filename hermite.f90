! Program that calculates hermite polynomials 

function factorial(n) result(fact)
    ! calculates factorial of int n 
    implicit none

    integer:: n, i 
    real :: fact

    if (n == 0 .or. n == 1) then
        ! base case 
        fact = 1
        return 
    else    
        fact = 1
        do i = 1 , n
            fact = fact * i
        end do
    end if
end function factorial 


function coeff_2(n) result(sec_coeff)
    ! returns second coefficient of psi as a function of n
    implicit none 

    integer :: n
    real :: factorial 
    real :: sec_coeff 

    sec_coeff = 1/sqrt(2**n * factorial(n))
end function coeff_2 


function hermite(n, x) result(herme) 
    ! returns the nth hermite polynomial evaluated at given value for x
    implicit none 

    ! declare relevant vars
    integer, intent(in) :: n
    integer :: i 
    real, intent(in) :: x
    real, dimension(0:n) :: herm_array
    real :: herme 

    ! main 
    herm_array(0) = 1.0
    herm_array(1) = 2*x

    ! recursive relations 
    if (n == 0) then
        herme = herm_array(0)
        return 
    
    else if (n == 1) then
        herme = herm_array(1)
        return 

    else
        do i = 1, n-1
            herm_array(i+1) = 2 * x * herm_array(i) - 2 * i * herm_array(i-1)
        end do 
            herme = herm_array(n)
            return 
    end if 
end function hermite 


program main
    implicit none 

    integer :: n
    real :: u, psi, hermite, coeff_2 
    real, parameter :: du = 0.01, pi = 4*atan(1.0), coeff_1 = 1/(pi**(1.0/4.0))

    ! iterate over each indexed function
    do n = 0, 10 
        u = -7
        ! iterate over interval for u
        do while (u < 7)
            psi = coeff_1 * coeff_2(n) * hermite(n, u) * exp(-u**2/2)
            write(n + 10, *) u, psi 
            u = u + du 
        end do
    end do 
end program main 

