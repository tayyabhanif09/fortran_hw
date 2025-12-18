program geomath
    use types
    use geom2d
    use mathfun

    implicit none
    real(kind=rkind) :: a, b, area_circle, addition, area_square




    print*, "input a = "
    read(*,*) a
    print*, "input b = "
    read(*,*) b



    ! summ of real numbers
    addition = add(a,b)
    print*, "addition of a and b = ", addition


    ! swapping two variables and numbers
    call swap(a,b)
    print*, "swaped a and b: ", a,b


    ! area of circle

    area_circle = carea(a)
    print*, "area of circle is: ", area_circle


    ! area of square

    area_square = sarea(b)
    print*, "area of square: ", area_square







end program geomath

