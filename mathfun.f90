module mathfun
    use types
    implicit none

    contains

    function add(x,y) result(summ)
        real(kind=rkind), intent(in) :: x,y
        real(kind=rkind) :: summ
        summ = x+y
    end function add

    subroutine swap(c,d)
        real(kind=rkind), intent(inout) :: c,d
        real(kind=rkind) :: temp
        temp = c
        c = d
        d = temp
    end subroutine swap


end module mathfun
