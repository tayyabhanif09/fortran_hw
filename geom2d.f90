module geom2d
    use types !calling file types here
    implicit none
    private ! no private variable yet
    public :: carea, sarea, rectap ! the variables available globally

    contains ! what does this mean

    real(kind=rkind) function carea(r) result(a)
    !function takes the radius(r) and returns area(a)
        real(kind=rkind), intent(in) :: r
        real(kind=rkind) :: pi
        pi = 4.0_rkind*  atan(1.0_rkind)
        a = pi * r ** 2
    end function carea

    real(kind=rkind) function sarea(s) result(as)
        real(kind=rkind), intent(in) :: s
        as = s**2
    end function sarea


    ! when output is more than 1 variable use subroutine
    subroutine rectap(ar,br,A,P)
        real(kind=rkind), intent(in) :: ar,br
        real(kind=rkind), intent(out) :: A,P
        A = ar*br
        P = 2.0_rkind * (ar+br)
    end subroutine rectap


end module geom2d
