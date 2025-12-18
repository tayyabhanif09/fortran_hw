

module sort_numbers
    use types
    implicit none

!     private :: checkinputs
    public :: sorthighlow, sortlowhigh!, sorthighlow_inplace, sortlowhigh_inplace, permut
    contains


!     function mymaxloc(v) result(pos)
!         use types
!         integer(kind=rkind), dimension(1) :: position
!         real(kind=rkind), dimension(:) :: v, pos
!
!         position = maxloc(v)
!
!         pos = position(1)
!     end function mymaxloc

! verifies the input vector remained unchanged


    subroutine checkinputvec(vin, vin_copy, vout)
            real(kind=rkind), dimension(:), intent(in) :: vin, vin_copy
            real(kind=rkind), dimension(:), intent(in) :: vout

            if (all(vin == vin_copy)) then
                print *, "input vector unchanged"
            else
                print *, "input vector was modified"
            end if
    end subroutine checkinputvec


! verifies the input and output vectors are allocated


    logical function checkinputs(vin,vout) result(success)


        use types
        real(kind=rkind), dimension(:), allocatable, intent(in) :: vin
        real(kind=rkind), dimension(:), allocatable, intent(out) :: vout


        if (.not. allocated(vin)) then
            success = .false.
            RETURN
        end if

        if (.not. allocated(vout)) then
            allocate(vout(ubound(vin,1)))
        else
            if (ubound(vout,1) /= ubound(vin,1)) then
                deallocate(vout)
                allocate(vout(ubound(vin,1)))
            end if
        end if

        success = .true.

    end function checkinputs



    ! NON DESTRUCTIVE
! can assign unbound into a variable
! non-destructive descending sort (sorthighlow)

    subroutine sorthighlow(vin, vout, success)

        real(kind=rkind), dimension(:), allocatable, intent(in) :: vin
        real(kind=rkind), dimension(:), allocatable, intent(out) :: vout
        integer(kind=rkind), dimension(1) :: position
        integer(kind=rkind) :: i, tmp, fin
        logical :: success

        if (.not. checkinputs(vin,vout)) then
            ERROR STOP "incorrect inputs; exited at sort_numbers_lab:: sorthighlow"
        end if


        vout = vin

        fin = ubound(vout,1)


        do i = 1, fin
            position = maxloc(vout(i:)) + i - 1
            tmp = vout(position(1))
            vout(position(1)) = vout(i)
            vout(i) = tmp
        end do
        print*, "NON DESTRUCTIVE DESCENDING"
        print*, "vin before sort: ", vin
        print*, "vout after sort: ", vout




    end subroutine sorthighlow




! non-destructive ascending sort
    subroutine sortlowhigh(vin, vout, success)
        real(kind=rkind), dimension(:), intent(in), allocatable :: vin
        real(kind=rkind), dimension(:), allocatable, intent(out) :: vout
        integer(kind=rkind), dimension(1) :: position
        integer(kind=rkind) :: i, tmp, fin
        logical :: success

        if (.not. checkinputs(vin,vout)) then
            ERROR STOP "incorrect inputs; exited at sort_numbers_lab:: sorthighlow"
        end if


        vout = vin

        fin = ubound(vout,1)


        do i = 1, fin
            position = minloc(vout(i:)) + i - 1
            tmp = vout(position(1))
            vout(position(1)) = vout(i)
            vout(i) = tmp
        end do
        print*, "NON DESTRUCTIVE ASCENDING"
        print*, "vin before sort: ", vin
        print*, "vout after sort: ", vout







    end subroutine sortlowhigh




! destructive descending sort

    subroutine sortlowhigh_inplace(vin)

        real(kind=rkind), dimension(:), intent(inout) :: vin
        integer(kind=ikind), dimension(1) :: pos
        real(kind=rkind) :: tmp,fin,i
        print*, "DESTRUCTIVE SORT DESCENDING"
        print*, "vin before sort: ", vin

        fin = ubound(vin,1)

        do i = 1, fin
            pos = maxloc(vin(i:))
            tmp = vin(i)
            vin(i) = vin(i + pos(1) - 1)
            vin(i + pos(1) - 1) = tmp
        end do

        print*, "vin after sort: ", vin



    end subroutine sortlowhigh_inplace
!
!
! destructive ascending sort
!

    subroutine sorthighlow_inplace(vin)


        real(kind=rkind), dimension(:), intent(inout) :: vin
        integer(kind=ikind), dimension(1) :: pos
        real(kind=rkind) :: tmp,fin,i
        print*, "DESTRUCTIVE SORT ASCENDING"
        print*, "vin before sort: ", vin

        fin = ubound(vin,1)

        do i = 1, fin
            pos = minloc(vin(i:))
            tmp = vin(i)
            vin(i) = vin(i + pos(1) - 1)
            vin(i + pos(1) - 1) = tmp
        end do

        print*, "vin after sort: ", vin


    end subroutine sorthighlow_inplace
!
!



! permutation sort

    subroutine permut(vin)


            real(kind=rkind), dimension(:), intent(in) :: vin
            real(kind=rkind), dimension(:), allocatable :: p
            real(kind=rkind) :: tmp
            integer(kind=ikind) :: fin, i
            integer(kind=ikind), dimension(1) :: pos_arr !maxloc returns rank-1 array
            integer(kind=ikind) :: position  ! index

            fin = size(vin)
            allocate(p(fin))

            ! to avoid modifying input 'vin'
            p = vin

            do i = 1, fin - 1
                pos_arr = maxloc(p(i:fin))
                position = pos_arr(1) + i - 1

                ! Swap the current element (p(i)) with the max element found
                tmp = p(position)
                p(position) = p(i)
                p(i) = tmp
            end do

            print*, "PERMUTATION SORTING:  high to low"
            print*, "vin before sort: ", vin
            print*, "p   after sort: ", p

    end subroutine permut



end module sort_numbers


