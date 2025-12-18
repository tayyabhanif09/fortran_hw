program main
    use types
    use sort_numbers
    implicit none


    real(kind=rkind), dimension(:), allocatable :: vinput, voutput, vin_c
    logical :: t

    !allocates a test vector, fills with a mix of positive/negative/large values
    vinput = [2.0_rkind, -3.0_rkind, 1000.0_rkind, 675.0_rkind, -68000.0_rkind]
    vin_c = vinput



    t = checkinputs(vinput,voutput)


!calls the non-destructive descending sort (sorthighlow) and prints the result
    call sorthighlow(vinput,voutput,t)



! verifies the input vector remained unchanged

   call checkinputvec(vinput,vin_c,voutput)





!calls the non-destructive ascending sort (sortlowhigh) and prints the result
    call sortlowhigh(vinput,voutput,t)


!calls the destructive ascending sort (sortlowhigh) and prints the result
    call sortlowhigh_inplace(vinput)

!calls the destructive descending sort (sorthighlow) and prints the result
    call sorthighlow_inplace(vinput)



! calls the permutation sort and prints the result
    call permut(vinput)





end program main
