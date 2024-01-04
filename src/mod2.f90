module step
    use mtmod

    contains 
        function take_step(x,y) result(next)
        implicit none 
        integer :: irn, irn_low=0,irn_high=3, next(2), x, y, a, b
        next = 0
        irn=igrnd(irn_low,irn_high) !random integer in the interval[0:3]
!------------------------------------------------------------------------
!       I use the random number generator provided in the instructions (mod2.f90), 
!       worth to mention the seed is the same every time UNLESS you change
!       it manually on row 136 in mod2.f90.
!------------------------------------------------------------------------

!-------------------------------------------------------------------------
!       The x and y are coordinates that are given by mod6, and this function 
!       decides on random the proposed next step that is then given as a result
!       back to mod 6 to be tested for validity and blocked-iness.
!------------------------------------------------------------------------- 
        
        
        !0: west
        !1: north
        !2: east
        !3: south
        
        
        if (irn == 0) then
            a = x-1
            b = y
        else if (irn == 1) then
            a = x
            b = y+1
        else if (irn == 2) then
            a = x+1
            b = y
        else if (irn == 3) then
            a = x
            b = y-1
        end if
        
        next(1)=a
        next(2)=b
        end function take_step

end module step
        
