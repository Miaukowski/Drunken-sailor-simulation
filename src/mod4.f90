module blocked_path
    contains
    
        logical function path_is_blocked(current_pos, grid, steps) result (blocked)
        implicit none
        integer, intent(in) :: steps
        integer, intent(in) :: current_pos(2), grid(2*steps+3,2*steps+3)
        integer :: north(2), east(2), south(2), west(2), midpoint
        logical :: n_blocked, s_blocked, e_blocked, w_blocked
        north = 0
        east = 0
        south = 0
        west = 0        
        midpoint = steps+1


!-----------------------------------------------------------------------------
!	Here we simply define the current directional position and a proposed 
!	next step to said direction
!-----------------------------------------------------------------------------
        north(1) = current_pos(1)
        north(2) = current_pos(2)+1
        south(1) = current_pos(1)
        south(2) = current_pos(2)-1
        east(1) = current_pos(1)+1
        east(2) =  current_pos(2)
        west(1) = current_pos(1)-1
        west(2) = current_pos(2)
        
        blocked = .FALSE.
        
        n_blocked = .FALSE.
        s_blocked = .FALSE.
        e_blocked = .FALSE.
        w_blocked = .FALSE.
        
        
!------------------------------------------------------------------------------
!       the folllowing if statements checks individually if we have already 
!       visited a point, so if the values we are looking at in the arrray = 1, 
!       it's an illegal move. So the said boolean becomes true. 
!------------------------------------------------------------------------------
        if (grid(midpoint + north(1), midpoint + north(2)) == 1) then
        	n_blocked = .TRUE.
        end if
        if (grid(midpoint + south(1), midpoint + south(2)) == 1) then
        	s_blocked = .TRUE.
        end if
        if (grid(midpoint + east(1), midpoint + east(2)) == 1) then
        	e_blocked = .TRUE.
        end if
        if (grid(midpoint + west(1), midpoint + west(2)) == 1) then
        	w_blocked = .TRUE.
	end if
!------------------------------------------------------------------------------
!       This checks if the sailor is completely blocked in, so all the above 
!       statements are true, the blocked becomes true, and that is the returned 
!       value. If not blocked in (so false) it returnes false and mod6's function 
!       tries to take a new step. 
!------------------------------------------------------------------------------
        if ((n_blocked.eqv..TRUE.).and.(s_blocked.eqv..TRUE.).and.(e_blocked.eqv..TRUE.).and.(w_blocked.eqv..TRUE.)) then 
            blocked = .TRUE.
        end if
        end function path_is_blocked
end module blocked_path

        
