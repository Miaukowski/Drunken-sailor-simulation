module validate_step_pos
    contains 
        logical function validate_step_saw(next_position, grid, steps) result(ft)
        integer, intent(in) :: steps
        integer, intent(in) :: next_position(2), grid(2*steps+3, 2*steps+3)
        integer :: midpoint
        
        midpoint = steps + 1
!-------------------------------------------------------------------------------------------------
!	This functions purpose is to check if the proposed next step has already been visited
!       if it has not been visited it returns true, else it returns false. This is then used 
!       in the do while loop in mod6, since it stops when it becomes true (so if it's a valid proposed step).
!-------------------------------------------------------------------------------------------------
        
        if (grid(midpoint + next_position(1), midpoint + next_position(2)) == 1) then
            ft = .FALSE.
        else
            ft = .TRUE.
        end if
        end function validate_step_saw
      
        
        
end module validate_step_pos
        
