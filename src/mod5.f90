module sim_run
use blocked_path
use step
use validate_step_pos
contains 
    function run_sim(n_steps, print_path, self_avoiding) result (return_value)
    implicit none
    integer, intent (in) :: n_steps, print_path, self_avoiding
    integer:: x_positions(n_steps+1), y_positions(n_steps+1), return_value(3), next_position(2), current_position(2)
    integer, allocatable :: visited(:,:)
    integer :: step, final_step,i, ios, midpoint
    logical :: blocked_in, finished, valid_step
!-------------------------------------------------------------------------------------------------------------------
!   This is the function that does the hard work. For every iteration of simulation, it performs the do loop for
!   every step that the sailor does, and checks those steps with the help of other modules and the functions 
!   inside them. 
!-------------------------------------------------------------------------------------------------------------------
    x_positions = 0 		!coordinates in the x_axis
    y_positions = 0 		!coordinates in the y_axis
    current_position = 0 	!Keeping track on where the sailor is right now
    return_value = 0 		!The list that the function returns to mod6's function to use. 
    next_position = 0 		!Suggested next position
    midpoint = n_steps + 1 	!middle index is n_steps+1 in the array visited(:,:), used to set the midpoint of the array. 
    step = 0 			!The amount of "blocks" walked, 1 = 100m
    blocked_in = .FALSE. 	!we set this to false, not blocked in from the start
    final_step = n_steps 	!Amount of steps left
    finished = .FALSE.		!Set to false in the beggining, not finished from the start
    valid_step = .FALSE.	!Boolean to check if the step we are trying to take is "legal", must be set to false in the beggining
    
    
    
    if (self_avoiding==1) then 			!we check if we are doing SAW, and only then allocate an array to keep track on where we have been. 
    	allocate(visited(2*n_steps+3,2*n_steps+3))
    	visited = 0 					!we set to zero since we haven't visited any point yet
    	visited(midpoint, midpoint) = 1 		!here we now start, so we set the startpoints to 1
    end if
    
    if (print_path == 1) then !Here we simply print to the outputfile the first(!) x and y coordinates, since outside the do loop this will only happend once/iteration
            open(unit = 1, file = 'output.dat', status = 'old', iostat = ios)
            if (ios/=0) then 
                print *, 'Error creating a file' 
            end if 
            write(1, *) x_positions(1), y_positions(1)
        end if

    do i=2,n_steps+1 !The hard work do loop mentioned in the beggining. 

        if (self_avoiding==1) then !Check if doing SAW
            if (blocked_in.eqv..FALSE.) then 
                blocked_in = path_is_blocked(current_position, visited, n_steps) !utilizing the function in mod 4.
                								    !returns a boolean
            end if
            
            if (blocked_in.eqv..TRUE.) then 
                final_step = step-1 !keeping track on the number of the last step taken
            end if
        end if
        if (blocked_in.eqv..TRUE.) then 
            x_positions(i) = x_positions(i-1) !if we are blocked in(has already visited)in some direction in the current path
            y_positions(i) = x_positions(i-1) !we check if there are other directions we can take, so we "backtrace" and try again
            cycle 				!hence the cycle stement, it starts from "if (self_avoiding==1) then !Check if doing SAW" 
        end if					!again.
        
        step = step + 1 			!We have now taken one step
	valid_step = .FALSE.
	
        do while (valid_step.eqv..FALSE.)	!trying for legal positions to move to
            next_position = take_step(x_positions(i-1), y_positions(i-1)) !utilizes mod2,proposes a next_position from the take_step function 

            if (self_avoiding == 1) then
                valid_step = validate_step_saw(next_position, visited, n_steps) !here we check if the proposed next_position is "legal
            else
                valid_step = .TRUE. !if not in SAW, all proposed steps will be valid
            end if 
        end do 
        
        x_positions(i) = next_position(1) !we now append the legal position to the x-,y- coordinate "list"
        y_positions(i) = next_position(2)
        current_position = next_position
        
        if (self_avoiding == 1) then
        	visited(midpoint + current_position(1), midpoint + current_position(2)) = 1 !changing the visited position to be 1, so it
        end if										       !doesn't get visited again in SAW mode. 
        
	if (print_path == 1) then !Here we simply print to the outputfile our x and y coordinates in each iteration. 
            open(unit = 1, file = 'output.dat', status = 'old', iostat = ios)
            if (ios/=0) then 
                print *, 'Error creating a file' 
            end if 
            write(1, *) x_positions(i), y_positions(i)
        end if
        
        if (next_position(1) >= 10) then !so if this is true, it means we got to the shore that was 10 blocks away from our startingpoint (0,0) in the positive x axis
            return_value(1) = 1 !this get used in mod6 for keeping track on how many times reached goal. 1=True, 0=False
            return_value(2) = step !Amount of steps taken
            return_value(3) = step
            finished = .TRUE. !setting this to true so that when we exit the do loop, the following if statement doesn't trigger
            exit
        end if

    end do

    if (finished.eqv..FALSE.) then !we did not get to the shore. 
    return_value(1) = 0 !this get used in mod6 for keeping track on how many times reached goal. 1=True, 0=False
    return_value(2) = final_step !the number of the last step taken
    return_value(3) = n_steps !Amount of total steps
    end if 


    if (print_path == 1) then !this is done for the plotting program, two empty lines means a new block. So a new iteration in our case.
	open(unit = 1, file = 'output.dat', status = 'old', iostat = ios)
            if (ios/=0) then 
                print *, 'Error creating a file' 
            end if 
            write(1, *) ' '
            write(1, *) ' '
    end if


    end function run_sim
end module sim_run
