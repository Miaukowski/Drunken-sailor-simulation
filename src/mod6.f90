module perform_sim
use sim_run
contains 
    function perform_simulation(iterations, steps, print_trajectories, saw) result(return_results)
    implicit none
    integer,intent(in) :: iterations, steps, print_trajectories, saw
    integer:: steps_list(iterations), iteration_list(iterations), res(3)
    integer :: times_reached_goal,i
    integer,parameter :: rk=selected_real_kind(10,40)
    real :: win_prob,loose_prob, mean_walkdist, max_walkdist
    real :: min_walkdist, mean_minutes, max_minutes, min_minutes, return_results(8)

   
    times_reached_goal = 0
    steps_list = 0
    iteration_list = 0
    res = 0
    return_results = 0
!-----------------------------------------------------------------------
!   We initialize an array res(3), set it to be 0, and append	   
!   the results we get from performing the 
!   run_sim(steps, print_trajectories, saw) to that array. 
!   res(1) tells us if the sailor made it or not (1 = True, 0 = False)   
!   res(2) is the amount of blocks walked (so 1 block = 100m)
!   and res(3) is the time taken.
!----------------------------------------------------------------------

    do i=1,iterations
        res = run_sim(steps, print_trajectories, saw)

        if (res(1) == 1) then
            times_reached_goal = times_reached_goal +1
        end if
        steps_list(i) = res(2)
        iteration_list(i) = res(3)

    end do
    
!----------------------------------------------------------------------
!   This is the section where we calculate our results, these are also
!   the results the function returns to the main program DrunkenSailor
!----------------------------------------------------------------------    
    win_prob = (real(times_reached_goal, rk)/iterations)*100 
    loose_prob = 100-win_prob						!fractions
    mean_walkdist = (sum(steps_list)/iterations)/10.0		!in km
    max_walkdist = (maxval(steps_list))/10.0				!in km
    min_walkdist = (minval(steps_list))/10.0				!in km
    mean_minutes = (sum(iteration_list))/real(iterations, rk)	!in min
    max_minutes = maxval(iteration_list)
    min_minutes = minval(iteration_list)

    return_results(1) = mean_walkdist
    return_results(2) = min_walkdist
    return_results(3) = max_walkdist
    return_results(4) = mean_minutes
    return_results(5) = min_minutes
    return_results(6) = max_minutes
    return_results(7) = win_prob
    return_results(8) = loose_prob
    end function perform_simulation
    

end module perform_sim
         
        
    
	
	



