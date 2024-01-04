program DrunkenSailor
    use perform_sim
    implicit none 
    character(len=80) :: arg
    real :: results(8), res_time(4), mean_time(4),min_time(4), max_time(4)
    integer :: ship_steps,max_steps, n_simulations, print_trajectories, Saw
    
    call get_command_argument(1,arg); read(arg,*) n_simulations !Number of simulations
    call get_command_argument(2,arg); read(arg,*) print_trajectories !1 = true, 0 = false
    call get_command_argument(3,arg); read(arg,*) Saw !1 = true, 0 = false
    
    
    !sailor will walk 1 block per minute
    
    max_steps = 50 * 365 * 24 * 60 !if sailor is left to walk for 50 years, non SAW
    ship_steps = 10 * 60 !if sailor only has 10 hours to reach ships or shore, SAW
    
    if (Saw == 1) then 
    	results = perform_simulation(n_simulations, ship_steps, print_trajectories, saw)
    end if
    if (Saw == 0) then
    	results = perform_simulation(n_simulations, max_steps, print_trajectories, saw)
    end if
    mean_time = converter(results(4))
    min_time = converter(results(5))
    max_time = converter(results(6))
    print '(a,F12.2,a)','Mean walkindistance: ',results(1),'km'
    print '(a,F12.2,a)','Minimum walkingdistance: ', results(2), 'km'
    print '(a,F12.2,a)','Maximum walkingdistance: ', results(3), 'km'
    print '(a,F5.2,a,F6.2,a,F5.2,a,F5.2,a)', 'Mean time: ', mean_time(1), ' years, ',mean_time(2), &
     ' days, ', mean_time(3), ' hours, ', mean_time(4), ' minutes'
    print '(a,F5.2,a,F6.2,a,F5.2,a,F5.2,a)', 'Minimun time: ', min_time(1), ' years, ',min_time(2), &
     ' days, ', min_time(3), ' hours, ', min_time(4), ' minutes'
    print '(a,F5.2,a,F6.2,a,F5.2,a,F5.2,a)', 'Maximum time: ', max_time(1), ' years, ',max_time(2), &
    ' days, ', max_time(3), ' hours, ', max_time(4), ' minutes'
    print '(a,F12.2,a)', 'fraction of walkers who got to the ship on time: ', results(7), ' %'
    if (Saw == 0) then
    	print '(a,F12.2,a)', 'fraction of walkers who died before getting to the ship: ', results(8), ' %'
    end if
    
!----------------------------------------------------------------------------
!   Here we simply convert minutes -> appropriate units to be printed above
!----------------------------------------------------------------------------
    contains     
    function converter (mins) result(res_time)
    real, intent(in) :: mins
    real :: m, year_length, hour_length, day_length
    integer :: year, days, hours, minutes, res_time(4)
    hour_length = 60
    day_length = 24 * hour_length
    year_length = 365 * day_length
    
    year = nint(mins/year_length)
    m = modulo(mins,year_length)
    days = nint(m/day_length)
    m = modulo(m,day_length)
    hours = nint(m/hour_length)
    m = modulo(m,hour_length)
    minutes = m
    res_time(1) = year
    res_time(2) = days
    res_time(3) = hours
    res_time(4) = minutes
    end function converter
    
end program drunkensailor
