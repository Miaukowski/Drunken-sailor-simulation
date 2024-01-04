reset
set key off #comment away if you want a legend, recommended with only a small number of iterations
set xrange[:11] #so we can see the sailors actually reach the goal and stop. 
set xlabel "x-coordinates"
set ylabel "y-coordinates"
plot for [i=0:*] 'output.dat' i i u 1:2 w l lc i t sprintf('This is iteration %d', i)
