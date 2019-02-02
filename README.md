# slide
Generation of a 2D slide using functional programming and genetic algorithms. 

This is a Haskell program implementing a genetic algorithm which computes the fastest slide (i.e. the supporting points forming the slide with the shortest sliding time). It uses basic physics concepts to calculate the time it would take an object to run through the slide. The overall goal of this project was to implement a genetic algorithm which creates a population of random slides (called candidates) and uses genetic operations to iteratively improve the quality of the population. After the last iteration, the best slide is returned. 

The function create_slide takes as input a starting and ending point, a list of x-values for the supporting points, the number of iterations, the size of the list of candidates, the number of candidates to be crossbred among each other, number of mutations, number of points to be mutated, an interval for y-values, and a seed for the random standard generator and returns the best slide which consist of an initial point with (x,y) coordinates, a final point, a list of supporting points and the total time of the slide.


