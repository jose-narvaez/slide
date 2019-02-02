import System.Random

---- Part 1 ----

{- This type is used to define positions in the slide where the first float from the tuple is the x-position and the second the is the y-position. -}
type Point = (Float,Float)

{-
velocity:
This function takes as input two floats: the starting height of the slide y0, and height of the i-th supporting point yi.
It calculates the velocity at the i-th supporting point.
Returns velocity as a float. -}
velocity :: Float -> Float -> Float
velocity y0 yi = sqrt ((9.81 * (y0-yi))/0.5)

{-
distance:
This function takes as input two Points.
It calculates the Euclidian distance between point (a,b) and (x,y).
Returns the distance as a Float.
-}
distance :: Point -> Point -> Float
distance (a,b) (x,y) = sqrt(((a-x)^2)+ ((b-y)^2))

{-
time:
This functions takes as input the distance d between two points, the velocity at first point and velocity at second point.
It calculates the sliding time from one point to another.
It returns time as a Float.
-}
time :: Float -> Float -> Float -> Float
time d vi vf = d / ((vi+vf)/2)

{-
orderlist_points (helper fucntion for arrange_in_pairs):
This function takes as input a list of supporting points.
It arranges the first and second elements of the list into a tuple.
It uses recursion by taking the tail of the list and arranging the first and second elements into a tuple.
It reaches the base case when the list of points consists of one element.
It returns a list of tuples.
-}
orderlist_points :: [Point] -> [(Point, Point)]
orderlist_points (x:[]) = []
orderlist_points (x:xs) =[(x,head xs)] ++ orderlist_points xs

{-
arrange_in_pairs:
This function takes as input an initial point i, an ending point f, and a list of supporting points.
It arranges the initial point and the first supporting point into a tuple.
It uses orderlist_points to arrange the supporting points into tuples.
It arranges the last supporting point with the ending point into a tuple.
It concatenates the three lists of tuples and returns a list of tuples.
It uses pattern matching in case the list of points contains a single point.
-}
arrange_in_pairs :: Point -> Point -> [Point] -> [(Point, Point)]
arrange_in_pairs i f (x:[]) = [(i,x)]++[(x,f)]
arrange_in_pairs i f (x:xs) = [(i,x)]++ orderlist_points (x:xs) ++ [(last xs,f)]

{-
total_time:
This function takes as input the starting and ending points and a list of supporting points.
It uses a list comprehension to create a new list of times.
For every tuple of the arrange_in_pairs list, it calculates the time it takes to move from the first point in the tuple to the second point using the distance and velocity functions.
After it gets each individual time, it adds up all of them using sum on the list.
It returns the total time as a Float.
-}
total_time :: Point -> Point -> [Point] -> Float
total_time (x0,y0) (xf,yf) xs = sum [time (distance (fst x) (snd x)) (velocity y0 (snd(fst(x)))) (velocity y0 (snd(snd(x))))  | x <- arranged_list]
  where
    arranged_list = arrange_in_pairs (x0,y0) (xf,yf) xs

---- Part 2 ----

{- This type is used to define a Candidate which consists of the starting point of the slide, the ending point, a list of supporting points and the total time for that slide. -}
type Candidate = (Point, Point, [Point], Float)

{-
make_candidates:
This function takes as input the starting point, the ending point and a list of lists of points where every list of points is a different set of supporting points.
It creates a candidate which consists of the initial and ending point, the first list of supporting points and its total time calculated using the total_time function.
It uses recursion and calls make_candidates with the tail of the list of all supporting points.
It reaches the base case when the list of all supporting points is empty.
It returns a list of candidates.
-}
make_candidates :: Point -> Point -> [[Point]] -> [Candidate]
make_candidates i f [] = []
make_candidates i f (x:xs) = [(i,f,x,(total_time i f x))] ++ make_candidates i f (xs)

{-
get_4 (helper function for sort_by_time):
It gets as input a candidate.
It returns the fourth element of a candidate i.e the total time.
-}
get_4 :: Candidate -> Float
get_4 (_,_,_,x) = x

{-
sort_by_time:
This function is a modification of the quicksort function.
This function takes as input a list of candidates.
It uses the head of the list as base.
It separates the candidates that have a smaller time than the base and the candidates that have greater time.
It then uses recursion on both new lists, taking the first elements of each list as a new base.
It reaches the base case when the list of candidates is empty.
It concatenates all resulting lists.
It returns a sorted list of all candidates where the first candidate is the one with the smallest time.
-}
sort_by_time :: [Candidate] -> [Candidate]
sort_by_time [] = []
sort_by_time (x:xs) = sort_by_time smaller ++ [x] ++ sort_by_time larger
  where
    smaller = [a |a <- xs, get_4 a <= get_4 x]
    larger = [b |b <- xs, get_4 b > get_4 x]


{-
candidate_to_string:
This function takes as input a candidate.
It converts the staring point into a string using the point_to_string function.
It uses the list_points_string to convert the list of supporting points into string.
It converts the ending point using point_to_string function.
It converts the time into string adding the message "Time: " in front of it.
It concatenates all strings and returns a string representing the candidate.
-}
candidate_to_string :: Candidate -> String
candidate_to_string (i,f,xs,t) = (point_to_string i) ++ (list_points_string (i,f,xs,t)) ++  (point_to_string f) ++  "Time: " ++ (show (t) ++ "\n")

{-
list_points_string (helper function for candidate_to_string):
This function takes as input a candidate.
It converts the first supporting point into a string using the point_to_string function.
It uses recursion by taking the tail of supporting points and converts the first point of the new list into a string.
It reaches the base case until the list of supporting points is empty.
It returns the list of supporting points as a string.
-}
list_points_string :: Candidate -> String
list_points_string (i,f,[],t) = []
list_points_string (i,f,(x:xs),t) = point_to_string x ++ list_points_string (i,f,xs,t)

{-
point_to_string (helper function for list_points_string):
This function takes as input a point.
It converts the first and second values of the tuple into strings.
The string are concatenated with an empty space in between them and with a new line indicator at the end.
It returns the point as a string.
-}
point_to_string :: Point -> String
point_to_string (x,y) = show (x) ++" "++ show (y) ++ "\n"

{-
divide_list:
This function takes as input two lists, where the first list is a list of x-values for supporting points and the second list possible y-values.
It uses zip to get a list of supporting points where the y-values are the first n values of the y-list. n is the length of the x-values list.
It then uses recursion to generate a list of supporting points with the next y-values. It drops n elements of the y-list and calls divide_list again.
The base case is reached then the y-values list is empty.
It returns a list of lists of supporting points.
-}
divide_list :: [Float] -> [Float] -> [[Point]]
divide_list _ [] = []
divide_list xs ys = [zip xs ys] ++ divide_list xs (drop (length xs) ys)

---- Part 3 ----

{-
random_list (helper function for create_random_candidates):
This function takes as input the number of random numbers that are to be created, an interval and a StdGen.
It creates a random number and a new generator using randomR.
It uses recursion by subtracting 1 to the number of random numbers to be created and creates another random number using same interval and the new generator.
It reaches the base case when the number of random numbers to be created is 0.
It returns a tuple which consists of a list of n random floats and a StdGen.
-}
random_list :: Int -> (Float,Float) -> StdGen -> ([Float], StdGen)
random_list 0 minmax gen = ([], gen)
random_list n minmax gen = ((r:rs), g2)
  where
    (r,g) = randomR minmax gen
    (rs, g2) = random_list (n-1) minmax g

{-
create_random_candidates:
This function takes as input the number of candidates to be created, a starting point, an ending point, a list of x-values, an interval for y-values and a StdGen.
It creates a list of random y-values. The length of this list is the number of candidates that are to be created multiplied by the length of x-values list.
It then uses the divide list function to create a list of lists of supporting points.
It finally uses the make_candidates function to create a list of candidates which have the same initial and ending points and one of the supporting points list.
It returns a tuple which contains a list of candidates and a StdGen.
-}
create_random_candidates :: Int -> Point -> Point -> [Float] -> (Float, Float) -> StdGen -> ([Candidate], StdGen)
create_random_candidates n i f x_val y_interval gen = (make_candidates i f point_list, g2)
  where
    y_val_length = n*(length x_val)
    (y_val, g2) = random_list y_val_length y_interval gen
    point_list = divide_list x_val y_val


---- Part 4 ----

{-
crossover:
The function gets as input a list of candidates, the number of candidates to be crossbred among each other, and a StdGen.
It uses pairs to generate a list of candidate tuples i.e [(Candidate, Candidate)], it creates all possible combinations of tuples by using a list comprehension.
It uses cross_pairs on the list of candidate tuples to create a list with all new candidates.
It appends the original candidate list with the list containing the new candidates.
Returns a tuple which contains a list of the original and new candidates and the StdGen generated using the cross_pairs function.

It creates one new candidate for each tuple of candidates. When n is 2 it creates 1 new candidate, if n is 3 it creates 3 new candidates, if n is 4 it creates 6 new candidates and so on.
To crossbreed the best candidates the list needs to be sorted.
 -}
crossover :: [Candidate] -> Int -> StdGen -> ([Candidate], StdGen)
crossover cs n g = (cs ++ cs_new, g1)
  where
    pairs = [(( cs !! c1), (cs !! c2)) | c1 <- [0..(n-1)], c2 <- [(c1+1)..(n-1)]]
    (cs_new, g1) = cross_pairs pairs g

{-
cross_pairs:
This function takes as input a list of candidate tuples and a StdGen.
It creates a list of new candidates by getting a new candidate from every candidate tuple in the list.
It uses cross_pair to determine a new candidate from the first tuple of the list.
It uses recursion and takes the tail of the list to determine a new candidate from the next tuple.
It continues with the recursion until it reaches the base case when the list of tuples is empty.
It returns a tuple which consists of a list of the new candidates and a new StdGen.
-}
cross_pairs :: [( Candidate, Candidate)] -> StdGen -> ([Candidate], StdGen)
cross_pairs [] g = ([], g)
cross_pairs (cp:cps) g = (c:cs, g2)
  where
    (c, g1) = cross_pair cp g
    (cs, g2) = cross_pairs cps g1

{-
cross_pair:
This function takes as input a tuple of candidates and a StdGen.
This function creates a new candidate from the tuple.
It creates a new list of supporting points using the function cross_supp.
It calculates a new total time using the total_time function for the new candidate which has the same starting and ending points and the new list of supporting points.
It returns a tuple which consists of a a new candidate created and a new StdGen.
-}
cross_pair :: (Candidate, Candidate) -> StdGen -> (Candidate, StdGen)
cross_pair ((s, e, ps1, _), (_, _, ps2, _)) g = ((s, e, ps, t ), g1)
  where
  (ps, g1) = cross_supp ps1 ps2 g
  t = total_time s e ps

{-
cross_supp:
This function creates a new list of supporting points.
This function takes as input two list of supporting points.
It generates a random float with value between 0 and 1 using randomR.
If the random float is less than 0.5 it appends the first point of the first list of supporting points to a new list of supporting points.
If the random float is greater than 0.5 then it appends the first point of the second list.
It then uses recursion, it calls the function with the tail of both lists to select a second point to append to the list of new supporting points.
It reaches the base case when both lists are empty.
It returns a tuple which contains a list of new supporting points and a new StdGen.
-}
cross_supp :: [Point] -> [Point] -> StdGen -> ([Point], StdGen)
cross_supp [] [] g = ([], g)
cross_supp (c1:cs1) (c2:cs2) g = (( if r < 0.5 then c1 else c2) : xs, g2)
  where
    (r, g1) = randomR (0 :: Float, 1 :: Float) g
    (xs, g2) = cross_supp cs1 cs2 g1 {- recursion using tail of lists-}

{-
mutation:
This function takes as input the number of candidates to be mutated, the number of points to be mutated in each candidate,
an interval for y-values, a list of candidates and a StdGen.
It calls mutating_candidates to get a list of all new mutated candidates.
It appends the original list of candidates with the list of mutated candidates
It returns a tuple which consists of the list of original and new candidates and a StdGen.
-}
mutation :: Int -> Int -> (Float,Float) -> [Candidate] -> StdGen -> ([Candidate], StdGen)
mutation num_can num_points minmax cand_list gen = ((old_candidates ++ new_candidates), g1)
  where
  old_candidates = cand_list
  (new_candidates, g1) = mutating_candidates num_can num_points minmax cand_list gen

{-
mutating_candidates:
This function takes as input the number of candidates to be mutated, the number of points to be mutated in each candidate,
an interval for y-values, a list of candidates and a StdGen.
It uses the mutating_candidate function to mutate the first candidate of the list and adds it to a new list.
It uses recursion and takes the tail of the candidates and number of candidates to be mutated minus 1.
It now mutates the second candidate of the original list of candidates.
It ends the recursion when the base case is reached, when the number of candidates to be mutated is 0.
It returns a tuple which consists of a list of new candidates and a new StdGen.
-}
mutating_candidates :: Int -> Int -> (Float,Float) -> [Candidate] -> StdGen -> ([Candidate], StdGen)
mutating_candidates 0 _ _ _ gen = ([],gen)
mutating_candidates num_can num_points minmax cand_list gen = ((c:cs), g2)
  where
    (c,g1) = mutating_candidate num_points (head cand_list) minmax gen
    (cs,g2) = mutating_candidates (num_can-1) num_points minmax (tail cand_list) g1


{-
mutating_candidate:
This function takes as input the number of points to be mutated, a candidate, an interval for y-values and a StdGen.
It uses the function mutating_points to create a new list of points.
It calculates the total time for the new list of points using the total_time function.
It returns a tuple which contains a candidate which has the same initial and final points, the mutated list of points
and the new time, and the second element of the tuple is a new StdGen.
-}
mutating_candidate :: Int -> Candidate ->  (Float, Float) -> StdGen -> (Candidate, StdGen)
mutating_candidate num_points (i,f,point_list,t) minmax gen = ((i,f,new_point_list, new_t),g1)
  where
    (new_point_list, g1) = mutating_points num_points point_list minmax gen
    new_t = total_time i f new_point_list

{-
mutating_points:
This Function takes as input the number of points to be mutated, a list of supporting points,
an interval for y-values and a StdGen.
It creates a random position using the function random_pos
It uses mutating_point to mutate the point with random position from the list of points.
It uses the replace function to substitute the new random point generated on the original list of points
It then uses recursion, if only one point was required to mutate, the function finishes.
Otherwise the function is called again with number of points minus 1 and the new list of points.
If it reaches the base case, when the number of points to be mutated is 0, the function ends.
It returns a tuple which contains the mutated list of points and a new StdGen.
This function generated a random position every time a point needs to be mutated, this means that a same position can be mutated more than once.
-}
mutating_points :: Int -> [Point] ->  (Float, Float) -> StdGen -> ([Point], StdGen)
mutating_points 0 point_list _ gen = (point_list ,gen)
mutating_points num_points point_list minmax gen  = (new_points, g3)
  where
    (position, g1) = random_pos point_list gen
    (sub, g2) = mutating_point (point_list !! position) minmax g1
    new_point_list = replace position sub point_list
    (new_points, g3) = mutating_points (num_points-1) new_point_list minmax g2

{-
replace:
This function takes as input a position,  a supporting point and a list of supporting points.
This function changes the list of points by substituting one of the points by a new point
e.g we have [p1,p2,p3] and we want to substitute p_new in a certain position (either 0,1 or 2)
If the position is 0 it concatenates the new point to the tail of the original list of points,
otherwise it concatenates the first point of the original list and uses recursion with the tail of the list and with new position which is position -1.
This function returns a new list of points with a certain new point substituted.
-}
replace :: Int -> Point -> [Point] -> [Point]
replace position value (p:ps)
  | position == 0 = (value:ps)
  | otherwise =  p: (replace (position-1) value ps)

{-
random_pos:
This function takes as input a list of supporting points and a StdGen
It uses randomR to create a random position. It uses the length of the list of points as interval indicator (to produce a possible position)
It returns a tuple which contains a random position and a StdGen.
-}
random_pos :: [Point] -> StdGen -> (Int, StdGen)
random_pos point_list gen = (position, g1)
  where
    (position,g1)= randomR (0, (length point_list)-1) gen

{-
mutating_point:
This function takes as input a point which represents one of the supporting points and a tuple of Floats which is an interval for y-values.
It uses randomR to create a random value.
It modifies the point by changing the y value to the new random value and it keeps x as it was
It returns a tuple, which contains the new point and a StdGen
-}
mutating_point :: Point -> (Float, Float) -> StdGen -> (Point, StdGen)
mutating_point (x, y) minmax gen = ((x,y2), g2)
  where
    (y2, g2) = randomR minmax gen

---- Part 5 ----

{- In order to plot the graph of the best candidate I used python and a library called pylab which contains the function plot.
The function plot, plots a two dimensional graph using points with x and y coordinates. The function uses as input two lists of numbers,
where the first list contains the x values and the second list the y values of the points, it then returns a graph with the given values.
e.g the graph of points (2,3) (4,5) (6,7) would need to done as: pylab.plot([2,4,6], [3,5,7])

The function below, output_candidate, creates the best candidate and then returns a tuple that consists of two lists, the x and y values lists.
The output_candidate function creates the arguments that needs to be entered into the pylab.plot function. Hence, in order to create the graph,
you would need to call the function output_candidate and then copy the result into a python script, copying it after pylab.plot.
You then would need to run the program. The python script needs to contain import pylab, the pylab.plot with its arguments and pylab.show()
in order to display the graph.
e.g output_candidate returns ([2,4,6], [3,5,7]), copy that after pylab.plot -}

{-
output_candidate:
This function takes as input a starting and ending point, a list of x-values, the number of iterations, the size of the list of candidates,
the number of candidates to be crossbred among each other, number of mutations, number of points to be mutated, an interval for y-values and a seed.
It uses create_slide to get the best slide after certain amount of iterations.
From this candidate it takes its list of supporting points and uses functions get_ylist and get_xlist to get two list of floats.
It adds the starting x value and ending x value to the list of x values
It adds the starting y value and ending y value to the list of y values
It returns a tuple which contains two lists, a list containing all x values and a list containing all y values of the points -}
output_candidate :: Point -> Point -> [Float] -> Int -> Int -> Int -> Int -> Int -> (Float, Float) -> Int -> ([Float], [Float])
output_candidate  i f x_val iterations size n_cross n_mut num_points minmax seed = (x,y)
  where
   ((xi,yi),(xf,yf),xs,t) = create_slide i f x_val iterations size n_cross n_mut num_points minmax seed
   x= [xi]++(get_xlist xs)++[xf]
   y= [yi]++get_ylist xs++[yf]

{-
get_ylist:
This function gets as input a list of supporting points.
It gets the y value of the first point and adds it into a list.
It uses recursion to to the same on the second point and again until it reaches the base case which is an empty list.
It returns a list of y values. (list of floats)-}
get_ylist :: [Point] -> [Float]
get_ylist [] = []
get_ylist (n:ns) = y : get_ylist ns
  where
    (x,y) = n
{-
get_xlist:
This function gets as input a list of supporting points.
It gets the x value of the first point and adds it into a list.
It uses recursion to to the same on the second point and again until it reaches the base case which is an empty list.
It returns a list of x values. (list of floats)
-}
get_xlist :: [Point] -> [Float]
get_xlist [] = []
get_xlist (n:ns) = x : get_xlist ns
  where
    (x,y) = n

---- Part 6 ----

{-
create_slide:
This function takes as input a starting and ending point, a list of x-values, the number of iterations, the size of the list of candidates,
the number of candidates to be crossbred among each other, number of mutations, number of points to be mutated, an interval for y-values and a seed.
It uses create_random_candidates to create a list of random candidates.
It uses random candidates in the all_iterations function to get a list of the best candidates after certain amount of iterations.
It takes the head of the list of the best slides (which will be best the candidate).
It returns a candidate
-}
create_slide :: Point -> Point -> [Float] -> Int -> Int -> Int -> Int -> Int -> (Float, Float) -> Int -> Candidate
create_slide i f x_val iterations size n_cross n_mut num_points minmax seed = head (best_slide)
  where
    (random_candidates, g1) = create_random_candidates size i f x_val minmax (mkStdGen seed)
    best_slide = all_iterations random_candidates iterations size n_cross n_mut num_points minmax g1

{-
all_iterations:
It takes as input a list of candidates, the number of iterations, the size of the list of candidates, the number of
candidates to be crossbred among each other, number of mutations, number of points to be mutated, an interval for y-values and a StdGen.
It sorts the list of candidates in order to mutate and crossbred the best candidates.
It uses crossover function to generate new candidates.
It uses mutation function to generate new candidates.
It drops the original candidates from the new candidate list generated by crossover and mutation
It creates a new sorted list that contains the original candidates, and both new lists of candidates
It takes the first n (size) elements in order to make the new list the same size as the original.
It uses recursion, and calls the same function with the list of new candidates, the number of iterations minus 1 and a new StdGen.
It finishes when the function reaches the base case: when the number of iterations is equal to 0.
It returns a list of the best candidates after n iterations.

-}
all_iterations :: [Candidate] -> Int -> Int -> Int -> Int -> Int -> (Float, Float) -> StdGen -> [Candidate]
all_iterations can 0 _ _ _ _ _ _  = can
all_iterations can iterations size n_cross n_mut num_points minmax gen = list_candidate
  where
    sorted = sort_by_time can
    (crossovers, g1) = crossover sorted n_cross gen
    (mutations, g2) = mutation n_mut num_points minmax sorted g1
    new_cross = drop size crossovers
    new_mut = drop size mutations
    new = sort_by_time (sorted ++ new_cross ++ new_mut)
    new_candidates = take size new
    (list_candidate) = all_iterations new_candidates (iterations-1) size n_cross n_mut num_points minmax g2
