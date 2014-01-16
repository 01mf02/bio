---
title: Assignment 6 --- Ant Colony Optimisation
author: Michael Färber
---

Description
-----------

We had to implement an Ant Colony Optimisation (ACO) algorithm to solve the traveling salesman problem on a given dataset (att48.tsp).

To read the dataset, I reused my parser from assignment 1. After reading the cities, I calculate the distances between all of them, thus creating a distance matrix. Then I initialise my pheromone matrix (which records the pheromone levels between all cities) with one.

The algorithm then works as follows: I create n ants and let them find a path through all cities, using the same pheromone matrix for each ant. When every ant has found a path and has safely returned to its bretheren in the nest (just joking, I do not consider going back to the nest), I compare the length of the ant paths and choose the smallest one. Then, I increase pheromone levels on all sections of the shortest ant path by a constant value. Finally, I evaporate the whole pheromone on the field by multiplicating the pheromone matrix with an evaporation factor, and run the algorithm with the updated pheromone matrix.

Originally, I considered all ant paths (not only the shortest one) for the pheromone update, but my experiments have shown that this prevents convergence of the algorithm: I calculated the ratio minimal_path_length / length_of_current_path and updated the pheromone levels for every path with these ratios, but even after exponentiating these ratios, this still was by far inferior to just considering the shortest path and completely forgetting about longer ones.


Results
-------

I use the following parameters:
* Ant population size: 50
* Evaporation factor: 0.2
* Iterations: 150

With these parameters, the algorithm produces a path length close to 46,000, down from about 130,000 at the beginning. The path looks as follows:

1 8 9 38 31 44 28 36 30 37 17 6 19 27 43 7 18 46 33 20 11 23 47 13 41 16 22 40 15 12 21 29 2 24 10 26 4 45 35 42 34 25 32 39 48 5 14 3

That means that compared to the optimal solution, 5 of the 6 initial cities are the same. Later on though, the solutions differ more and more. The runtime is about 8 seconds.

It seems that bigger ant populations do not really help the algorithm, but only increase the runtime. Choosing the ant population size too low, however, will decrease the probability of ants discovering good solutions in each iteration of the algorithm. Seeing that the number of cities in the given problem set is 48 and my experiments have shown that 50 is a quite good population size, it might seem reasonable to conclude that the ant population size should be chosen to be about equal to the number of cities considered. A short test on the dataset for assignment 1 (280 cities) showed that 280 was a much better population size than 50 for that problem, because the shortest path length steadily decreased for the former population size, but jumped up and down for the latter. For this dataset, the performance was also considerably slower than for the smaller first dataset, probably due to the much bigger pheromone matrix.

A higher evaporation factor increases exploration.