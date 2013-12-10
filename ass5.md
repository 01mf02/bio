---
title: Assignment 5 --- MOEA/D
author: Michael Färber
---

Description
-----------

I implemented a version of MOEA/D using the weighted sum approach. To recombine two solutions, I just take the average of the solution vectors. To mutate a solution, I take by average one component of the vector and add a value between -0.2 and 0.2, then I make sure that all components of the vector are still between 0 and 1.
The only real difference between the algorithm in the paper and my implementation is that about each 10th solutions update, I do a crossover with solutions from outside the neighborhood, to increase exploitation.


Results
-------

We use the following parameters:
* Number of different weight vectors: 300
* Vector dimension: 10
* Neighbourhood size: 3
* Iterations: 100

To obtain results, we first run our program:

  ocaml ass5.ml > ass5.dat

This takes about 50secs on my computer. Then, we plot the output:

  gnuplot -e "set terminal png; plot 'ass5.dat', 'LZ09_F1.pf'" > ass5.png

In this command, LZ09_F1.pf contains the Pareto front for the given problem. As we can see, the data produced by our program matches the Pareto front quite well. For a greater vector dimension, we also have to increase the number of iterations to get good output.