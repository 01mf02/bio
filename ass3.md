---
title: Assignment 3
author: Michael Färber
---


Approach no. 1
==============


Representation
--------------

At first, we represented solutions as lists, which would then be translated to binary trees by simply inserting all list elements one by one into a binary tree. This representation had the advantage of being quite easy to implement, at least at first sight.


Initial population
------------------

We take an initial list of the format [0, 1, ..., n] and generate a predefined number of permutations from this list, which then is the initial population.


Recombination
-------------

Recombination proved to be a hard nut for the list representation. We decided to use single point crossover, but of course there we had to eliminate duplicates. For this, we wrote a horribly complex function which counts the number of appearances of each element of the crossed-over list, then replaces double appearances by such elements which do not appear at all, thus restoring all elements of the original lists.


Mutation
--------

Mutation, on the other hand, is much easier to implement with lists: There, we just swap two random elements from a list.


Fitness functions
-----------------

We implemented different fitness functions, which all operate on the tree representation of the list.

* Depth: Simply measures the depth of the tree.
* Depth sum: Sums up the depth of all tree nodes.


Results
-------

For both data sets, we used 500 iterations and a population size of 100.

Data set 1:

* List: 100 elements
* Time: 49secs
* Depth: 7 (could store 127 elements, so optimal)

Data set 2:

* List: 500 elements
* Time: 5m20secs
* Depth: 11 (could store 2047 elements, so not optimal)



Approach no. 2
==============


Representation
--------------

In this second approach, we tried to see if using trees directly without resorting to lists could improve the performance of the algorithm.


Initial population
------------------

Quite similar to approach no. 1, but we convert the resulting lists directly to trees and exclusively operate on trees from this point on.


Recombination
-------------

At this point credit is due to Andreas Kochesser, who came up with an idea to recombine two trees whilst avoiding duplicates and missing elements. Please see his paper for an excellent description of the algorithm. :)

Roughly, we recombine a subtree of the male parent with the tree of the female, where the subtree of the male is chosen randomly.


Mutation
--------

To mutate a tree, we choose a random location in the tree and then rotate at this location. We experimented with rotating always in the same direction and also rotating in a random direction.


Fitness functions
-----------------

In addition to the fitness functions from approach no. 1, we used a fitness function which calculates pairs of tree depth and number of elements in the tree at the tree depth. That means that when we have two trees with the same depth, the one with fewer elements at the bottom will have a better fitness.


Results
-------

For both data sets, we used 500 iterations and a population size of 100.

Data set 1:

* List: 100 elements
* Time: 4secs
* Depth: 9 (could store 511 elements, so far from optimal)

Data set 2:

* List: 500 elements
* Time: 15secs
* Depth: 14 (could store 16383 elements, so quite far from optimal)

The cause of this relatively unsatisfying performance is yet unknown; it seems that already with quite small list sizes (e.g. 15 elements), the resulting sorted tree is not perfectly balanced. The algorithm seems to get stuck in local optima quite easily. However, even stronger mutation did not yield significantly better results.