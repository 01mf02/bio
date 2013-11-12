---
title: Assignment 4 --- Particle Swarm Optimization
author: Michael Färber
---

Description
-----------

The algorithm first generates a list of particles with random positions, i.e. a random swarm. At first, I also initialised the particles' velocity with a random value, but I soon found out that this made the algorithm converge slower, as the particles may move into a completely wrong direction and need some time to adapt to a better trajectory.

After the generation of the initial swarm, I call a recursive algorithm (called "fly"), which calculates the swarm best (global best), updates the swarm (by updating each of its particles using the swarm best) and then recurses.

The particle update calculates new particle velocity and position using the formulae from the lecture. In case a particle leaves a bound, we "clip" the particle position at the bound and multiply the new velocity with -1.



Results
-------

I tested both algorithms with 10000 iterations.


Genetic algorithm in assignment 2:
* Fitness: 0.000017
* Wallclock time: 1min1s

Particle swarm algorithm:
* Swarm size: 300 particles
* Inertia weight: 0.4
* Cognitive acceleration coefficient: 0.4
* Social acceleration coefficient: 3.6
* Fitness: 0.000000
* Wallclock time: 3m9s

The small cognitive acceleration coefficient might be surprising, but it turned out to be a quite sensible constant with 0.4 being a really good value. So peer pressure might be good after all. :)

We achieve better results with the PSO, but the PSO is slower.
