prbnmcn-clustering
------------------

This library implements the following clustering algorithms:
- K-means
- K-medoids (using either 'Partition Around Medoids' or the 'Voronoi Iteration' algorithms)
- Agglomerative clustering (yielding dendrograms)

A basic example can be found in the `test` subdirectory.

Multi-start routines are also available to pick the best out of `n` initial
clusterings. At the time of writing, the implementation is entirely sequential.

# TODOs
- many low-hanging fruits for optimization
- implement parallel multi-start routine when multicore lands
