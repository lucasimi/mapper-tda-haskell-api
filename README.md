# mapper-tda-haskell-api

This package provides a web server exposing an API for the Mapper Algorithm from Topological Data Analysis, a branch of data analysis using topological tools to recover insights from datasets. In the following, we give a brief description of the algorithm, but the interested user is advised to take a look at the original [paper](https://research.math.osu.edu/tgda/mapperPBG.pdf). The Mapper Algorithm builds a graph from a given dataset, and some user choices. The output graph, called "mapper graph" gives a global approximation of (some) topological features of the original dataset, giving direct information about its shape and its connected components.

### Input
Assume we have a dataset $D$ inside a metric space $X$, together with the following choices:

1. A continuous map $f:X \to Y$
2. A cover algorithm for $f(D)$
3. A clustering algorithm for $D$.

### Steps
The mapper algorithm follows these steps:

1. Build an open cover of $f(D)$
2. For each open chart U of $f(D)$ let $V$ the preimage of $U$ under $f$, then the $V$'s form an open cover of $D$. For each $V$, run the chosen clustering algorithm
3. For each local cluster obtained, build a node. Whenever two local clusters (from different $V$'s) intersect, draw an edge between their corresponding nodes.

The graph obtained is called a "mapper graph".
