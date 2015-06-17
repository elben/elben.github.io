---
layout: post
title: Planjure: A* and Dijkstra's in Om
categories: blog
tags: clojure
---

I wrote [Planjure](/p/planjure/) to learn ClojureScript and Om. It's a fun little program. You paint islands on a canvas and run path-planning algorithms to find the optimal path for your ship to traverse from start to finish. Sailing across blue ocean is faster than hiking through green islands. The three algorithms implemented are Dijkstra’s, A\* and depth-first.

My favorite feature is Visited mode. When you enable Visited, you'll be able to see the nodes that the algorithm had to visit in its search for the optimal path. A node is visited only if the algorithm thinks that an optimal path may use the node. An unvisited node implies that the algorithm has found a faster path that does not require that node.

In the example below, Dijkstra’s algorithm has to check almost every single node except the center of the island (because trekking across the island is more difficult than sailing around it).

<div class="some-padding"><img src="{{ site.url }}/images/planjure/dijkstra.jpg"/></div>

But when we run A\*, we find that a lot of the nodes are ignored! This is because A\* has a better heuristic for determining if a node should be visited or not.

<div class="some-padding"><img src="{{ site.url }}/images/planjure/astar.jpg"/></div>

Here's one more with a more complex world:

<div class="some-padding"><img src="{{ site.url }}/images/planjure/islands.jpg"/></div>

Play around with Planjure [here](/p/planjure/), and read the source code [here](https://github.com/elben/planjure/).
