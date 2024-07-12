# cycling-survey-routes
Processing routes from cycling survey

`process.routes.R` does the following:

1 Filter network links to exclude motorways, unless specifically tagged as' bike’ or ‘walk’, and filter network nodes to those connected by the cyclable links

- Note that this would allow cycling on some links that aren’t actually tagged as ‘bike’ (which cyclists may in fact do!)
- Note that network is assumed to be one where all links are one way

2 Convert route strings in survey results to WKT, and then to an sf object

3 Extract the vertices of each string as points, and find the nearest node to each point

4 Eliminate any sections that return to a pre-used node as backtracking or a loop, except where start and end nodes are the same, or where manual inspection shows what looks like an obvious deliberate loop

5 For each segment between a pair of points, find the shortest route, and extract its nodes (vpath) and edges (epath)
- Note that this finds a directed route (cyclists cannot ride the wrong way, or on a footpath, which some may in fact do)

6 Do another round of elimination of any sections that return to a pre-used node as backtracking or a loop

7 Write output

8 Produce check maps showing the survey and networked routes

9 Find routes that pass through 'stressful junctions'

10 Produce an expanded version of the output routes with network link details attached converts the WKT strings representing the digitised routes provided by the survey participants into sections of the network that correspond as closely as possible to the digitised routes.
