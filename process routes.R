# convert paths digitised by survey participants to network paths

# Approach - 
# 1 Filter network links to exclude motorways, unless specifically tagged as 
#   ‘bike’ or ‘walk’, and filter network nodes to those connected by the cyclable links
#   •	Note that this would allow cycling on some links that aren’t actually 
#     tagged as ‘bike’ (which cyclists may in fact do!)
#   •	Note that network is assumed to be one where all links are one way
# 2 Convert route strings in survey results to WKT, and then to an sf object
# 3 Extract the vertices of each string as points, and find the nearest node to each point
# 4 Eliminate any sections that return to a pre-used node as backtracking or a loop
# 5 For each segment between a pair of points, find the shortest route, and \
#   extract its nodes (vpath) and edges (epath)
#   •	Note that this finds a directed route (cyclists cannot ride the wrong way, 
#     or on a footpath, which some may in fact do)
# 6 Do another round of elimination of any sections that return to a pre-used 
#   node as backtracking or a loop
# 7 Write output
# 8 Produce check maps showing the survey and networked routes


# set inputs
networkFile <- "./data/network_melbourne_survey.sqlite"  # simplified network (note - all one way)
linkLayer <- "links"
nodeLayer <- "nodes"
surveyFile <- "./data/routedata.csv"
outputMapDir <- "./data/output maps"

# parameters
bufferDistance <- 100  # distance to buffer paths, creating constrained network to search for matching link


# set up environment
library(dplyr)
library(sf)
library(igraph)
library(stringr)
library(ggplot2)
library(ggspatial) ## map tiles
library(doSNOW)
library(parallel)
library(foreach)
# library(lwgeom)
# library(nngeo)  # for nn (nearest) - not used where k=1, use st_nearest_feature
# library(fs)  # for dir_walk

# dir_walk(path="./functions/",source, recurse=T, type = "file")


# 1 Load and process network ----
# -----------------------------------------------------------------------------#
# load network
links <- st_read(networkFile, layer = linkLayer)
nodes <- st_read(networkFile, layer = nodeLayer)

# exclude all links that are motorways, unless they are specifically tagged as
# cyclable or walkable
cyclable.links <- links %>%
  filter(!highway %in% c("motorway", "motorway_link") | 
           (highway %in% c("motorway", "motorway_link") & 
              (str_detect(modes, "walk") | str_detect(modes, "bike"))))

cyclable.nodes <- nodes %>%
  filter(id %in% cyclable.links$from_id | id %in% cyclable.links$to_id)

# nodes for from and to links
cyclable.from.nodes <- nodes %>%
  filter(id %in% cyclable.links$from_id)

cyclable.to.nodes <- nodes %>%
  filter(id %in% cyclable.links$to_id)

cyclable.both.nodes <- nodes %>%
  filter(id %in% cyclable.links$from_id & id %in% cyclable.links$to_id)
  

# get the crs of the network (so routes can be in same crs)
networkCrs <- st_crs(links)

# graph from network
graph <- graph_from_data_frame(cyclable.links %>%
                                 mutate(weight = length),
                               directed = T,
                               vertices = cyclable.nodes)


# 2 Load and process survey routes ----
# -----------------------------------------------------------------------------#
# load survey paths
routes <- read.csv(surveyFile)

# convert the route column to wkt (not needed where it's already wkt)
# for (i in 1:nrow(routes)) {
#   string <- routes$Route_to_work[i]
#   
#   # remove square brackets and double quotes, keep numbers and commas
#   # (note that the outer square brackets define a character class, containing
#   # the escaped elements ", [ and ]; the [ and ] require double escape)
#   cleaned_string <-  str_replace_all(string, "[\"\\[\\]]", "") 
#   
#   # split the string into a vector of numbers
#   coords <- as.numeric(strsplit(cleaned_string, ",")[[1]])
#   
#   # create a matrix with two columns for lat and long (reversing order)
#   coords_matrix <- matrix(coords, ncol = 2, byrow = TRUE)[, c(2, 1)]
#   
#   # create an sf object with a linestring geometry
#   linestring <- st_linestring(coords_matrix)
#   
#   # convert the linestring to WKT format
#   wkt_linestring <- st_as_text(linestring)
#   
#   # add a geom column containing wkt geometry
#   routes$WKT[i] <- wkt_linestring
# 
# }

# convert to an sf object in the correct CRS
routes_sf <- st_as_sf(routes, wkt = "WKT", crs = 4326) %>%
  st_transform(networkCrs)


# 3 Find routes  ----
# -----------------------------------------------------------------------------#
# function for eliminating loops/backtracks: returns any section where there
# is a return to a pre-used node
removeLoops <- function(selected.nodes) {
  idx_to_omit <- c()
  for (j in 2:length(selected.nodes)) {
    previous_nodes = selected.nodes[1:j-1]
    if (length(idx_to_omit) > 0) {
      retained_previous_nodes <- previous_nodes[-idx_to_omit]
    } else {
      retained_previous_nodes <- previous_nodes
    }
    if (selected.nodes[j] %in% retained_previous_nodes) {
      # find the first instance of the node that is repeated, and the node before its repeat
      first_match_idx = match(selected.nodes[j], previous_nodes)
      j_minus_1_idx = j-1
      # add sequence from first_match_idx to j-1 to the indices to omit, but only
      # if not previously omitted
      if (!first_match_idx %in% idx_to_omit & !j_minus_1_idx %in% idx_to_omit) {
        idx_to_omit <- c(idx_to_omit, first_match_idx:j_minus_1_idx)
      }
    }
  }
  
  return(idx_to_omit)
  
}

# table to hold outputs
routes_networked_base <- routes_sf %>%
  mutate(network_nodes = "",
         network_edges = "")

# setup for parallel processing - detect available cores and create cluster
cores <- detectCores()
cluster <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cluster)

# report
print(paste(Sys.time(), "| Finding routes for", nrow(routes_networked_base), 
            "survey paths; parallel processing with", cores, "cores"))

# set up progress reporting
# https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
pb <- txtProgressBar(max = nrow(routes_networked_base), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# find network routes
routes_networked <- 
  foreach(i = 1:nrow(routes_networked_base),
          # foreach(i = 1:8,
          .combine = rbind,
          .packages = c("dplyr", "sf", "igraph"),
          .options.snow = opts) %dopar% {
            
            route <- routes_networked_base[i,]
            
            # extract vertices as points
            vertices <- st_coordinates(route) %>%
              as.data.frame() %>%
              st_as_sf(coords = c("X", "Y"), crs = networkCrs)
            
            # find nearest node to each vertex
            nearest_nodes_start <- 
              cyclable.from.nodes$id[st_nearest_feature(vertices[1,], cyclable.from.nodes)]
            if(nrow(vertices) > 2) {
              second_last = nrow(vertices) - 1
              nearest_nodes_mid <- 
                cyclable.both.nodes$id[st_nearest_feature(vertices[2:second_last, ], cyclable.both.nodes)]
            } else {
              nearest_nodes_mid <- c()
            }
            nearest_nodes_end <- 
              cyclable.to.nodes$id[st_nearest_feature(vertices[nrow(vertices),], cyclable.to.nodes)]
            nearest_nodes <- c(nearest_nodes_start, nearest_nodes_mid, nearest_nodes_end)
            vertices <- cbind(vertices, nearest_nodes)
            
            # eliminate any sections that return to a pre-used node
            # identify the indices of the relevant nodes
            idx_to_omit <- removeLoops(nearest_nodes)
            
            # omit the vertices corresponding to the indices to be omitted, if any
            if (length(idx_to_omit > 0)) {
              vertices <- vertices[-idx_to_omit, ]
            }
            
            # proceed to find route if there is more than one vertex
            if (nrow(vertices) > 1) {
              # empty vectors to hold nodes and edges for the route
              node_list <- c()
              edge_list <- c()
              
              # find shortest routes
              for (j in 2:nrow(vertices)) {
                from_node <- vertices$nearest_nodes[j-1]
                to_node <- vertices$nearest_nodes[j]
                path <- shortest_paths(graph, 
                                       from = as.character(from_node),
                                       to = as.character(to_node),
                                       mode = "out",
                                       output = "both")
                
                # nodes and edges in the shortest route
                if (j == 2) {
                  # first section: all nodes
                  nodes <- cyclable.nodes$id[as.numeric(path$vpath[[1]])]
                } else {
                  # numbers of the nodes, except the first (which was last of the preceding segment)
                  nodes <- cyclable.nodes$id[as.numeric(path$vpath[[1]])[-1]]
                }
                
                edges <- edge_attr(graph, "link_id", path$epath[[1]])
                
                # add to the node and edge lists
                node_list <- c(node_list, nodes)
                edge_list <- c(edge_list, edges)
              }
              
              # do another round of eliminating any sections that return to a pre-used node
              idx_to_omit <- removeLoops(node_list)

              # if there are indices to be omitted, then omit the nodes and edges;
              # and test again for any further repetitions
              while (length(idx_to_omit > 0)) {
                node_list <- node_list[-idx_to_omit]
                edge_list <- edge_list[-idx_to_omit]
                idx_to_omit <- removeLoops(node_list)
              }
              
              # write to output row
              route$network_nodes <- toString(node_list)
              route$network_edges <- toString(edge_list)
              
            }
            
            # return output row
            return(route)
            
          }

# close the progress bar and cluster
close(pb)
stopCluster(cluster)

# save output
st_write(routes_networked, "./data/routes_networked.sqlite", 
         layer = "survey", delete_layer = TRUE)


# 4 Output paths  ----
# -----------------------------------------------------------------------------#
# reload routes (note code below assumes re-loading sqlite has converted column names to lower case)
routes_networked <- st_read("./data/routes_networked.sqlite")

# empty sf objects
routes_networked_paths <- st_sf(geometry = st_sfc(), 
                                data.frame(id = numeric(), 
                                           stringsAsFactors = FALSE),
                                crs = st_crs(links))

routes_networked_startpoints <- st_sf(geometry = st_sfc(), 
                                      data.frame(id = numeric(), 
                                                 stringsAsFactors = FALSE),
                                      crs = st_crs(links))

# create line and start point for each route
for (i in 1:nrow(routes_networked)) {
  link.ids <- routes_networked$network_edges[i] %>%
    strsplit(., ", ") %>%
    unlist() %>%
    as.numeric()
  
  link.startnode <- routes_networked$network_nodes[i] %>%
    strsplit(., ", ") %>%
    unlist() %>%
    .[1] %>%
    as.numeric()
  
  route_links <- links %>% 
    filter(link_id %in% link.ids) %>%
    st_union()
  
  route_start <- nodes %>%
    filter(id == link.startnode) %>%
    mutate(id = i) %>%
    dplyr::select(id)
  
  if (length(route_links) > 0) {
    routes_networked_paths <- bind_rows(routes_networked_paths, 
                                        st_sf(geometry = st_sfc(route_links), 
                                              id = i))
    
    routes_networked_startpoints <- rbind(routes_networked_startpoints,
                                              route_start)
  } 
}

# write output
st_write(routes_networked_paths, "./data/routes_networked.sqlite", 
         layer = "networked", delete_layer = TRUE)
st_write(routes_networked_startpoints, "./data/routes_networked.sqlite",
         layer = "startpoints", delete_layer = TRUE)



# 5 Output maps  ----
# -----------------------------------------------------------------------------#
# create folder for maps if doesn't exist
if (!dir.exists(outputMapDir)) {
  dir.create(outputMapDir)
}

# reload routes (note code below assumes re-loading sqlite has converted column names to lower case)
routes_networked <- st_read("./data/routes_networked.sqlite")

# setup for parallel processing - detect available cores and create cluster
cores <- detectCores()
cluster <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cluster)

# report
print(paste(Sys.time(), "| Printing maps for", nrow(routes_networked), 
            "network maps; parallel processing with", cores, "cores"))

# set up progress reporting
# https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
pb <- txtProgressBar(max = nrow(routes_networked_base), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# create and save map for each route 
output <- 
  foreach(i = 1:nrow(routes_networked_base),
          # foreach(i = 1:8,
          .packages = c("dplyr", "sf", "stringr", "ggplot2", "ggspatial"),
          .options.snow = opts) %dopar% {
            
            # selected route
            route <- routes_networked[i,]
            
            map.title <- paste0("Map no ", i, ",  routeID ", route$routeID)
            map.filename <- paste0("map_", i, "_routeID_", route$routeID)
            
            # surrounding roads
            route_bbox <- st_bbox(route) %>%
              st_as_sfc() %>%
              st_buffer(., sqrt(as.numeric(st_area(.)))/10)
            surrounding_edges <- st_intersection(cyclable.links, route_bbox)
            
            # map zoom
            if (as.numeric(st_area(route_bbox)) < 500000) {
              map.zoom = 17
            } else if (as.numeric(st_area(route_bbox)) < 4000000) {
              map.zoom = 15
            } else {
              map.zoom = 13
            }
            
            # networked route
            network_edges <- as.numeric(unlist(str_split(route$network_edges, ", ")))
            networked_route <- cyclable.links %>%
              filter(link_id %in% network_edges)
            
            if (nrow(networked_route) > 0) {
              # starting point
              starting_node <- route$network_nodes[1] %>%
                str_split(., ", ") %>%
                unlist() %>%
                .[1] %>%
                as.numeric()
              starting_point <- cyclable.nodes %>%
                filter(id == starting_node)
              
              map <- ggplot() +
                annotation_map_tile(type = "osm", zoom = map.zoom, alpha = 0.8) +
                geom_sf(data = surrounding_edges, colour = "black", linewidth = 0.25) +
                geom_sf(data = route, aes(colour = "Survey route"), linewidth = 2, alpha = 0.8) +
                geom_sf(data = starting_point, aes(colour = "Starting point"), size = 4) +
                geom_sf(data = networked_route, aes(colour = "Networked route"), linewidth = 2, alpha = 0.8) +
                
                
                scale_color_manual(name = "",
                                   values = c("Networked route" = "blue",
                                              "Starting point" = "blue",
                                              "Survey route" = "red")) +
                
                guides(color = guide_legend(override.aes = list(linetype = c("solid", "blank", "solid"),
                                                                shape = c(NA, 16, NA)))) +
                
                theme(axis.ticks = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank()) +
                
                labs(title = map.title)
              
            } else {
              map <- ggplot() +
                annotation_map_tile(type = "osm", zoom = map.zoom, alpha = 0.8) +
                geom_sf(data = surrounding_edges, colour = "black", linewidth = 0.25) +
                geom_sf(data = route, aes(colour = "Survey route"), linewidth = 2, alpha = 0.8) +
                
                scale_color_manual(name = "",
                                   values = c("Survey route" = "red")) +
                
                # guides(color = guide_legend(override.aes = list(linetype = c("solid", "blank", "solid"),
                #                                                 shape = c(NA, 16, NA)))) +
                # 
                theme(axis.ticks = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank()) +
                
                labs(title = map.title,
                     subtitle = "No networked route found")
              
            }
            
            # map
            
            # save the map
            ggsave(paste0(outputMapDir, "/", map.filename ,".png"),
                   map,
                   width = 15, height = 12, units = "cm")
            
          }

# close the progress bar and cluster
close(pb)
stopCluster(cluster)

