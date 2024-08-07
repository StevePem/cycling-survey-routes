# convert paths digitised by survey participants to network paths

# Approach - 
# 1 Filter network links to exclude motorways, unless specifically tagged as 
#   ‘bike’ or ‘walk’, and filter network nodes to those connected by the cyclable links
#   •	Note that this would allow cycling on some links that aren’t actually 
#     tagged as ‘bike’ (which cyclists may in fact do!)
#   •	Note that network is assumed to be one where all links are one way
# 2 Convert route strings in survey results to WKT, and then to an sf object
# 3 Extract the vertices of each string as points, and find the nearest node to each point
# 4 Eliminate any sections that return to a pre-used node as backtracking or a loop,
#   except where start and end nodes are the same, or where manual inspection
#   shows what looks like an obvious deliberate loop
# 5 For each segment between a pair of points, find the shortest route, and 
#   extract its nodes (vpath) and edges (epath)
#   •	Note that this finds a directed route (cyclists cannot ride the wrong way, 
#     or on a footpath, which some may in fact do)
# 6 Do another round of elimination of any sections that return to a pre-used 
#   node as backtracking or a loop
# 7 Write output
# 8 Produce check maps showing the survey and networked routes
# 9 Find routes that pass through 'stressful junctions'
# 10 Produce an expanded version of the output routes with network link details attached


# set inputs
networkFile <- "./data/network_weighted.sqlite"  # simplified network (note - all one way, contains LTS and other impedances)
linkLayer <- "links"
nodeLayer <- "nodes"
surveyFile <- "./data/routedata.csv"
outputMapDir <- "./data/output maps"
stressJctFile <- "./data/StressJct.csv"
favSpotFile <- "./data/FavSpot.csv"


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
library(fs)  # for dir_walk

dir_walk(path="./functions/",source, recurse=T, type = "file")


# 1 Load and process network ----
# -----------------------------------------------------------------------------#
# load network
links <- st_read(networkFile, layer = linkLayer)
nodes <- st_read(networkFile, layer = nodeLayer)

# exclude all links that are motorways, unless they are specifically tagged as
# cyclable or walkable; exclude public transport
all.cyclable.links <- links %>%
  filter(!highway %in% c("motorway", "motorway_link") | 
           (highway %in% c("motorway", "motorway_link") & 
              (str_detect(modes, "walk") | str_detect(modes, "bike")))) %>%
  filter(!highway %in% c("train", "tram", "bus"))

all.cyclable.nodes <- nodes %>%
  filter(id %in% all.cyclable.links$from_id | id %in% all.cyclable.links$to_id)

# keep largest connected network
largest.component <- largestConnectedComponent(all.cyclable.nodes, all.cyclable.links)
cyclable.nodes <- largest.component[[1]]
cyclable.links <- largest.component[[2]]


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
                                 mutate(weight = length) %>%
                                 dplyr::select(from_id, to_id, weight, link_id),
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
# is a return to a pre-used node, unless entire route is a loop
removeLoops <- function(selected.nodes, route) {
  idx_to_omit <- c()
  
  # don't eliminate if start and end nodes are the same, or other where manual
  # checking revealed a large loop component
  if (!(selected.nodes[1] == selected.nodes[length(selected.nodes)] &
        length(selected.nodes > 2)) &
      # manual (maps 353, 376, 542, 554, 517, 66)
      !(route$routeID %in% c("7bep8cwz3fb6_2", "7l9g4utj97r6_1",
                             "9vl86ffx3ue4_1", "9yr2zbt7i4s6_1",
                             "9pt99buo346p_1", "2t66ibj3wkt8_1"))) {
    
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
            idx_to_omit <- removeLoops(nearest_nodes, route)
            
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
                  shortest.nodes <- cyclable.nodes$id[as.numeric(path$vpath[[1]])]
                } else {
                  # numbers of the nodes, except the first (which was last of the preceding segment)
                  shortest.nodes <- cyclable.nodes$id[as.numeric(path$vpath[[1]])[-1]]
                }
                
                shortest.edges <- edge_attr(graph, "link_id", path$epath[[1]])
                
                # add to the node and edge lists
                node_list <- c(node_list, shortest.nodes)
                edge_list <- c(edge_list, shortest.edges)
              }
              
              # do another round of eliminating any sections that return to a pre-used node
              idx_to_omit <- removeLoops(node_list, route)

              # if there are indices to be omitted, then omit the nodes and edges;
              # and test again for any further repetitions
              while (length(idx_to_omit > 0)) {
                node_list <- node_list[-idx_to_omit]
                edge_list <- edge_list[-idx_to_omit]
                idx_to_omit <- removeLoops(node_list, route)
              }
              
              # write to output row
              route$network_nodes <- toString(node_list)
              route$network_edges <- toString(edge_list)
              
            }
            
            # ggplot() +
            #   geom_sf(data = links %>%
            #                     filter(link_id %in% (route$network_edges  %>%
            #                              strsplit(., ", ") %>%
            #                              unlist() %>%
            #                              as.numeric())),
            #                   colour = "blue") +
            #   geom_sf(data = vertices, colour = "red")

            # ggplot() +
            #   geom_sf(data = links[shortest.edges,])
            
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
routes_networked <- st_read("./data/routes_networked.sqlite", layer = "survey")

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
routes_networked <- st_read("./data/routes_networked.sqlite", layer = "survey")

# setup for parallel processing - detect available cores and create cluster
cores <- detectCores()
cluster <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cluster)

# report
print(paste(Sys.time(), "| Printing maps for", nrow(routes_networked), 
            "network maps; parallel processing with", cores, "cores"))

# set up progress reporting
# https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
pb <- txtProgressBar(max = nrow(routes_networked), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# create and save map for each route 
output <- 
  foreach(i = 1:nrow(routes_networked),
          # foreach(i = 1:8,
          .packages = c("dplyr", "sf", "stringr", "ggplot2", "ggspatial"),
          .options.snow = opts) %dopar% {
            
            # selected route
            route <- routes_networked[i,]
            
            map.title <- paste0("Map no ", i, ",  routeID ", route$routeid)
            map.filename <- paste0("map_", i, "_routeID_", route$routeid)
            
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


# 6 Routes passing through stressful junctions  ----
# -----------------------------------------------------------------------------#
# load network nodes
nodes <- st_read(networkFile, layer = nodeLayer)

# load routes
routes <- st_read("./data/routes_networked.sqlite", layer = "survey")

# read in  stressful junction file, and convert to sf object
stressJct<- read.csv(stressJctFile) %>%
  rename(WKT = WKT._stressJct) %>%
  st_as_sf(., wkt = "WKT", crs = 4326) %>%
  st_transform(st_crs(nodes)) 

# join details of nearest node used by the survey participant
for (i in 1:nrow(stressJct)) {
  
  # find the nodes used by the survey participant
  respondent <- stressJct$Respondent.ID[i]
  
  respondent.node.ids <- routes %>%
    filter(respondent.id == respondent) %>%
    .$network_nodes %>%
    str_split(", ") %>%
    unlist() %>%
    as.numeric() %>%
    unique()
  
  respondent.nodes <- nodes %>%
    filter(id %in% respondent.node.ids)
  
  # find the nearest respondent node to the junction
  nearest.node.id <- 
    respondent.nodes[st_nearest_feature(stressJct[i, ], respondent.nodes), ] %>%
    .$id
  
  # build vector of routes using the node, starting with empty vector
  routes.using.node <- c()
  
  # loop through the routes
  for (j in 1:nrow(routes)) {
    # extract vector of network nodes
    network_nodes <- routes$network_nodes[j] %>%
      str_split(", ") %>%
      unlist() %>%
      as.numeric()
    
    # if the route uses the node, add it to the vector
    if (nearest.node.id %in% network_nodes) {
      routes.using.node <- c(routes.using.node, routes$routeid[j])
    }
  }
  
  # add the nearest node vector to the stressful junctions dataframe
  stressJct[i, "nearest_node"] <- nearest.node.id
  stressJct[i, "route_ids"] <- toString(routes.using.node)
  
}

# write output
st_write(stressJct, "./data/stressJct.sqlite", delete_layer = TRUE)


# 7 Favourite spot land types  ----
# -----------------------------------------------------------------------------#
# read in nodes (just used for CRS)
nodes <- st_read(networkFile, layer = nodeLayer)

# read in meshblocks
MB <- read_zipped_GIS(zipfile = "./data/MB_2021_AUST_SHP_GDA2020.zip") %>%
  st_transform(st_crs(nodes))

# read in  favourite junction file
favSpotBase <- read.csv(favSpotFile) %>%
  # add a unique id for later joining
  mutate(id = row_number())

# convert to sf object and join meshblock land category
favSpotSF <- favSpotBase %>%
  # transform to sf in correct CRS
  rename(WKT = WKT_FavSpot) %>%
  st_as_sf(., wkt = "WKT", crs = 4326) %>%
  st_transform(st_crs(nodes)) %>%
  
  # add meshblock land category to favourite spot file
  st_join(MB %>% dplyr::select(land_type = MB_CAT21), join = st_intersects)

# prepare output - original file with meshblock land category joined
favSpotOutput <- favSpotBase %>%
  left_join(favSpotSF %>% 
              st_drop_geometry() %>%
              dplyr::select(id, land_type), 
            by = "id") %>%
  dplyr::select(-id)

# write output
write.csv(favSpotOutput, "./data/favSpotLandType.csv", row.names = FALSE)


# 8 Expanded networked survey routes  ----
# -----------------------------------------------------------------------------#
# table which is an expanded version of 'routes_networked', with one row
# per link in each trip, and network details attached

# read in routes_networked and network links
routes_networked <- st_read("./data/routes_networked.sqlite", layer = "survey")
links <- st_read(networkFile, layer = linkLayer)

# expand routes_network by adding details listed below from links
routes_networked_expanded <- 
  expandRoutes(routes_networked %>%
                 st_drop_geometry() %>%
                 dplyr::select(routeid, respondent.id, destination, network_edges),
               links %>% 
                 st_drop_geometry() %>%
                 dplyr::select(link_id, length, highway, cycleway, freespeed,
                               surface, slope_pct, ndvi, ndvi_md, ndvi_75, ndvi_90,
                               adt, lvl_traf_stress))


# write output
write.csv(routes_networked_expanded, "./data/routes_networked_expanded.csv",
          row.names = FALSE)


# 9 Equivalent shortest paths  ----
# -----------------------------------------------------------------------------#
## 9.1 Shortest paths ----
## ------------------------------------#
# read in routes_networked and network
routes_networked <- st_read("./data/routes_networked.sqlite", layer = "survey") %>%
  # remove geometry (which is the geometry drawn by the participant)
  st_drop_geometry %>%
  # keep required fields
  dplyr::select(routeid, respondent.id, destination, network_nodes)

# RUN SECTION 1, to read in network and create cyclable network graph ('graph')

# setup for parallel processing - detect available cores and create cluster
cores <- detectCores()
cluster <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cluster)

# report
print(paste(Sys.time(), "| Finding shortest equivalent routes for", 
            nrow(routes_networked), 
            "survey paths; parallel processing with", cores, "cores"))

# Set up progress reporting (see notes in section 2.3.3 for explanations)
pb <- txtProgressBar(max = nrow(routes_networked), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# find shortest routes corresponding to survey paths
shortest.routes <- 
  foreach(i = 1:nrow(routes_networked),
  # foreach(i = 1:10,
          .packages = c("dplyr", "sf", "igraph", "stringr"), 
          .combine = rbind,
          .options.snow = opts) %dopar% {

            # begin with row from routes_networked                
            output.row <- routes_networked[i, ]
            
            # start and end nodes
            network_nodes <- str_split(output.row$network_nodes, ", ") %>% 
              unlist()
            start.node <- network_nodes[1]
            end.node <- network_nodes[length(network_nodes)]

            # shortest route
            shortest <- shortest_paths(graph, 
                                       from = start.node,
                                       to = end.node,
                                       mode = "out",
                                       output = "epath")
            
            # complete details if shortest route exists, otherwise NA
            if (length(shortest$epath[[1]]) > 0) {
              shortest.link.ids <- edge_attr(graph, "link_id", shortest$epath[[1]])
              shortest.links <- cyclable.links %>%
                filter(link_id %in% shortest.link.ids)
              shortest.link.ids <- toString(shortest.link.ids)
 
            } else {
              shortest.link.ids <- NA

            }
            
            # add shortest route details to output row
            output.row <- c(output.row %>%
                              dplyr::select(-network_nodes), 
                            network_edges = shortest.link.ids)

            # convert the output row (which has become a list) to a dataframe
            output.row <- as.data.frame(output.row)
            
            # Return the output
            return(output.row)
            
          }                                                    

# end parallel processing
close(pb)
stopCluster(cluster)


## 9.2 Shortest paths for display ----
## ------------------------------------#
# empty sf objects
shortest.routes.paths <- st_sf(geometry = st_sfc(), 
                                data.frame(id = numeric(), 
                                           stringsAsFactors = FALSE),
                                crs = st_crs(links))


# create line for each route
for (i in 1:nrow(shortest.routes)) {
  link.ids <- shortest.routes$network_edges[i] %>%
    strsplit(., ", ") %>%
    unlist() %>%
    as.numeric()
  
  route_links <- links %>% 
    filter(link_id %in% link.ids) %>%
    st_union()
  
  if (length(route_links) > 0) {
    shortest.routes.paths <- bind_rows(shortest.routes.paths, 
                                        st_sf(geometry = st_sfc(route_links), 
                                              id = i,
                                              routeid = shortest.routes$routeid[i]))
  } 
}

# write output
st_write(shortest.routes.paths, "./data/routes_networked.sqlite", 
         layer = "shortest", delete_layer = TRUE)


## 9.3 Expanded with network details ----
## ------------------------------------#
# expand routes_network by adding details listed below from links
routes_shortest_expanded <- 
  expandRoutes(shortest.routes %>%
                 st_drop_geometry() %>%
                 dplyr::select(routeid, respondent.id, destination, network_edges),
               links %>% 
                 st_drop_geometry() %>%
                 dplyr::select(link_id, length, highway, cycleway, freespeed,
                               surface, slope_pct, ndvi, ndvi_md, ndvi_75, ndvi_90,
                               adt, lvl_traf_stress))

# write output
write.csv(routes_shortest_expanded, "./data/routes_shortest_expanded.csv",
          row.names = FALSE)
