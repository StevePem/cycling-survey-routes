# function to expand routes, containing 'network_edges' in the form of a 
# vector of link ids, into a dataframe with one row per link and 
# network link attributes attached

expandRoutes <- function(input.routes, input.links) {
  
  # input.routes = routes_networked %>%
  #   st_drop_geometry() %>%
  #   dplyr::select(routeid, respondent.id, destination, network_edges)
  # input.links = links %>% 
  #   st_drop_geometry() %>%
  #   dplyr::select(link_id, length, highway, cycleway,
  #                 surface, slope_pct, ndvi)
  
  # empty dataframe to hold output
  routes_expanded_base <- data.frame()
  
  # loop through rows, converting to multiple rows, one for each leg
  for (i in 1:nrow(input.routes)) {
    # individual row
    row <- input.routes[i,]
    
    # vector of network edges (assumed to be in 'network_edges' field)
    network_edges <- str_split(row$network_edges, ", ") %>% 
      unlist() #%>% 
      # as.numeric()
    
    # repeat the row multiple times, once for each edge, with leg no and link id
    df <- row %>%
      # exclude 'network_edges' field
      dplyr::select(-network_edges) %>%
      # repeat the row by the number of network edges
      slice(rep(1, length(network_edges))) %>%
      # bind in leg no's and link id's
      cbind(leg = seq(1:length(network_edges)),
            link_id = network_edges)
    
    routes_expanded_base <- bind_rows(routes_expanded_base, df)
    
  }
  
  # join details from links
  routes_expanded <- routes_expanded_base %>%
    left_join(input.links,
              by =  "link_id")
  
  return(routes_expanded)
  
}
