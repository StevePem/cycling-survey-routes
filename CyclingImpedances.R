# Using a network created by 'NetworkGenerator.R', add impedances for 
# calculating cycling accessibility

# Assumes input network is a one-way network that includes elevation, 
# and that one-way daily traffic volumes are available

addImpedances <- function() {
    
  # Parameters -----------------------------------------------------------------
  # Input network, to which cycling impedances are to be added, with layer names
  # Nodes - from base network
  input.network <- "./data/melbourne_network_survey_before_traffic/network.sqlite" 
  input.node.layer <- "nodes"

  # Links - from simulation, being base layer plus a 'total_vol' column containing 1-way daily traffic
  traffic.file <- "./data/links_base_with_traffic.sqlite" 
  traffic.link.layer <- "cars_aht" 
  
  # Traffic multiplier - where volumes are for a sample of traffic only (eg
  # multiplier of 20 if the volumes are a 5% sample; use 1 if full volumes)
  traffic.multiplier <- 10

  # Output location - same directory as input.network
  output.location <- "./data/network_weighted.sqlite"
  
  
  # Packages and functions -----------------------------------------------------
  library(dplyr)
  library(sf)
  library(fs)
  library(osmextract)
  library(stringr)
  
  dir_walk(path="./functions/",source, recurse=T, type = "file")
  
  
  # Load input network and traffic file ----------------------------------------
  input.nodes <- st_read(input.network, layer = input.node.layer)  
  input.links <- st_read(traffic.file, layer = traffic.link.layer) %>%
    mutate(ADT = total_vol * traffic.multiplier)
  networkTraffic <- list(input.nodes, input.links)
  
  
  # Add LTS and its impedance --------------------------------------------------
  echo("Adding LTS and its impedance\n")
  networkLTS <- addLTS(networkTraffic[[1]], networkTraffic[[2]])
  
  
  # Add slope impedance --------------------------------------------------------
  echo("Adding slope impedance")
  networkSlope <- addSlopeImped(networkLTS[[1]], networkLTS[[2]])
  
  
  # Add surface impedance --------------------------------------------------------
  echo("Adding surface impedance")
  networkSurf <- addSurfImped(networkSlope[[1]], networkSlope[[2]])
  
  
  # Calculate total weight -----------------------------------------------------
  echo("Calculating cycling weight")
  networkWeighted <- 
    list(networkSurf[[1]],
         networkSurf[[2]] %>%
           mutate(cycle.weight = length + LTS_imped + slope_imped + surf_imped))
  
  
  # write output ---------------------------------------------------------------
  st_write(networkWeighted[[1]], output.location, layer = "nodes", delete_layer = T)
  st_write(networkWeighted[[2]], output.location, layer = "links", delete_layer = T)
  
}


