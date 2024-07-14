library(tidyverse)
library(ggplot2)
library(ggspatial)
library(sf)
library(vroom)
library(fs)

# Same Network that was used for the simulation 
network_links <- st_read("./melbourne_network_base.sqlite", 
                        layer = "links", quiet=T)

trip_links <- vroom("./trip_links.txt")

vehicle_trips <- read_csv("./vehicle_trip.txt")

vehicle_lists <- vehicle_trips %>% 
  distinct(person,vehicle,networkMode) 

trip_links_with_vehicle <- trip_links %>% 
  left_join(vehicle_lists, by = "vehicle") %>% 
  # Filtering to non-pt vehicles 
  filter(!(grepl("bus", vehicle) | grepl("train", vehicle) | grepl("tram", vehicle)))

exit_links_with_time <- trip_links_with_vehicle %>% 
  filter(type=="left_link") %>% 
  dplyr::select(-type) %>% 
  mutate(left_hour=  floor(time / 3600)) %>% 
  mutate(left_hms=paste(sprintf("%02d",left_hour),
                       sprintf("%02d", floor(time %% 3600 / 60)),
                       sprintf("%02d", floor(time %% 60 )),
                       sep=":"))

exit_links_with_time %>% 
  group_by(left_hour) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=left_hour, y=n)) +
  geom_col() +
  labs(title = "Aggregated exiting link event count by the hour of the day") +
  xlab("Hour of the day") +
  ylab("Link exit even count")

car_exit_link_hourly <- exit_links_with_time %>%
  filter(networkMode=="car") %>% # filter to car trips only
  mutate(id=as.character(link)) %>% 
  group_by(id, left_hour) %>% 
  summarise(hourly_vol=n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols=id, names_from=left_hour, values_from=hourly_vol) %>% 
  mutate(total_vol = rowSums(across(where(is.numeric)),na.rm = T))

links_with_aht_car <- network_links %>% 
  mutate(link_id=as.character(link_id)) %>% 
  left_join(car_exit_link_hourly, by = c("link_id"="id"))   

st_write(links_with_aht_car, 
         "./links_base_with_traffic.sqlite",
         layer="cars_aht",
         delete_layer = T)

links_with_aht_car %>% 
  filter(total_vol>0) %>% 
  ggplot() +
  geom_sf(aes(fill=total_vol)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .8) 
