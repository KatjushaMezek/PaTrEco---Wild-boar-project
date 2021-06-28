#### WILDSCHWEIN SCHRECK REPORT####

###DATA IMPORT & LIBRARIES 
## install.packages("devtools") # <- if you havent installed devtools already
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")

## Libraries
library(ComputationalMovementAnalysisData)
library(readr)        
library(dplyr)        
library(ggplot2)      
library(sf)           
library(terra)       
library(tmap)         
library(mapproj)
library(rgdal)
library(lubridate)
library(sp)
library(leaflet)
library(sfheaders)
library(tidyverse)
library(leaflet.extras)
library(ggpubr)

## Import the data
wildschwein = wildschwein_BE
schreck_agenda = schreck_agenda
schreck_loc =schreck_locations

### DATA TIDYING AND PREPARATION

## key join the schreck datased (key = ID)
schreck_all =left_join(schreck_agenda,schreck_locations, by="id")

## Convert decimal degrees E,N to cartesian coordinates X,Y & rename columns
# Schreck dataset
schreck_all_sf = st_as_sf(schreck_all, coords=c("lon","lat"), crs=4326, remove=FALSE)
schreck_all_ch = st_transform(schreck_all_sf, 2056)
transformed = sf::st_coordinates(schreck_all_ch,geometry)

schreck_all_ch = schreck_all_ch %>% cbind(transformed)%>% 
  dplyr::rename (speaker_X =X ,speaker_Y=Y, speaker_lat = lat, speaker_lon = lon) 

# Wildschwein dataset
# Annotate the wildboar data with the vegetation indexand crop data
veg_height <- rast("vegetationshoehe_LFI.tif")
crop_fanel <- read_sf("Feldaufnahmen_Fanel.gpkg")

wildschwein_sf = st_as_sf(wildschwein, coords=c("E","N"), crs=2056, remove=FALSE)
wildschwein_sf = st_join(wildschwein_sf,crop_fanel)
veg_height_df <- terra::extract(veg_height,st_coordinates(wildschwein_sf))
wildschwein_sf <- cbind(wildschwein_sf,veg_height_df)
wildschwein_sf = st_transform(wildschwein_sf, 4326)
transformed = sf::st_coordinates(wildschwein_sf,geometry)


wildschwein = wildschwein_sf %>% cbind(transformed)%>% 
  dplyr::rename(wildschwein_lat = Y, wildschwein_lon=X, wildschwein_X=E, wildschwein_Y=N) 


#EXPLANATION: first the schreck_agenda and schreck_locations were joined with the ID key. 
# then the joined schreck dataset was turned to an sf object and transformed to crs=2056,
# giving us the cartesian coordinates X and y. The wildschwein dataset was transformed to
# crs=4326, giving us decimal degrees coordinates E, N. In both datasets the E,N and X, Y 
# were renamed (lat/lon are decimal degrees and X/Y are cartesian coordinates)
# this transformation was done for later analyses and visualisations
# additionally the wildboar dataset was annotatd with the vegetation index and crop data for further analyses


#################################################################################

### DATA EXPLORATION
## Wildschwein tracking times
wildschwein %>%
  ggplot(aes(DatetimeUTC, TierName)) + geom_line()

## Schreck speaker start times
schreck_all %>%
  ggplot(aes(datum_on, id)) + geom_point()

# We excluded the year 2014 from analyses since almost no wild boar were tracked then
# The year 2017 was also excluded from analyses since no wilds boar were tracked then

## A look into the wild boar and speaker positions
# Year 2015
wildschwein_2015 = wildschwein %>% 
  mutate(
    DatetimeUTC_posix = as.POSIXct(DatetimeUTC, format = "%Y-%m-%d %H:%M:%S",tz="UTC")) %>%
  filter(DatetimeUTC_posix >= "2015-01-01" & DatetimeUTC_posix <= "2015-12-31")

schreck_all_ch_2015 = schreck_all_ch %>% 
  mutate(
    DatetimeUTC_posix = as.POSIXct(datum_on, format = "%Y-%m-%d",tz="UTC")) %>%
  filter(datum_on >= "2015-01-01" & datum_on <= "2015-12-31")

ggplot() +
  geom_point(wildschwein_2015, mapping=aes(wildschwein_X,wildschwein_Y)) +
  theme(legend.position = "none") +
  geom_point(schreck_all_ch_2015, mapping=aes(speaker_X,speaker_Y,col="red"))+
  theme_bw() +
  labs(x="E", y="N", title = "Positions of the wild boar and speakers in the year 2015")

# Year 2016
wildschwein_2016 = wildschwein %>% 
  mutate(
    DatetimeUTC_posix = as.POSIXct(DatetimeUTC, format = "%Y-%m-%d %H:%M:%S",tz="UTC")) %>%
  filter(DatetimeUTC_posix >= "2016-01-01" & DatetimeUTC_posix <= "2016-12-31")

schreck_all_ch_2016 = schreck_all_ch %>% 
  mutate(
    DatetimeUTC_posix = as.POSIXct(datum_on, format = "%Y-%m-%d",tz="UTC")) %>%
  filter(datum_on >= "2016-01-01" & datum_on <= "2016-12-31")

ggplot() +
  geom_point(wildschwein_2016, mapping=aes(wildschwein_X,wildschwein_Y)) +
  theme(legend.position = "none") +
  geom_point(schreck_all_ch_2016, mapping=aes(speaker_X,speaker_Y,col=""))+
  theme_bw() +
  labs(x="E", y="N", title = "Positions of the wild boar and speakers in the year 2016")

# we see from the plots that some speakers are not relevant since no wild boar are 
# nearby - we need to filter out the speakers where there are no wild boar


#################################################################################

### DATA PREPROCESSING

## Filter the speakers based on the maximum wildschwein longitude and lattitude
# (every speaker > max wildschwein lon/lat is excluded). Additionally "schreck_id"
# was added to the dataset, which gives each row (speaker) its own id number

# Year 2015
schreck_2015_filtered = schreck_all_ch_2015 %>%
  filter(speaker_X <= max(wildschwein_2015$wildschwein_X), speaker_Y <= max(wildschwein_2015$wildschwein_Y)) 

schreck_2015_filtered$schreck_id = seq.int(nrow(schreck_2015_filtered))
 

ggplot() + geom_point(wildschwein_2015, mapping=aes(wildschwein_X,wildschwein_Y)) +
  theme(legend.position = "bottom") + geom_point(schreck_2015_filtered,size=3, mapping=aes(speaker_X,speaker_Y,col=as.factor(id))) +
  labs(col="Speaker ID") + theme_bw() +
  labs(x="E", y="N", title = "Positions of the wild boar and filtered speakers in the year 2015")

# Year 2016
schreck_2016_filtered = schreck_all_ch_2016 %>%
  filter(speaker_X <= max(wildschwein_2016$wildschwein_X), speaker_Y <= max(wildschwein_2016$wildschwein_Y)) 
    
schreck_2016_filtered$schreck_id = seq.int(nrow(schreck_2016_filtered))


ggplot() + geom_point(wildschwein_2016, mapping=aes(wildschwein_X,wildschwein_Y)) +
  theme(legend.position = "bottom") +
  geom_point(schreck_2016_filtered, size=3, mapping=aes(speaker_X,speaker_Y,col=as.factor(id))) +
  labs(col="Speaker ID") + theme_bw() +
  labs(x="E", y="N", title = "Positions of the wild boar and filtered speakers in the year 2016")

## Pick speakers for analysis
# Locations WSS_2015_01,wss_2016_01 and were picked (try also wss_2016_05)

## round the wildboar times to 15 minutes
# Year 2015
wildschwein_2015 = wildschwein_2015 %>%
  mutate(
    Datetimeround= lubridate::round_date(DatetimeUTC_posix,"15 minutes")
  )
#Year 2016
wildschwein_2016 = wildschwein_2016 %>%
  mutate(Datetimeround= lubridate::round_date(DatetimeUTC_posix,"15 minutes")
  )
    
## Make time windows (7 days before the start date of the speaker, the day the speaker starts and 7 days after the speaker starts)
# Speaker 1 (WSS_2015_01,standard, schreck_id = 1, start date = 2015-05-20) 
wildschwein_2015_loc1 = wildschwein_2015 %>%
  mutate(
    TimeWindow = ifelse(Datetimeround >= "2015-05-13 00:00:00" & Datetimeround < "2015-05-20 00:00:00","1w before", 
                        ifelse(Datetimeround >= "2015-05-20 00:00:00" & Datetimeround <= "2015-05-20 23:59:59" , "start",
                               ifelse(Datetimeround > "2015-05-20 23:59:59" & Datetimeround <= "2015-05-27 23:59:59", "1w after","other"))),
    schreck_id = rep(1)    #an additional column schreck_id was made, in order to key-join the wildschwein dataset with the schreck dataset
  ) %>%
  filter(TimeWindow != "other")    

# Speaker 2 (WSS_2016_01, standard, schreck_id = 1, start date = 2016-04-04)
wildschwein_2016_loc2 = wildschwein_2016 %>%
  mutate(
    TimeWindow = ifelse(Datetimeround >= "2016-03-28 00:00:00" & Datetimeround < "2016-04-04 00:00:00","1w before", 
                        ifelse(Datetimeround >= "2016-04-04 00:00:00" & Datetimeround <= "2016-04-04 23:59:59" , "start",
                               ifelse(Datetimeround > "2016-04-04 23:59:59" & Datetimeround <= "2016-04-11 23:59:59", "1w after","other"))),
    schreck_id = rep(1)
  ) %>%
  filter(TimeWindow != "other") 

## join the wildschwein and schreck datasets ( key= schreck id)
wildschwein_2015_loc1_join = left_join(as.data.frame(wildschwein_2015_loc1), as.data.frame(schreck_2015_filtered), by= "schreck_id")
wildschwein_2016_loc2_join = left_join(as.data.frame(wildschwein_2016_loc2),as.data.frame(schreck_2016_filtered), by= "schreck_id")

## Calculate euclidean distance between the wildschwein locations and the speaker location
## Find wild boar that were within 500m of the speaker at the start
# Function for euclidean distance
euc.dist = function(x1,y1,x2,y2) {
  (sqrt((lead(x2)-x1)^2+(lead(y2)-y1)^2))
}

# loc 1
wildschwein_2015_loc1_join %>%
  mutate(
    distance = euc.dist(wildschwein_X,wildschwein_Y,speaker_X,speaker_Y),
    TierName = as.factor(TierName)
  ) %>%
  filter(TimeWindow == "start") %>%
  filter(distance <= 500) %>%
  .$TierName %>% unique()
# Caroline, Claude and Olga were within 500m of the speaker WSS_2015_01 (standard) when it was started

# loc 2
wildschwein_2016_loc2_join %>%
  mutate(
    distance = euc.dist(wildschwein_X,wildschwein_Y,speaker_X,speaker_Y),
    TierName = as.factor(TierName)
  ) %>%
  filter(TimeWindow == "start") %>%
  filter(distance <= 500) %>%
  .$TierName %>% unique()

# Caroline, Miriam and Frida were within 500m of the speaker WSS_2016_01 (standard) when it was started

#################################################################################
### VISUALIZATIONS

## Line plots of the wildboar trajectories
# the trajectory granulation was reduced to every 4 rows, using slice()
# loc 1
wildschwein_2015_loc1_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    fTierName= as.factor(TierName),
    fTimeWindow = as.factor(TimeWindow))%>%
  filter(fTierName %in% c("Caroline", "Claude", "Olga")) %>%
  ggplot(aes(x=wildschwein_X, y=wildschwein_Y, col=TimeWindow)) + geom_path() + geom_point()  + 
  geom_point(mapping=aes(speaker_X,speaker_Y, fill="id"), shape = 17, col="black", size =3)+
  labs(color ="Time windows", fill ="Speaker") + 
  scale_fill_discrete(labels= "WSS_2015_01") + 
  scale_color_discrete(limits= c("1w before", "start", "1w after"))+
  facet_grid(~fTierName) + theme_bw() +
  labs(x="E", y="N", title = "Wild boar trajectories during different time windows of the speaker WSS_2015_01")


# loc 2
wildschwein_2016_loc2_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    fTierName= as.factor(TierName),
    fTimeWindow = as.factor(TimeWindow))%>%
  filter(fTierName %in% c("Caroline", "Miriam", "Frida")) %>%
  ggplot(aes(x=wildschwein_X, y=wildschwein_Y, col=TimeWindow)) + geom_path() + geom_point()  + 
  geom_point(mapping=aes(speaker_X,speaker_Y, fill="id"), shape = 17, col="black", size =3)+
  labs(color ="Time windows", fill ="Speaker") + 
  scale_fill_discrete(labels= "WSS_2016_01") + 
  scale_color_discrete(limits= c("1w before", "start", "1w after"))+
  facet_grid(~fTierName) + theme_bw() +
  labs(x="E", y="N", title = "Wild boar trajectories during different time windows of the speaker WSS_2016_01")

#EXPLANATION: Trajectory plots indicate that the wild boar do move away from the speakers after
# the start of the operation, but after 1 week, most of the wildboar return on the affected fields


## Heat maps

## loc 1
df1=wildschwein_2015_loc1_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    fTierName= as.factor(TierName),
    fTimeWindow = as.factor(TimeWindow)) %>%
  filter(fTierName %in% c("Caroline", "Claude", "Olga"))

groups = as.character(unique(df1$TimeWindow))

leaflet() %>%
  addTiles(group="map") %>%
  addHeatmap(data=df1[df1$TimeWindow=="1w before",], group = "1w before", lng=~wildschwein_lon, lat=~wildschwein_lat, max=1, blur = 60)%>%
  addHeatmap(data=df1[df1$TimeWindow=="start",],group = "start", lng=~wildschwein_lon, lat=~wildschwein_lat, max=2, blur = 20)%>%
  addHeatmap(data=df1[df1$TimeWindow=="1w after",],group = "1w after", lng=~wildschwein_lon, lat=~wildschwein_lat, max=3, blur = 45)%>%
  addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = FALSE))%>%
  addMarkers(data=schreck_2015_filtered[schreck_2015_filtered$schreck_id==1,], ~speaker_lon, ~speaker_lat, popup = ~as.character(1), label = "WSS_2015_01")%>%
  setView(7.055569, 46.99734, zoom=16)

# loc 2
df2 = wildschwein_2016_loc2_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    fTierName= as.factor(TierName),
    fTimeWindow = as.factor(TimeWindow)) %>%
  filter(fTierName %in% c("Caroline", "Miriam", "Frida"))

leaflet() %>%
  addTiles(group="map") %>%
  addHeatmap(data=df2[df2$TimeWindow=="1w before",], group = "1w before", lng=~wildschwein_lon, lat=~wildschwein_lat, max=1, blur = 60)%>%
  addHeatmap(data=df2[df2$TimeWindow=="start",],group = "start", lng=~wildschwein_lon, lat=~wildschwein_lat, max=2, blur = 20)%>%
  addHeatmap(data=df2[df2$TimeWindow=="1w after",],group = "1w after", lng=~wildschwein_lon, lat=~wildschwein_lat, max=3, blur = 45)%>%
  addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = FALSE))%>%
  addMarkers(data=schreck_2016_filtered[schreck_2016_filtered$schreck_id==1,], ~speaker_lon, ~speaker_lat, popup = ~as.character(1), label = "WSS_2016_01")%>%
  setView(7.053519, 46.97996, zoom=18)

# EXPLANATION: we see from the heat maps that the wild boar spend less time on the fields with the 
# speakers (lower density of wildboar shown on the heatmaps)

################################################################################
### GIVING SEMANTIC MEANING TO THE DATA/ QUANTIFICATION OF THE SCARE OFF MEASURE EFFECTS

# Three methods were tested to determine the effects of the speaker scare off measure
# on the wildboar:

## METHOD 1 - Analyzing the distances between the wildboar and speakers
# The euclidean distance between the wildboar and speakers was compared between the different
# time windows. The data was not normally distributed, therefore  Kruskal-Wallis with Wilcoxon`s rank sum post hoc test
# was used to determine if there are significant differences between the different time-windows

# loc 1
# normality test
loc1_norm=wildschwein_2015_loc1_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    distance = euc.dist(wildschwein_X,wildschwein_Y,speaker_X,speaker_Y),
    fTierName= as.factor(TierName),
    fTimeWindow = as.factor(TimeWindow)) %>%
  filter(fTierName %in% c("Caroline", "Claude", "Olga")) 


ggqqplot(loc1_norm$distance)
shapiro.test(loc1_norm$distance) 

# The data is not normally distributed based on the qqplot and Shapiro-Wilk normality test (p<2.2e-16)

# Kruskal-Wallis with Wilcoxon`s rank sum post hoc test
my_comparisons <- list(c("1w after", "start"), c("1w before", "start"),c("1w after", "1w before") )

wildschwein_2015_loc1_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    distance = euc.dist(wildschwein_X,wildschwein_Y,speaker_X,speaker_Y),
    fTierName= as.factor(TierName),
    fTimeWindow = factor(TimeWindow, levels = c("1w before", "start", "1w after"))) %>%
  filter(fTierName %in% c("Caroline", "Claude", "Olga")) %>%
  ggboxplot("fTimeWindow", "distance", color="fTimeWindow", palette="jco")+
  stat_compare_means(label.x = 1.5, label.y = 2000) +
  stat_compare_means(comparisons = my_comparisons) + theme_bw() +
  labs(x="Time windows", y="Distance", color="Time window", title = "Distance between the wild boar and speaker WSS_2015_01 during different time windows")

# For loc1, the distance after 1 week of speaker operation is significantly 
# higher than the distance 1 week before or at the start of the speaker operation
# indicating that the wild boar moved further away from the speaker after its start

# loc 2
loc2_norm=wildschwein_2016_loc2_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    distance = euc.dist(wildschwein_X,wildschwein_Y,speaker_X,speaker_Y),
    fTierName= as.factor(TierName),
    fTimeWindow = as.factor(TimeWindow)) %>%
  filter(fTierName %in% c("Caroline", "Miriam", "Frida")) 


ggqqplot(loc2_norm$distance)
shapiro.test(loc2_norm$distance) 

# The data is not normally distributed based on the qqplot and Shapiro-Wilk normality test (p<2.2e-16)

# Kruskal-Wallis with Wilcoxon`s rank sum post hoc test
wildschwein_2016_loc2_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    distance = euc.dist(wildschwein_X,wildschwein_Y,speaker_X,speaker_Y),
    fTierName= as.factor(TierName),
    fTimeWindow = factor(TimeWindow, levels = c("1w before", "start", "1w after"))) %>%
  filter(fTierName %in% c("Caroline", "Miriam", "Frida")) %>%
  ggboxplot("fTimeWindow", "distance", color="fTimeWindow", palette="jco")+
  stat_compare_means(label.x = 1.5, label.y = 2000) +
  stat_compare_means(comparisons = my_comparisons) + theme_bw() +
  labs(x="Time windows", y="Distance", color="Time window", title = "Distance between the wild boar and speaker WSS_2016_01 during different time windows")

# For loc2, the distance after 1 week of speaker operation is significantly 
# higher than the distance 1 week before or at the start of the speaker operation
# indicating that the wild boar moved further away from the speaker after its start

## METHOD 2 - quantify the percentage of wild boar in forests in the different time windows of speaker operation using a vegetation index
# The wild boar was annotated with a vegetation height index raster data. The data  "Vegetationindex" was
# prepared for us in the course module which is provided bythe Swiss National Forest Inventory (NFI). This dataset
# contains high resolution information (1x1 Meter) on the vegetation height, which is determined from the difference between the digital
# surface models DSM and the digital terrain model by swisstopo (swissAlti3D).
# It is expected that the vegetation index will get higher after the start of speaker operation, 
# because the wildboar would hide in the forests (higher vegetation index)

## The vegetation index
tm_shape(veg_height) + 
  tm_raster(palette = "cividis",style = "cont", legend.is.portrait = FALSE) +
  tm_layout(legend.outside = TRUE,legend.outside.position = "bottom", frame = FALSE)

# loc 1 - plot
wildschwein_2015_loc1_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    distance = euc.dist(wildschwein_X,wildschwein_Y,speaker_X,speaker_Y),
    fTierName= as.factor(TierName),
    fTimeWindow = factor(TimeWindow, levels = c("1w before", "start", "1w after"))) %>%
  filter(fTierName %in% c("Caroline", "Claude", "Olga")) %>%
  ggboxplot("fTimeWindow", "vegetationshoehe_LFI", color="fTimeWindow", palette="jco")+
  stat_compare_means(label.x = 1.5, label.y = 35) +
  stat_compare_means(comparisons = my_comparisons) + theme_bw() +
  labs(x="Time windows", y="Vegetation index LFI", color="Time window", title = "The vegetation index of the wild boar trajectories during different time windows of the speaker WSS_2015_01")


# loc 2 - plot
wildschwein_2016_loc2_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    distance = euc.dist(wildschwein_X,wildschwein_Y,speaker_X,speaker_Y),
    fTierName= as.factor(TierName),
    fTimeWindow = factor(TimeWindow, levels = c("1w before", "start", "1w after"))) %>%
  filter(fTierName %in% c("Caroline", "Miriam", "Frida")) %>%
  ggboxplot("fTimeWindow", "vegetationshoehe_LFI", color="fTimeWindow", palette="jco")+
  stat_compare_means(label.x = 1.5, label.y = 50) +
  stat_compare_means(comparisons = my_comparisons) + theme_bw() +
  labs(x="Time windows", y="Vegetation index LFI", color="Time window", title = "The vegetation index of the wild boar trajectories during different time windows of the speaker WSS_2016_01")

# RESULTS: The vegetation index seems to get lower after 1 week of speaker operation
# This is not in line with our hypothesis that the index will get higher. The explanation for this can be that the
# method is not optimal for this location, since the marshy areas show a similar vegetation index as the fields,
# making interpretation purely by the vegetation index hard

## METHOD 3 - Quantify the percentage of wild boar in forests in the different time windows of speaker operation using field crop information
# The wild boar was annotated with crop raster data. The data  "Feldaufnahmen_Fanel" was
# prepared for us in the course module and contains information on the crops grown in the Fanel area.
# It is expected that the wildboar will visit the fields with the speakers less often after they are turned on

# the Feldaufnahmen_Fanel dataset
head(crop_fanel)
summary(crop_fanel)
unique(crop_fanel$Frucht)
st_crs(crop_fanel)
ggplot(crop_fanel) +
  geom_sf(aes(fill = Frucht))

# loc 1 - plot
wildschwein_2015_loc1_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    Frucht = ifelse(is.na(Frucht),"other",Frucht),
    fTierName= as.factor(TierName),
    fTimeWindow = factor(TimeWindow, levels = c("1w before", "start", "1w after"))) %>%
  filter(fTierName %in% c("Caroline", "Claude", "Olga")) %>%
  group_by(fTimeWindow,Frucht, fTierName) %>%
  count() %>%
  ungroup() %>%
  group_by(fTierName, fTimeWindow) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Frucht = fct_reorder(Frucht, n,sum, desc = TRUE)
  ) %>%
  ggplot(aes(fTimeWindow, perc, fill=Frucht)) + geom_col() + 
  scale_y_continuous(name = "Percentage", labels = scales::percent_format()) +
  scale_x_discrete(name = "Time windows") +
  facet_wrap(~fTierName) +
  theme_light() +
  labs(title = "Percentages of visited crops duting different time windows for speaker WSS_2015_01")

wildschwein_2015_loc1_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    Frucht = ifelse(is.na(Frucht),"other",Frucht),
    fTierName= as.factor(TierName),
    fTimeWindow = factor(TimeWindow, levels = c("1w before", "start", "1w after"))) %>%
  filter(fTierName %in% c("Caroline", "Claude", "Olga")) %>%
  group_by(fTimeWindow,Frucht, fTierName) %>%
  count() %>%
  ungroup() %>%
  group_by(fTierName, fTimeWindow) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Frucht = fct_reorder(Frucht, n,sum, desc = TRUE)
  ) %>%
  ggplot(aes(fTimeWindow, n, fill=Frucht)) + geom_col() + 
  scale_y_continuous(name = "Number of visits") +
  scale_x_discrete(name = "Time windows") +
  facet_grid(~Frucht)  +
  theme_light() +
  labs(title = "Percentages of visited crops duting different time windows for speaker WSS_2015_01")


# RESULTS: The speaker was located in the wheat (Weizen) field. We assumed that the wheat field would be visited less often
# after the scare off measures. The results show that the wheat field really was visited less often after the
# start of the speaker WSS_2015_01 (but results are undecisive since the field wasnt often visited anyway)
# the wildboar also visited the meadow more often after the start of the measures

# loc 2 
wildschwein_2016_loc2_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    Frucht = ifelse(is.na(Frucht),"other",Frucht),
    fTierName= as.factor(TierName),
    fTimeWindow = factor(TimeWindow, levels = c("1w before", "start", "1w after"))) %>%
  filter(fTierName %in% c("Caroline", "Miriam", "Frida")) %>%
  group_by(fTimeWindow,Frucht, fTierName) %>%
  count() %>%
  ungroup() %>%
  group_by(fTierName, fTimeWindow) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Frucht = fct_reorder(Frucht, n,sum, desc = TRUE)
  ) %>%
  ggplot(aes(fTimeWindow, perc, fill=Frucht)) + geom_col() + 
  scale_y_continuous(name = "Percentage", labels = scales::percent_format()) +
  scale_x_discrete(name = "Time windows") +
  facet_wrap(~fTierName) +
  theme_light() +
  labs(title = "Percentages of visited crops duting different time windows for speaker WSS_2016_01")

wildschwein_2016_loc2_join %>%
  slice(seq(1,nrow(.),4))%>%
  mutate(
    Frucht = ifelse(is.na(Frucht),"other",Frucht),
    fTierName= as.factor(TierName),
    fTimeWindow = factor(TimeWindow, levels = c("1w before", "start", "1w after"))) %>%
  filter(fTierName %in% c("Caroline", "Miriam", "Frida")) %>%
  group_by(fTimeWindow,Frucht, fTierName) %>%
  count() %>%
  ungroup() %>%
  group_by(fTierName, fTimeWindow) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Frucht = fct_reorder(Frucht, n,sum, desc = TRUE)
  ) %>%
  ggplot(aes(fTimeWindow, n, fill=Frucht)) + geom_col() + 
  scale_y_continuous(name = "Number of visits") +
  scale_x_discrete(name = "Time windows") +
  facet_grid(~Frucht)  +
  theme_light() +
  labs(title = "Percentages of visited crops duting different time windows for speaker WSS_2016_01")

# RESULTS: The speaker was located in a meadow (Wiese). We assumed that the wheat field would be visited less often
# after the scare off measures. The results show that the wild boar visited the meadows less often 
# after the start of the speaker WSS_2016_01. The wild boar also visited some
# crops more often after the start of the measure (Erbsen, Chinaschilf)

# in conclusion the best methods are Method 1 and 3 for evaluating the effectiveness
# further tests should be done on more locations and more wildschwein to confirm this