wdpa
================
Millie Chapman
2/21/2020

``` r
cpad <- st_read("../data/CPAD_2020a/CPAD_2020a_Units.shp")
```

    ## Reading layer `CPAD_2020a_Units' from data source `/Users/milliechapman/Desktop/Berkeley/wdpa-hotspots/wdpa-hotspots/data/CPAD_2020a/CPAD_2020a_Units.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 17068 features and 21 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -374984.2 ymin: -604454.8 xmax: 540016.3 ymax: 449743.2
    ## epsg (SRID):    3310
    ## proj4string:    +proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs

``` r
cpad %>% filter(YR_EST >1990) %>% select(UNIT_ID)  %>%plot()
```

![](wdpa-hotspots_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
cced <- st_read("../data/CCED_2020a/CCED_2020a.shp")
```

    ## Reading layer `CCED_2020a' from data source `/Users/milliechapman/Desktop/Berkeley/wdpa-hotspots/wdpa-hotspots/data/CCED_2020a/CCED_2020a.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 12297 features and 25 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XYZ
    ## bbox:           xmin: -368046.7 ymin: -603668.6 xmax: 493109 ymax: 444116.4
    ## z_range:        zmin: 2.273737e-13 zmax: 344.2585
    ## epsg (SRID):    3310
    ## proj4string:    +proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs

``` r
cced %>% filter(year_est >1990) %>% select(cced_id) %>% plot()
```

![](wdpa-hotspots_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
sig_hab <- st_read("../data/Terrestrial_Significant_Habitats_Summary_-_ACE_[ds2721]-shp/Terrestrial_Significant_Habitats_Summary_-_ACE_[ds2721].shp")
```

    ## Reading layer `Terrestrial_Significant_Habitats_Summary_-_ACE_[ds2721]' from data source `/Users/milliechapman/Desktop/Berkeley/wdpa-hotspots/wdpa-hotspots/data/Terrestrial_Significant_Habitats_Summary_-_ACE_[ds2721]-shp/Terrestrial_Significant_Habitats_Summary_-_ACE_[ds2721].shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 60000 features and 25 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -124.4098 ymin: 32.53429 xmax: -114.1312 ymax: 42.00934
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs

``` r
sig_hab <- as.tibble(sig_hab) %>%
  select(Hex_ID, TerrHabRan, TerrHabTot)
```

    ## Warning: `as.tibble()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

``` r
connectivity <- st_read("../data/Terrestrial_Connectivity_-_ACE_[ds2734]-shp/")
```

    ## Reading layer `Terrestrial_Connectivity_-_ACE_[ds2734]' from data source `/Users/milliechapman/Desktop/Berkeley/wdpa-hotspots/wdpa-hotspots/data/Terrestrial_Connectivity_-_ACE_[ds2734]-shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 63890 features and 15 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -124.4098 ymin: 32.53429 xmax: -114.1312 ymax: 42.00934
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs

``` r
connectivity %>% filter(County == "ALAMEDA") %>% select(OBJECTID) %>% plot()
```

![](wdpa-hotspots_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

transform

``` r
cced <- st_transform(cced, 4326)
cpad <- st_transform(cpad, 4326)
```

``` r
int_c_pa <- as_tibble(st_intersection(connectivity, cpad))
```

    ## although coordinates are longitude/latitude, st_intersection assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int_c_pa %>% filter(County == "MONTEREY") %>% select(geometry) %>% plot()
```

    ## Warning in if (cl %in% c("integer", "numeric")) stripchart(x1, ...) else
    ## plot(x1, : the condition has length > 1 and only the first element will be used

![](wdpa-hotspots_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ccedb<- st_buffer(cced, dist = 0)
```

    ## Warning in st_buffer.sfc(st_geometry(x), dist, nQuadSegs, endCapStyle =
    ## endCapStyle, : st_buffer does not correctly buffer longitude/latitude data

    ## dist is assumed to be in decimal degrees (arc_degrees).

``` r
int_c_ce <- as_tibble(st_intersection(connectivity, ccedb))
```

    ## although coordinates are longitude/latitude, st_intersection assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
int_c_ce %>% filter(County == "ALAMEDA") %>% select(geometry) %>% plot()
```

    ## Warning in if (cl %in% c("integer", "numeric")) stripchart(x1, ...) else
    ## plot(x1, : the condition has length > 1 and only the first element will be used

![](wdpa-hotspots_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
int_c_ce$ce_area<- st_area(int_c_ce$geometry)

int_c_pa$pa_area<- st_area(int_c_pa$geometry)
```

``` r
paArea <- int_c_pa %>%
  group_by(Hex_ID) %>%  
  mutate(pa_area = as.numeric(pa_area)) %>%
  summarise(paArea = sum(pa_area))

ceArea <- int_c_ce %>%
  group_by(Hex_ID) %>%
  mutate(ce_area = as.numeric(ce_area)) %>%
  summarise(ceArea = sum(ce_area))
```

``` r
all <- connectivity %>%
  left_join(ceArea) %>%
  left_join(paArea) 
```

    ## Joining, by = "Hex_ID"
    ## Joining, by = "Hex_ID"

``` r
all$tot_area <- st_area(all$geometry)

all <- all %>%
  mutate(tot_area = as.numeric(tot_area)) 

all <- all %>%
  full_join(sig_hab) %>%
  mutate(perc_pa = paArea/tot_area,
         perc_ce = ceArea/tot_area) %>%
  mutate(perc_pa = replace_na(perc_pa, 0),
         perc_ce = replace_na(perc_ce, 0))
```

    ## Joining, by = "Hex_ID"

``` r
mean(all$perc_ce)
```

    ## [1] 0.02086938

``` r
mean(all$perc_pa)
```

    ## [1] 0.4877632

``` r
habitat_total <- all %>% select(TerrHabTot, perc_pa, perc_ce) %>%
  group_by(TerrHabTot) %>%
  summarise(perc_pa = mean(perc_pa),
            perc_ce = mean(perc_ce))
```

``` r
p <- habitat_total %>% ggplot(aes(x = TerrHabTot, y = perc_pa))  + 
  geom_point(shape=19, fill="blue", color="darkred", size=3) + theme_minimal() + 
  labs( x="Total significant habitat coverage count", y = "Mean percent covered \n by protected areas")
c <- habitat_total %>% ggplot(aes(x = TerrHabTot, y = perc_ce)) + 
  geom_point(shape=19, fill="blue", color="darkred", size=3) + theme_minimal() + 
  labs( x="Total significant habitat coverage count", y = "Mean percent covered \n by conservation easement")

ggarrange(p,c, nrow = 2) 
```

    ## Warning: Removed 1 rows containing missing values (geom_point).
    
    ## Warning: Removed 1 rows containing missing values (geom_point).

![](wdpa-hotspots_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
connectivity_total <- all %>% select(Connectivi, perc_pa, perc_ce) %>%
  group_by(Connectivi) %>%
  summarise(perc_pa = mean(perc_pa),
            perc_ce = mean(perc_ce))
```

``` r
p <- connectivity_total %>% ggplot(aes(x = Connectivi, y = perc_pa))+ 
  geom_point(shape=19, fill="blue", color="darkred", size=3) + theme_minimal() + 
  labs( x="Connectivity metric", y = "Mean percent covered \n by protected areas")
c <- connectivity_total %>% ggplot(aes(x = Connectivi, y = perc_ce)) + 
  geom_point(shape=19, fill="blue", color="darkred", size=3) + theme_minimal() + 
  labs( x="Connectivity metric", y = "Mean percent covered \n by conservation easements")

ggarrange(p,c, nrow = 2) 
```

![](wdpa-hotspots_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
biodiversity <- st_read("../data/Species_Biodiversity_-_ACE_%5Bds2769%5D-shp/Species_Biodiversity_-_ACE_%5Bds2769%5D.shp")
```

    ## Reading layer `Species_Biodiversity_-_ACE_%5Bds2769%5D' from data source `/Users/milliechapman/Desktop/Berkeley/wdpa-hotspots/wdpa-hotspots/data/Species_Biodiversity_-_ACE_%5Bds2769%5D-shp/Species_Biodiversity_-_ACE_%5Bds2769%5D.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 121901 features and 35 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -124.4098 ymin: 32.53429 xmax: -114.1308 ymax: 42.0095
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs

``` r
biodiversity <- as.tibble(biodiversity) 

biodiv <- biodiversity %>%
  select(Hex_ID, NtvSpRnkEc, RarRnkEco, TerrClimRa, SpBioRnkEc)

all <- all %>%
  full_join(biodiv) 
```

    ## Joining, by = "Hex_ID"

``` r
biodiversity_total <- all %>% select(SpBioRnkEc, perc_pa, perc_ce) %>%
  group_by(SpBioRnkEc) %>%
  summarise(perc_pa = mean(perc_pa, na.rm = TRUE),
            perc_ce = mean(perc_ce, na.rm = TRUE))
```

``` r
p <- biodiversity_total %>% ggplot(aes(x = SpBioRnkEc, y = perc_pa))+ 
  geom_point(shape=19, fill="blue", color="darkred", size=3) + theme_minimal() + 
  labs( x="Biodiversity metric", y = "Mean percent covered \n by protected areas")
c <- biodiversity_total %>% ggplot(aes(x = SpBioRnkEc, y = perc_ce)) + 
  geom_point(shape=19, fill="blue", color="darkred", size=3) + theme_minimal() + 
  labs( x="Biodiversity metric", y = "Mean percent covered \n  by conservation easements")

ggarrange(p,c, nrow = 2) 
```

    ## Warning: Removed 1 rows containing missing values (geom_point).
    
    ## Warning: Removed 1 rows containing missing values (geom_point).

![](wdpa-hotspots_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
paArea_yr <- int_c_pa %>%
  mutate(pa_area = as.numeric(pa_area)) %>%
  group_by(YR_EST) %>%  
  summarise(paArea = sum(pa_area) ,
            yr_est = mean(YR_EST))

ceArea_yr <- int_c_ce %>%
  group_by(Hex_ID) %>%
  mutate(ce_area = as.numeric(ce_area)) %>%
  summarise(ceArea = sum(ce_area),
            yr_est = mean(year_est))
```

``` r
paArea_yr <- paArea_yr %>%
  filter(yr_est>0) %>%
  mutate(yr_est_pa = as.factor(round(yr_est, 0)))


ceArea_yr <- ceArea_yr %>%
  filter(yr_est>0) %>%
  mutate(yr_est_ce = as.factor(round(yr_est, 0)))
```

\`\`\`
