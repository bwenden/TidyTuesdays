setwd("~/Stats en cours/TidyTuesday/2018-09-11")
library(tidyverse)
library(tmap)
library(tmaptools)

data <- read.csv2("cats_vs_dogs.csv", sep=",", dec=".")

# obtain US county shape
get_US_state_2017_shape <- function() {
  dir <- tempdir()
  download.file("http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_500k.zip", destfile = file.path(dir, "cb_2017_us_state_500k.zip"))
  unzip(file.path(dir, "cb_2017_us_state_500k.zip"), exdir = dir)
  US <- read_shape(file.path(dir, "cb_2017_us_state_500k.shp"))
  levels(US$NAME) <- iconv(levels(US$NAME), from = "latin1", to = "utf8")
  US
}

US <- get_US_state_2017_shape()

# append data to shape
US <- append_data(US, data, key.shp = "NAME", key.data = "state", ignore.duplicates = TRUE) %>%
  na.omit()

tm_shape(US) + tm_fill("n_households")

####Plot households number per state
state_households <- tm_shape(US) +
  tm_polygons("n_households", title = "", 
              border.col = "gray50", border.alpha = .5, palette="BuPu", showNA=FALSE) +
  tm_borders(lwd=1, col = "black", alpha = .5) +
  tm_credits("Data @ data.world",
             position = c("right", "bottom")) +
  tm_layout(title = "Number of households", 
            
            legend.position = c("left","bottom" ), 
            frame = FALSE, 
            inner.margins = c(0.1, 0.1, 0.05, 0.05))
tmap_save(state_households, "Households.png", scale = 0.7, width = 6.125,outer.margins = 0)


####Plot proportion of pet owners per state
state_pets <- tm_shape(US) +
  tm_polygons("percent_pet_households", title = "", 
              border.col = "gray50", border.alpha = .5, palette="BuPu", showNA=FALSE,
              
              breaks =  c(40, 45,50,55, 60,65, 70,75, 80, Inf)) +
  tm_borders(lwd=1, col = "black", alpha = 0) +
  tm_credits("Data @ data.world",
             position = c("right", "bottom"), size=0.9) +
  tm_layout(main.title = "Percentage of households with pets", 
            legend.position = c("left","bottom" ), 
            legend.text.size=1,
            frame = FALSE, 
            earth.boundary=TRUE,
            inner.margins = c(0.1, 0.1, 0.05, 0.05))
  
tmap_save(state_pets, "percent_pet_household.png", scale = 0.7, width = 6.125,outer.margins = 0)


####Plot households number and proportion of pet owners per state
state_households <- tm_shape(US) +
  tm_polygons("n_households", title = "", 
              border.col = "gray50", border.alpha = .5, palette="BuPu", showNA=FALSE) +
  tm_borders(lwd=1, col = "black", alpha = .5) +
  tm_credits("Data @ data.world",
             position = c("right", "bottom")) +
  tm_layout(title = "Number of households", 
            legend.position = c("left","bottom" ), 
            frame = FALSE, 
            inner.margins = c(0.1, 0.1, 0.05, 0.05))+
  tm_bubbles(size=1,  col = "percent_pet_households", border.col = "black",
             border.alpha = 0.5,
             #breaks = c(-Inf, 0, 2, 4, 6, Inf) ,
             title.col = "% pet households", 
             id = "NAME"
            ) 
tmap_save(state_households, "Households.png", scale = 0.7, width = 6.125,outer.margins = 0)


####Identify dogs states vs cats states (percentage)
US_pets <- US %>%
  select(1:9, percent_dog_owners, percent_cat_owners) %>%
  mutate(percent_cat_dog_owners = percent_dog_owners/percent_cat_owners )

state_pets <- tm_shape(US_pets) +
  tm_polygons("percent_cat_dog_owners", title = "Dog vs cat ratio\n(in household percentages)", 
              border.col = "gray50", border.alpha = .5, palette="PRGn", midpoint=1, style="cont", showNA=FALSE,
              legend.is.portrait=FALSE) +
  tm_borders(lwd=1, col = "black", alpha = 0) +
  tm_credits("Data @ data.world",
             position = c("right", "bottom"), size=0.9) +
  tm_layout(main.title = "TidyTuesday: Dog vs cat households", 
            legend.position = c("left","bottom" ), 
            legend.text.size=1,
            frame = FALSE, 
            earth.boundary=TRUE,
            inner.margins = c(0.1, 0.1, 0.05, 0.05))


####Identify dogs states vs cats states (population)
US_pet_pop <- US %>%
  select(1:9, dog_population, cat_population) %>%
  mutate(cat_dog_population = dog_population/cat_population) 

tm_shape(US_pet_pop) +
  tm_polygons("cat_dog_population", title = "Dog vs cat ratio",  
              border.col = "gray50", border.alpha = .5, palette="PRGn",midpoint=1, style="cont", showNA=FALSE,
              legend.is.portrait=FALSE) +
  tm_borders(lwd=1, col = "black", alpha = 0) +
  tm_credits("Data @ data.world",
             position = c("right", "bottom"), size=0.9) +
  tm_layout(main.title = "TidyTuesday: Dog vs cat populations", 
            legend.position = c("left","bottom" ), 
            legend.text.size=1,
            frame = FALSE, 
            earth.boundary=TRUE,
            inner.margins = c(0.1, 0.1, 0.05, 0.05))


###Compare with median age
age <- read.csv2("sc-est2016-agesex-civ.csv", sep=",", dec=".") %>%
  select(NAME, SEX, AGE, POPEST2016_CIV) %>%
  group_by(NAME, AGE) %>%
  summarise(pop=sum(POPEST2016_CIV))

tot_pop <- age %>%
  group_by(NAME) %>%
  summarise(tot_pot = sum(pop))

age <- left_join(age, tot_pop, by="NAME") %>%
  group_by(NAME) %>%
  mutate(cumsum_pop=cumsum(pop),
         tot_pop = tot_pop/2)

age <- age[age$cumsum_pop<=0.5*age$tot_pop,]
  
