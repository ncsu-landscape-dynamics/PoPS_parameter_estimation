library(rgdal)
library(raster)
library(sp)
library(Rcpp)
#library(ggplot2)
source("quantity_allocation_disagreement.R")
Sys.setenv("PKF_CXXFLAGS"="-std=c++11")
sourceCpp("rpops/pops.cpp")

## Load required data for model
infected_file = "H:/My Drive/PoPS and Tangible Landscape/Case Studies/spotted_latternfly/slf_new_extent/initial_infections_2014_single_count_pm.tif"
host_file = "H:/My Drive/PoPS and Tangible Landscape/Case Studies/spotted_latternfly/slf_new_extent/tree_of_heaven_new_extent_pm.tif"
total_plants_file = "H:/My Drive/PoPS and Tangible Landscape/Case Studies/spotted_latternfly/slf_new_extent/total_hosts_pm.tif"
temperature_file = "H:/My Drive/PoPS and Tangible Landscape/Case Studies/spotted_latternfly/slf_new_extent/crit_temp_slf_2014_2017.tif"
temperature_coefficient_file = "H:/My Drive/PoPS and Tangible Landscape/Case Studies/spotted_latternfly/slf_new_extent/temp_coefficient_slf_2014_2017.tif"
precipitation_coefficient_file = ""

use_lethal_temperature = FALSE
temp = FALSE
precip = FALSE

infected = raster(infected_file)
infected[is.na(infected)] <- 0
host = raster(host_file)
host[is.na(host)] <- 0
susceptible = host - infected
susceptible[is.na(susceptible)] <- 0
total_plants = raster(total_plants_file)
total_plants[is.na(total_plants)] <- 0
weather = FALSE
if (use_lethal_temperature == TRUE) {
  temperature_stack = stack(temperature_file)
  temperature_stack[is.na(temperature_stack)] <- 0
}
if (temp == TRUE) {
  temperature_coefficient = stack(temperature_coefficient_file)
  weather = TRUE
  weather_coefficient_stack = temperature_coefficient
  if (precip ==TRUE){
    precipitation_coefficient = stack(precipitation_coefficient_file)
    weather_coefficient_stack = weather_coefficient_stack * precipitation_coefficient
  }
} else if(precip == TRUE){
  precipitation_coefficient = stack(precipitation_coefficient_file)
  weather = TRUE
  weather_coefficient_stack = precipitation_coefficient
}

season_month_start = 6
season_month_end = 11
time_step = "month"
start_time = 2015
end_time = 2017

number_of_years = end_time-start_time+1
if (use_lethal_temperature == TRUE) {
  temperature = list(as.matrix(temperature_stack[[1]]))
  for(i in 2:number_of_years) {
    temperature[[i]] <- as.matrix(temperature_stack[[i]])
  }
}

if (time_step == "week") {
  number_of_time_steps = (end_time-start_time+1)*52 +1
} else if (time_step == "month") {
  number_of_time_steps = (end_time-start_time+1)*12
} else if (time_step == "day") {
  number_of_time_steps = (end_time-start_time+1)*365
}

if (weather == TRUE) {
  weather_coefficient_stack[is.na(weather_coefficient_stack)] <- 0
  weather_coefficient <- list(as.matrix(weather_coefficient_stack[[1]]))
  for(i in 2:number_of_time_steps) {
    weather_coefficient[[i]] <- as.matrix(weather_coefficient_stack[[i]])
  }
}

mortality_tracker = infected
values(mortality_tracker) <- 0

cols = as.numeric(ncol(susceptible))
rows = as.numeric(nrow(susceptible))
ew_res = xres(susceptible)
ns_res = yres(susceptible)
lethal_temperature = -25.87
random_seed = 42
reproductive_rate = 2.2
short_distance_scale = 80
lethal_temperature_month = 1
infected = as.matrix(infected)
susceptible = as.matrix(susceptible)
total_plants = as.matrix(total_plants)
mortality_tracker = as.matrix(mortality_tracker)
dispersal_kern = "cauchy"
percent_short_distance_dispersal = 1.0
long_distance_scale = 0.0
wind_dir = "NONE"
kappa = 0

data <- pops_model(random_seed = random_seed, 
                   lethal_temperature = lethal_temperature, use_lethal_temperature = use_lethal_temperature, lethal_temperature_month = lethal_temperature_month,
                   reproductive_rate = reproductive_rate,
                   weather = weather, short_distance_scale = short_distance_scale, infected = infected,
                   susceptible = susceptible, mortality_tracker =mortality_tracker,
                   total_plants = total_plants, temperature = temperature,
                   weather_coefficient = weather_coefficient, 
                   ew_res = ew_res, ns_res = ns_res,
                   time_step = time_step,
                   season_month_start = season_month_start, season_month_end = season_month_end,
                   start_time = start_time, end_time = end_time,
                   dispersal_kern = dispersal_kern, percent_short_distance_dispersal = percent_short_distance_dispersal,
                   long_distance_scale = long_distance_scale,
                   wind_dir = wind_dir, kappa = kappa)

       ## Load observed data on occurence
       slf2015 <- raster("H:/My Drive/PoPS and Tangible Landscape/Case Studies/spotted_latternfly/slf_new_extent/initial_infections_2015_single_count_pm.tif")
       slf2016 <- raster("H:/My Drive/PoPS and Tangible Landscape/Case Studies/spotted_latternfly/slf_new_extent/initial_infections_2016_single_count_pm.tif")
       
       ## set up reclassification matrix for binary reclassification
       rcl <- c(1, Inf, 1, 0, 0.99, NA)
       rclmat <- matrix(rcl, ncol=3, byrow=TRUE)
       ## reclassify to binary values
       slf2015 <- reclassify(slf2015, rclmat)
       slf2016 <- reclassify(slf2016, rclmat)
       ## Get rid of NA values to make the code 
       slf2015[is.na(slf2015)] <- 0
       slf2016[is.na(slf2016)] <- 0
       
       ## Create our function for variable(s) of interest
       param_func <- function(reproductive_rate, short_distance_scale, random_seed) {
         data <- pops_model(random_seed = random_seed, 
                            lethal_temperature = lethal_temperature, use_lethal_temperature, lethal_temperature_month,
                            reproductive_rate = reproductive_rate,
                            weather = weather, short_distance_scale = short_distance_scale, infected = infected,
                            susceptible = susceptible, mortality_tracker =mortality_tracker,
                            total_plants = total_plants, temperature = temperature,
                            weather_coefficient = weather_coefficient, 
                            ew_res = ew_res, ns_res = ns_res,
                            time_step = time_step,
                            season_month_start = season_month_start, season_month_end = season_month_end,
                            start_time = start_time, end_time = end_time,
                            dispersal_kern = dispersal_kern, percent_short_distance_dispersal = percent_short_distance_dispersal,
                            long_distance_scale = long_distance_scale,
                            wind_dir = wind_dir, kappa = kappa)
       return(data)
       }
       
       ## Create function for MCMC runs
       MCMC = function(num_iterations, start_reproductive_rate, start_short_distance_scale, start_random_seed, sd_reproductive_rate, sd_short_distance_scale){
       params = data.frame(reproductive_rate = rep(0,num_iterations), short_distance_scale = rep(0,num_iterations), random_seed = rep(0,num_iterations), total_disagreement = rep(0,num_iterations))
       params$reproductive_rate[1] = start_reproductive_rate
       params$short_distance_scale[1] = start_short_distance_scale
       params$random_seed[1] = start_random_seed
       data <- param_func(start_reproductive_rate, start_short_distance_scale, start_random_seed)
       
       ## set up comparison
       comp2015 <- raster(infected_file)
       comp2016 <- raster(infected_file)
       comp2015[] <- data[[1]][[1]]
       comp2016[] <- data[[1]][[2]]
       comp2015 <- reclassify(comp2015, rclmat)
       comp2016 <- reclassify(comp2016, rclmat)
       comp2015 [is.na(comp2015)] <- 0
       comp2016 [is.na(comp2016)] <- 0
       
       c15 <- quantity_allocation_disagreement(slf2015, comp2015)
       c16 <- quantity_allocation_disagreement(slf2016, comp2016)
       params$total_disagreement[1] = c15$total_disagreement+c16$total_disagreement
       
       
       for(i in 2:num_iterations){
       current_reproductive_rate = params$reproductive_rate[i-1]
       proposed_reproductive_rate = round(abs(rnorm(1,mean=current_reproductive_rate,sd= sd_reproductive_rate)), digits = 2)
       
       current_short_distance_scale = params$short_distance_scale[i-1]
       proposed_short_distance_scale = round(abs(rnorm(1, mean=current_short_distance_scale, sd=sd_short_distance_scale)), digits = 1)
       
       current_random_seed = params$random_seed[i-1]
       proposed_random_seed = current_random_seed + 1
       
       data <- param_func(proposed_reproductive_rate, proposed_short_distance_scale, proposed_reproductive_rate)
       
       ## set up comparison
       comp2015 <- raster(infected_file)
       comp2016 <- raster(infected_file)
       comp2015[] <- data[[1]][[1]]
       comp2016[] <- data[[1]][[2]]
       comp2015 <- reclassify(comp2015, rclmat)
       comp2016 <- reclassify(comp2016, rclmat)
       comp2015 [is.na(comp2015)] <- 0
       comp2016 [is.na(comp2016)] <- 0
       
       c15 <- quantity_allocation_disagreement(slf2015, comp2015)
       c16 <- quantity_allocation_disagreement(slf2016, comp2016)
       params$total_disagreement[i] = c15$total_disagreement+c16$total_disagreement
       
       if(params$total_disagreement[i] <= params$total_disagreement[i-1]){
         params$reproductive_rate[i] = proposed_reproductive_rate
         params$short_distance_scale[i] = proposed_short_distance_scale
         params$random_seed[i] = proposed_random_seed      # accept change if model improves
       } else if (abs((params$total_disagreement[i]/params$total_disagreement[i-1])-1) < runif(1)) {
         params$reproductive_rate[i] = proposed_reproductive_rate
         params$short_distance_scale[i] = proposed_short_distance_scale
         params$random_seed[i] = proposed_random_seed    # accept change randomly if model is worse than previous run but only up until 2x total disagreement
       } else {
         params$reproductive_rate[i] = current_reproductive_rate
         params$short_distance_scale[i] = current_short_distance_scale
         params$random_seed[i] = current_random_seed
         params$total_disagreement[i] = params$total_disagreement[i-1]    # otherwise "reject" move, and stay where we are
       }
       
       }
       return(params)
       }
       
       num_iterations = 1000
       start_reproductive_rate = 3.0
       start_short_distance_scale = 40
       start_random_seed = 1
       sd_reproductive_rate = 0.4
       sd_short_distance_scale = 4
       
       params <- MCMC(num_iterations, start_reproductive_rate, start_short_distance_scale, start_random_seed, sd_reproductive_rate, sd_short_distance_scale)
       
       

