### Program description #######################################################
# Program name: Fx_Compile_GAMA_Hosps.R                                       #
# Created by: Mike Jackson                                                    #
# Date: 11 April 2020                                                         #
#_____________________________________________________________________________#
# The GAMA simulation outputs hospitalizations for sub-population patches as  #
# as separate .csv files. This function identifies the relevant files, reads  #
# the data, and ouputs as a single array for the population.                  #
# There are two separate types of hospitalization files: daily total counts   #
# (hospx.csv) and total counts by age(hospagex.csv), where x indexes the sub- #
# population.                                                                 #
# The input files are oddly formatted: The first row contains the name of     #
# the output variable from GAMA, and the second contains the daily counts     #
# from the specified age group and infection category.                        #
#_____________________________________________________________________________#

fx.read.hosp <- function(dir){
  # Input: dir, the directory where the simulation results are stored
  # Outputs: an array with the simulation data
  
  # Get the list of .csv files
  files.v <- list.files(path=dir, pattern="hosp") 
  
  # Split off the hospage files from the hosp files
  age.files <- grep("age", files.v)
  files.v.hosp <- files.v[-age.files]
  files.v.age <- files.v[age.files]
  
  # Get the number of files (will be +1 since GAMA indexes starting at 0)
  # Strip out the .csv, then get the population group, category, and sim number
  files.s <- unlist(lapply(strsplit(files.v.hosp, c("\\.")), "[", 1))
  #pop.lab <- unlist(lapply(strsplit(files.s, c("_")), "[", 1)) 
  #comp.lab <- unlist(lapply(strsplit(files.s, c("_")), "[", 2))
  #sim.num <- as.numeric(unlist(lapply(strsplit(files.s, c("_")), "[", 3)))
  sim.num <- as.numeric(sub("hosp", "", files.s))
  
  n.sims <- max(sim.num, na.rm=TRUE) + 1
  
  # Find out how many time steps are in each simulation
  sim.len <- dim(read.csv(paste0(dir, files.v.hosp[1]), header=FALSE))[2]
  
  # Create output array for daily hospitalizations. Dimensions are sims, time
  hosp.a <- array(data=NA, dim=c(n.sims, sim.len)) 
  
  # Create output array for hospitalizations by age. Dimensions asre sims, years
  age.a <- array(data=NA, dim=c(n.sims, 100))

  
  # Need to:
  # a) Based on file suffixes, figure out how many simulations were run
  # b) Based on length of file, figure out how many time steps there were
  # c) Create the array
  # d) Read each .csv file into the correct dimension of the array
  
  # Read in the second row from each file
  for (f in 1:n.sims){
    hosp.a[f,] <- as.numeric(read.csv(paste0(dir, files.v.hosp[f]), header = FALSE)[2,])
    age.a[f,] <- as.numeric(read.csv(paste0(dir, files.v.age[f]), header = FALSE)[2,])
    
  }
  
  return(list(hosp.a, age.a))
  
}