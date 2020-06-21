### Program description #######################################################
# Program name: Fx_Compile_GAMA_Output.R                                      #
# Created by: Mike Jackson                                                    #
# Date: 11 April 2020                                                         #
#_____________________________________________________________________________#
# The agent-based simulation in GAMA outputs counts of the population in      #
# categories of age and infection status as separate .csv files. These files  #
# may come from a single simulation run or from multiple iterations over some #
# set of parameters. This function identifies the relevant files, reads in    #
# the data, and ouputs as a single array for the population.                  #
# The input files are oddly formatted: The first row contains the name of     #
# the output variable from GAMA, and the second contains the daily counts     #
# from the specified age group and infection category.                        #
#_____________________________________________________________________________#

fx.read.GAMA <- function(dir){
  # Input: dir, the directory where the simulation results are stored
  # Outputs: an array with the simulation data
  
  # Get the list of .csv files
  # Underscore pattern leaves out the beta values .csv file
  files.v <- list.files(path=dir, pattern="_") 
  
  # Get the number of files (will be +1 since GAMA indexes starting at 0)
  # Strip out the .csv, then get the population group, category, and sim number
  files.s <- unlist(lapply(strsplit(files.v, c("\\.")), "[", 1))
  pop.lab <- unlist(lapply(strsplit(files.s, c("_")), "[", 1)) 
  comp.lab <- unlist(lapply(strsplit(files.s, c("_")), "[", 2))
  sim.num <- as.numeric(unlist(lapply(strsplit(files.s, c("_")), "[", 3)))
  
  n.sims <- max(sim.num, na.rm=TRUE) + 1
  
  # Find out how many time steps are in each simulation
  sim.len <- dim(read.csv(paste0(dir, files.v[1]), header=FALSE))[2]
  
  # Create output array. Dimensionss are sims, population groups, categories, time
  pop.a <- array(data=NA, dim=c(n.sims, 6, 3, sim.len)) 
  
  dimnames(pop.a)[[2]] <- c("Toddler", "Child", "Adult", "Senior", "NH", "GQ")
  dimnames(pop.a)[[3]] <- c("Sus", "Exp", "Inf")
  
  # Need to:
  # a) Based on file suffixes, figure out how many simulations were run
  # b) Based on length of file, figure out how many time steps there were
  # c) Create the array
  # d) Read each .csv file into the correct dimension of the array

  # Read in the second row from each file
  for (f in 1:length(files.v)){
    dim.3 <- ifelse(pop.lab[f]=="toddler", 1,
                    ifelse(pop.lab[f]=="child", 2, 
                           ifelse(pop.lab[f]=="adult", 3,
                                  ifelse(pop.lab[f]=="senior", 4,
                                         ifelse(pop.lab[f]=="nh", 5, 6)))))
    dim.2 <- ifelse(comp.lab[f]=="sus", 1,
                    ifelse(comp.lab[f]=="exp", 2, 3))
    
    pop.a[sim.num[f]+1, dim.3, dim.2, ] <- 
      as.numeric(read.csv(paste0(dir, files.v[f]), header=FALSE)[2,])
    
  }
  
  return(pop.a)
  
}