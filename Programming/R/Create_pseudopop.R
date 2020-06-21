### Program description #######################################################
# Program name: Create_pseudopop.R                                            #
# Program author: Mike Jackson                                                #
# Project: nCoV_ABM                                                           #
#_____________________________________________________________________________#
# Input datasets: Export_AgeSex.csv, Export_Family.csv, Export_NonFamily.csv, #
# Export_Exact.csv, Export_Sizes.csv                                          #
# Output datasets: sim_tttt_sss.csv, where tttt indexes agent type (Toddler,  #
# Child, etc) and sss indexes size (5k agents, 50k agents, etc)               #
#_____________________________________________________________________________#
# Description: This program uses data mainly from the 2018 American Community #
# Survey from King County WA to create a pseudopopulation for agent-based     #
# modeling. Population members each have an age and sex and are grouped into  #
# households, schools, and workplaces.                                        #
# Steps in this program:                                                      #
# (1) Create population by age/sex                                            #
# (2) Create distribution of households                                       #
# (3) Functions for sampling from population                                  #
# (4) Sample into households                                                  #
# (5) Pull into group quarters                                                #
# (6) Parse into different sizes                                              #
# (7) Distribute children into schools                                        #
# (8) Distribute adults into workplaces                                       #
# (9) Export                                                                  #
#_____________________________________________________________________________#

library(dplyr)
path <- "C:/Users/O992928/Documents/nCoV_ABM/Compiled_Data/"

### (1) Create population by age/sex ##########################################
# Read in the age/sex distribution of the population from the ACS and split   #
# into single-year age groups.                                                #

pop.dist <- read.csv(paste0(path, "Export_AgeSex.csv"), stringsAsFactors = FALSE)

pop.all <- data.frame(Male=logical(0), AgeYrs=numeric(0), Count=numeric(0))
temp.df <- pop.all[1,]

# Use remainders to divide multi-year agegroups into single age groups
for (i in 1:dim(pop.dist)[1]){
  
  temp.df$Male <- pop.dist$Male[i]
  
  ages <- pop.dist$AgeMax[i] - pop.dist$AgeMin[i] + 1
  
  if (ages > 1){
    for (a in pop.dist$AgeMin[i]:pop.dist$AgeMax[i]){
      temp.df$AgeYrs <- a
      temp.df$Count <- ifelse(a==pop.dist$AgeMin[i],
                              floor(pop.dist$Value[i]/ages) + pop.dist$Value[i] %% ages,
                              floor(pop.dist$Value[i]/ages))
      pop.all <- rbind(pop.all, temp.df)
    }
  } else if (ages==1){
    temp.df$AgeYrs <- pop.dist$AgeMin[i]
    temp.df$Count <- pop.dist$Value[i]
    pop.all <- rbind(pop.all, temp.df)
  }
}

row.names(pop.all) <- 1:dim(pop.all)[1]


### (2) Create distribution of households #####################################
# Set up data.frame that splits houses into sizes, with number of known vs.   #
# unknown ages/sexes.                                                         #
# In order to have people of more similar ages clustering in households,      #
# rather than fully random sampling, make some additional assumptions about   #
# age ranges in multi-person households.                                      #
# The first pass through (without these additional assumptions) gave odd HH   #
# structures (e.g. one 4 year old and five adults aged 30-80), and over-      #
# sampled non-senior adults. The second pass (with some of these assumptions) #
# still over-sampled non-senior adults, so force more sampling of children    #
# and seniors.                                                                #

hh.df.blank <- data.frame(child.0.5=numeric(6), child.6.17=numeric(6), 
                    adult.male=numeric(6), adult.female=numeric(6), 
                    adult.unk=numeric(6), senior.male=numeric(6), 
                    senior.female=numeric(6), senior.unk=numeric(6), 
                    unk.no.senior=numeric(6), unk.adult=numeric(6),
                    unk.no.child=numeric(6), unk.no.child.05=numeric(6),
                    unk.no.child.617=numeric(6), unk.any=numeric(6), 
                    Count.hh=numeric(6))

# (A) Read in census data on household characteristics

exact.df <- read.csv(paste0(path, "Export_Exact.csv"), stringsAsFactors = FALSE)
fam.df <- read.csv(paste0(path, "Export_Family.csv"), stringsAsFactors = FALSE)
nonfam.df <- read.csv(paste0(path, "Export_NonFamily.csv"), stringsAsFactors = FALSE)
sizes.df <- read.csv(paste0(path, "Export_sizes.csv"), stringsAsFactors = FALSE)

# (B) For non-family households of size > 1, multiply number of households
# by probability of non-family household sizes to get distribution of HH sizes

nonfam.size <- (sizes.df %>% filter(Hhtype=="NonFamily"))$Value /
  sum((sizes.df %>% filter(Hhtype=="NonFamily"))$Value)

# Non-family with no seniors must have 1 non-senior adult, and the
# rest unknown non-senior
# Extra assumption: most are not children (i.e. adult.unk)
nonfam.no.senior.df <- hh.df.blank
nonfam.no.senior.df$adult.unk <-     c(1, 1, 2, 2, 2, 3)
nonfam.no.senior.df$unk.no.senior <- c(1, 1, 1, 1, 2, 3)
nonfam.no.senior.df$child.0.5 <-     c(0, 0, 0, 1, 1, 1)
nonfam.no.senior.df$child.6.17 <-    c(0, 1, 1, 1, 1, 1)
nonfam.no.senior.df$Count.hh <- 
  round(nonfam.df$Value[nonfam.df$Seniors==FALSE] * nonfam.size)

# Non-family with at least one senior must have 1 senior and the
# rest unknown of any age
# Extra assumption: at least one other is a senior, others more likely
# adult than child
nonfam.senior.df <- hh.df.blank
nonfam.senior.df$senior.unk <- 2
nonfam.senior.df$adult.male <-      c(0, 1, 1, 1, 2, 2)
nonfam.senior.df$unk.no.child.05 <- c(0, 0, 0, 1, 1, 1)
nonfam.senior.df$child.0.5 <-       c(0, 0, 0, 0, 0, 1)
nonfam.senior.df$child.6.17 <-      c(0, 0, 1, 1, 1, 1)
nonfam.senior.df$Count.hh <- 
  round(nonfam.df$Value[nonfam.df$Seniors==TRUE] * nonfam.size)

# (C) For family households (all size > 1), multiply number of households 
# by probability of family household sizes. Make separate probabilities for
# 2-7 vs 3-7 vs 4-7 
# Using the strict probabilities results in a population skewed toward
# larger housholds, since the composition information only tells the minimum
# possible size, not the actual size distribution. Need to shift probabilities 
# downward to account for that. A skew "coefficient" of 2.5 gets very close

fam.2.7 <- (sizes.df %>% filter(Hhtype=="Family"))$Value /
  sum((sizes.df %>% filter(Hhtype=="Family"))$Value)
fam.3.7 <- (sizes.df %>% filter(Hhtype=="Family", Persons>=3))$Value /
  sum((sizes.df %>% filter(Hhtype=="Family", Persons>=3))$Value)
fam.4.7 <- (sizes.df %>% filter(Hhtype=="Family", Persons>=4))$Value /
  sum((sizes.df %>% filter(Hhtype=="Family", Persons>=4))$Value)

fam.2.7.skew <- fam.2.7/2.5
fam.3.7.skew <- fam.3.7/2.5
fam.4.7.skew <- fam.4.7/2.5

fam.2.7[1] <- fam.2.7[1] + sum(fam.2.7.skew[2:6])
fam.2.7[2:6] <- fam.2.7[2:6] - fam.2.7.skew[2:6]
fam.3.7[1] <- fam.3.7[1] + sum(fam.3.7.skew[2:5])
fam.3.7[2:5] <- fam.3.7[2:5] - fam.3.7.skew[2:5]
fam.4.7[1] <- fam.4.7[1] + sum(fam.4.7.skew[2:4])
fam.4.7[2:4] <- fam.4.7[2:4] - fam.4.7.skew[2:4]


# ACS household category: Married, no children
married.df1 <- hh.df.blank
married.df1$adult.male <-   c(1, 1, 2, 2, 3, 3)
married.df1$adult.female <- c(1, 1, 1, 2, 2, 2)
married.df1$unk.no.child <- c(0, 1, 1, 1, 1, 2)
married.df1$Count.hh <- round(fam.2.7*
    (fam.df %>% filter(AdultType=="Married", AnyChild==FALSE))$Value)

# ACS household category: Married, children, only children aged < 6 years
# Extra assumption, assume more children than non-parent adults
married.df2 <- hh.df.blank[1:5,]
married.df2$adult.male <- 1
married.df2$adult.female <- 1
married.df2$child.0.5 <-        c(1, 2, 2, 2, 3)
married.df2$unk.no.child.617 <- c(0, 0, 1, 2, 2)
married.df2$Count.hh <- round(fam.3.7 * 
      (fam.df %>% filter(AdultType=="Married", ChildType=="lt6_only"))$Value)

# ACS household category: Married, children, only children aged >= 6 years
# Extra assumption, assume more children than non-parent adults
married.df3 <- hh.df.blank[1:5,]
married.df3$adult.male <- 1
married.df3$adult.female <- 1
married.df3$child.6.17 <-      c(1, 1, 1, 2, 2)
married.df3$unk.no.child.05 <- c(0, 1, 2, 2, 3)
married.df3$Count.hh <- round(fam.3.7 * 
      (fam.df %>% filter(AdultType=="Married", ChildType=="ge6_only"))$Value)

# ACS household category: Married, children both <6 years and >=6 years
# Extra assumption, assume more children than non-parent adults
married.df4 <- hh.df.blank[1:4,]
married.df4$adult.male <- 1
married.df4$adult.female <- 1
married.df4$child.0.5 <-  c(1, 1, 1, 2)
married.df4$child.6.17 <- c(1, 1, 2, 2)
married.df4$unk.any <-    c(0, 1, 1, 1)
married.df4$Count.hh <- round(fam.4.7 * 
      (fam.df %>% filter(AdultType=="Married", ChildType=="lt_ge_6"))$Value)

# ACS household category: Male HoH only, no children
male.df1 <- hh.df.blank
male.df1$adult.male <- 1
male.df1$unk.no.child <- 1:6
male.df1$Count.hh <- round(fam.2.7 *
      (fam.df %>% filter(AdultType=="MaleOnly", AnyChild==FALSE))$Value)

# ACS household category: Male HoH only, children, only children < 6 years
# Extra assumption, assume more children than non-parent adults
male.df2 <- hh.df.blank
male.df2$adult.male <- 1
male.df2$child.0.5 <-        c(1, 1, 2, 2, 2, 3)
male.df2$unk.no.child.617 <- c(0, 1, 1, 2, 3, 3)
male.df2$Count.hh <- round(fam.2.7 *
      (fam.df %>% filter(AdultType=="MaleOnly", ChildType=="lt6_only"))$Value)

# ACS household category: Male HoH only, children, only children >= 6 years
# Extra assumption, assume more children than non-parent adults
male.df3 <- hh.df.blank
male.df3$adult.male <- 1
male.df3$child.6.17 <-      c(1, 1, 1, 1, 2, 2)
male.df3$unk.no.child.05 <- c(0, 1, 2, 3, 3, 4)
male.df3$Count.hh <- round(fam.2.7 *
      (fam.df %>% filter(AdultType=="MaleOnly", ChildType=="ge6_only"))$Value)

# ACS household category: Male HoH only, children both <6 and >=6 years
# Extra assumption, assume more children than non-parent adults
male.df4 <- hh.df.blank[1:5,]
male.df4$adult.male <- 1
male.df4$child.0.5 <-  c(1, 1, 1, 1, 2)
male.df4$child.6.17 <- c(1, 1, 2, 2, 2)
male.df4$unk.any <-    c(0, 1, 1, 2, 2)
male.df4$Count.hh <- round(fam.3.7 *
      (fam.df %>% filter(AdultType=="MaleOnly", ChildType=="lt_ge_6"))$Value)

# ACS household category: Female HoH only, no children
female.df1 <- hh.df.blank
female.df1$adult.female <- 1
female.df1$unk.no.child <- 1:6
female.df1$Count.hh <- round(fam.2.7 *
      (fam.df %>% filter(AdultType=="FemaleOnly", AnyChild==FALSE))$Value)

# ACS household category: Female HoH only, children only children < 6 years
# Extra assumption, assume more children than non-parent adults
female.df2 <- hh.df.blank
female.df2$adult.female <- 1
female.df2$child.0.5 <-        c(1, 1, 2, 2, 2, 3)
female.df2$unk.no.child.617 <- c(0, 1, 1, 2, 3, 3)
female.df2$Count.hh <- round(fam.2.7 *
      (fam.df %>% filter(AdultType=="FemaleOnly", ChildType=="lt6_only"))$Value)

# ACS household category: Female HoH only, children only children >= 6 years
# Extra assumption, assume more children than non-parent adults
female.df3 <- hh.df.blank
female.df3$adult.female <- 1
female.df3$child.6.17 <-      c(1, 1, 1, 1, 2, 2)
female.df3$unk.no.child.05 <- c(0, 1, 2, 3, 3, 4)
female.df3$Count.hh <- round(fam.2.7 *
      (fam.df %>% filter(AdultType=="FemaleOnly", ChildType=="ge6_only"))$Value)

# ACS household category: Female HoH only, children both <6 and >=6
# Extra assumption, assume more children than non-parent adults
female.df4 <- hh.df.blank[1:5, ]
female.df4$adult.female <- 1
female.df4$child.0.5 <-  c(1, 1, 1, 1, 2)
female.df4$child.6.17 <- c(1, 1, 2, 2, 2)
female.df4$unk.any <-    c(0, 1, 1, 2, 2)
female.df4$Count.hh <- round(fam.3.7 *
      (fam.df %>% filter(AdultType=="FemaleOnly", ChildType=="lt_ge_6"))$Value)

# (D) Households of size 1
single.df <- hh.df.blank[1:4,]
single.df$adult.female[1] <- 1
single.df$senior.female[2] <- 1
single.df$adult.male[3] <- 1
single.df$senior.male[4] <- 1
single.df$Count.hh[1] <- exact.df$Value[exact.df$Senior==FALSE & exact.df$Male==FALSE]
single.df$Count.hh[2] <- exact.df$Value[exact.df$Senior==TRUE & exact.df$Male==FALSE]
single.df$Count.hh[3] <- exact.df$Value[exact.df$Senior==FALSE & exact.df$Male==TRUE]
single.df$Count.hh[4] <- exact.df$Value[exact.df$Senior==TRUE & exact.df$Male==TRUE]

# (E) Combine 
hh.df <- rbind(nonfam.no.senior.df, nonfam.senior.df, 
               married.df1, married.df2, married.df3, married.df4, 
               male.df1, male.df2, male.df3, male.df4, 
               female.df1, female.df2, female.df3, female.df4)

### (3) Functions for sampling from population ################################

# Function for identifying number of males/females to sample
f.samp.sex <- function(n, agemin, agemax){
  n.male <- sum((pop.samp %>% filter(ageyrs>=agemin, ageyrs<=agemax))$Male)
  n.female <- sum((pop.samp %>% filter(ageyrs>=agemin, ageyrs<=agemax))$Female)
  n.all <- n.male/n.female
  x <- sample(c("male", "female"), size=n, replace=TRUE,
         prob=c(n.male/n.all, n.female/n.all))
  return(c(sum(x=="male"), sum(x=="female")))
}

# Function for sampling a random population member with specified constraints
f.samp.age <- function(sex, n, agemin, agemax){
  if (sex=="male"){
    age.s <- sample(pop.samp$ageyrs[pop.samp$ageyrs>=agemin & pop.samp$ageyrs<=agemax],
                    size=n, replace=TRUE,
                    prob=pop.samp$Male[pop.samp$ageyrs>=agemin & pop.samp$ageyrs<=agemax]/
                      sum(pop.samp$Male[pop.samp$ageyrs>=agemin & pop.samp$ageyrs<=agemax]))
  } else if (sex=="female") {
    age.s <- sample(pop.samp$ageyrs[pop.samp$ageyrs>=agemin & pop.samp$ageyrs<=agemax],
                    size=n, replace=TRUE,
                    prob=pop.samp$Female[pop.samp$ageyrs>=agemin & pop.samp$ageyrs<=agemax]/
                      sum(pop.samp$Female[pop.samp$ageyrs>=agemin & pop.samp$ageyrs<=agemax]))
  }
  return(age.s)
}

# Function to remove individuals from total population
f.remove <- function(rm.df, type="both"){
  # rm.df is a data frame with columns (ageyrs, male) as (int, logical)
  # type is used for extra process when removing only males or only females
  
  if (type=="both"){
    agg.df <- aggregate(x~ageyrs+male, data=cbind(rm.df, data.frame(x=1)), FUN=length)
    rm.df.w <- reshape(agg.df, idvar="ageyrs", timevar="male", direction="wide")
    names(rm.df.w)[2:3] <- c("female.sub", "male.sub")
  } else if (type=="male"){
    rm.df.w <- aggregate(male~ageyrs, data=rm.df, FUN=length)
    names(rm.df.w)[2] <- "male.sub"
  } else if (type=="female"){
    rm.df.w <- aggregate(male~ageyrs, data=rm.df, FUN=length)
    names(rm.df.w)[2] <- "female.sub"
  }

  pop.samp <<- full_join(pop.samp, rm.df.w, by="ageyrs")
  if (type %in% c("both", "male")){
    pop.samp$Male <<- ifelse(is.na(pop.samp$male.sub)==TRUE,
                             pop.samp$Male, pop.samp$Male - pop.samp$male.sub)
  }
  if (type %in% c("both", "female")){
    pop.samp$Female <<- ifelse(is.na(pop.samp$female.sub)==TRUE,
                             pop.samp$Female, pop.samp$Female - pop.samp$female.sub)
  }
  pop.samp <<- pop.samp %>% select(ageyrs, Male, Female)
  
}

### (4) Sample into HHs #######################################################
# In this step, individuals are sampled from the full population into         #
# households, using the distributions defined in step (2) and with the        #
# functions created in step (3). The final product of this step is a          #
# data.frame pop.full, which contains individuals characterized by age, sex,  #
# and household index, for the full King County population.                   #

# Convert pop.all from tall to wide for sampling
pop.samp <- reshape(pop.all[,1:3], idvar="AgeYrs", timevar="Male", direction="wide")
pop.samp <- pop.samp %>% rename(ageyrs=AgeYrs)
names(pop.samp)[2:3] <- c("Male", "Female")

pop.full <- data.frame(indexHome=numeric(0), ageyrs=numeric(0), 
                       male=logical(0))

# (A) Classify those who live alone. First sample from the correct age/sex groups
single.list <- list()
start.i <- 0
stop.i <- 0

# Non-senior women
all.persons <- numeric(0)
for (a in 18:64){
  all.persons <- c(all.persons, rep(a, times=pop.samp[a+1,3]))
}
single.list[[1]] <- sample(all.persons, 
                           size=single.df$Count.hh[single.df$adult.female==1],
                           replace=FALSE)

# Senior women
all.persons <- numeric(0)
for (a in 65:99){
  all.persons <- c(all.persons, rep(a, times=pop.samp[a+1,3]))
}
single.list[[2]] <- sample(all.persons, 
                           size=single.df$Count.hh[single.df$senior.female==1],
                           replace=FALSE)

# Non-senior men
all.persons <- numeric(0)
for (a in 18:64){
  all.persons <- c(all.persons, rep(a, times=pop.samp[a+1,2]))
}
single.list[[3]] <- sample(all.persons, 
                           size=single.df$Count.hh[single.df$adult.male==1],
                           replace=FALSE)

# Senior men
all.persons <- numeric(0)
for (a in 65:99){
  all.persons <- c(all.persons, rep(a, times=pop.samp[a+1,2]))
}
single.list[[4]] <- sample(all.persons, 
                           size=single.df$Count.hh[single.df$senior.male==1],
                           replace=FALSE)


# Put into households
hh.temp <- data.frame(indexHome=(stop.i+1):(stop.i+sum(single.df$Count.hh)),
                      ageyrs=numeric(sum(single.df$Count.hh)),
                      male=logical(sum(single.df$Count.hh)))

for (i in 1:4){
  start.i <- stop.i + 1
  stop.i <- stop.i + length(single.list[[i]])
  hh.temp$male[start.i:stop.i] <- ifelse(i %in% c(1,2), FALSE, TRUE)
  hh.temp$ageyrs[start.i:stop.i] <- single.list[[i]]
}
pop.full <- rbind(pop.full, hh.temp)

# Subtract from full population
sub1 <- as.data.frame(table(c(single.list[[1]], single.list[[2]])), stringsAsFactors = FALSE)
sub1$ageyrs <- as.numeric(sub1$Var1)
sub1 <- sub1 %>% select(ageyrs, Freq) %>% rename(Female.sub=Freq)
sub2 <- as.data.frame(table(c(single.list[[3]], single.list[[4]])), stringsAsFactors = FALSE)
sub2$ageyrs <- as.numeric(sub2$Var1)
sub2 <- sub2 %>% select(ageyrs, Freq) %>% rename(Male.sub=Freq)

sub.all <- full_join(sub1, sub2, by="ageyrs")

pop.samp <- full_join(pop.samp, sub.all, by="ageyrs")
pop.samp$Female.sub <- replace(pop.samp$Female.sub, is.na(pop.samp$Female.sub)==T, 0)
pop.samp$Male.sub <- replace(pop.samp$Male.sub, is.na(pop.samp$Male.sub)==T, 0)

pop.samp$Female <- pop.samp$Female - pop.samp$Female.sub
pop.samp$Male <- pop.samp$Male - pop.samp$Male.sub

pop.samp <- pop.samp %>% select(ageyrs, Male, Female)


# (B) Start with multi-person households, randomly sample population members
time1 <- Sys.time()  
for (r in 1:dim(hh.df)[1]){

  hh.n <- hh.df$Count.hh[r]
  start.i <- stop.i + 1
  stop.i <- stop.i + hh.n
  
  if (hh.df$child.0.5[r] > 0){
    p.n <- hh.df$child.0.5[r]
    sex.split <- f.samp.sex(hh.n*p.n, 0, 5)
    males <- f.samp.age("male", n=sex.split[1], 0, 5)
    females <- f.samp.age("female", n=sex.split[2], 0, 5)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n), 
                          ageyrs=c(males, females), 
                          male=c(rep(T, times=sex.split[1]), rep(F, times=sex.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }
    
  if (hh.df$child.6.17[r] > 0){
    p.n <- hh.df$child.6.17[r]
    sex.split <- f.samp.sex(hh.n*p.n, 6, 17)
    males <- f.samp.age("male", n=sex.split[1], 6, 17)
    females <- f.samp.age("female", n=sex.split[2], 6, 17)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n), 
                          ageyrs=c(males, females), 
                          male=c(rep(T, times=sex.split[1]), rep(F, times=sex.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }
  
  if (hh.df$adult.male[r] > 0){
    p.n <- hh.df$adult.male[r]
    males <- f.samp.age("male", n=hh.n*p.n, 18, 64)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n), 
                          ageyrs=males, 
                          male=c(rep(T, times=hh.n*p.n)))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3], type="male")
    hh.temp$indexHome <- start.i:stop.i
    pop.full <- rbind(pop.full, hh.temp)
  }
  
  if (hh.df$adult.female[r] > 0){
    p.n <- hh.df$adult.female[r]
    females <- f.samp.age("female", n=hh.n*p.n, 18, 64)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n), 
                          ageyrs=females, 
                          male=rep(F, times=hh.n*p.n))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3], type="female")
    hh.temp$indexHome <- start.i:stop.i
    pop.full <- rbind(pop.full, hh.temp)
  }
  
  if (hh.df$adult.unk[r] > 0){
    p.n <- hh.df$adult.unk[r]
    sex.split <- f.samp.sex(hh.n*p.n, 18, 64)
    males <- f.samp.age("male", n=sex.split[1], 18, 64)
    females <- f.samp.age("female", n=sex.split[2], 18, 64)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n), 
                          ageyrs=c(males, females), 
                          male=c(rep(T, times=sex.split[1]), rep(F, times=sex.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }
  
  if (hh.df$senior.unk[r] > 0){
    p.n <- hh.df$senior.unk[r]
    sex.split <- f.samp.sex(hh.n*p.n, 65, 99)
    males <- f.samp.age("male", n=sex.split[1], 65, 99)
    females <- f.samp.age("female", n=sex.split[2], 65, 99)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n), 
                          ageyrs=c(males, females), 
                          male=c(rep(T, times=sex.split[1]), rep(F, times=sex.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }

  if (hh.df$unk.no.senior[r] > 0){
    p.n <- hh.df$unk.no.senior[r]
    sex.split <- f.samp.sex(hh.n*p.n, 0, 64)
    males <- f.samp.age("male", n=sex.split[1], 0, 64)
    females <- f.samp.age("female", n=sex.split[2], 0, 64)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n),
                          ageyrs=c(males, females),
                          male=c(rep(T, times=sex.split[1]), rep(F, times=sex.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }
  
  if (hh.df$unk.no.child[r] > 0){
    p.n <- hh.df$unk.no.child[r]
    sex.split <- f.samp.sex(hh.n*p.n, 18, 99)
    males <- f.samp.age("male", n=sex.split[1], 18, 99)
    females <- f.samp.age("female", n=sex.split[2], 18, 99)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n),
                          ageyrs=c(males, females),
                          male=c(rep(T, times=sex.split[1]), rep(F, times=sex.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }
  
  if (hh.df$unk.no.child.05[r] > 0){
    p.n <- hh.df$unk.no.child.05[r]
    sex.split <- f.samp.sex(hh.n*p.n, 6, 99)
    males <- f.samp.age("male", n=sex.split[1], 6, 99)
    females <- f.samp.age("female", n=sex.split[2], 6, 99)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n),
                          ageyrs=c(males, females),
                          male=c(rep(T, times=sex.split[1]), rep(F, times=sex.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }
  
  if (hh.df$unk.no.child.617[r] > 0){
    p.n <- hh.df$unk.no.child.617[r]
    prob.child <- sum(pop.samp[1:6, 2:3])/sum(pop.samp[c(1:6, 19:100), 2:3])
    c.n <- round(hh.n * p.n * prob.child)
    a.n <- (hh.n * p.n) - c.n

    child.split <- f.samp.sex(c.n, 0, 5)
    boys <- f.samp.age("male", n=child.split[1], 0, 5)
    girls <- f.samp.age("female", n=child.split[2], 0, 5)
    
    adult.split <- f.samp.sex(a.n, 18, 99)
    males <- f.samp.age("male", n=adult.split[1], 18, 99)
    females <- f.samp.age("female", n=adult.split[2], 18, 99)
    
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n),
                          ageyrs=c(boys, males, girls, females),
                          male=c(rep(T, times=child.split[1]+adult.split[1]),
                                 rep(F, times=child.split[2]+adult.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }
  
  if (hh.df$unk.any[r] > 0){
    p.n <- hh.df$unk.any[r]
    sex.split <- f.samp.sex(hh.n*p.n, 0, 99)
    males <- f.samp.age("male", n=sex.split[1], 0, 99)
    females <- f.samp.age("female", n=sex.split[2], 0, 99)
    hh.temp <- data.frame(indexHome=numeric(hh.n*p.n),
                          ageyrs=c(males, females),
                          male=c(rep(T, times=sex.split[1]), rep(F, times=sex.split[2])))
    hh.temp <- hh.temp[order(runif(hh.n*p.n)),]
    f.remove(hh.temp[,2:3])
    hh.temp$indexHome <- rep(start.i:stop.i, times=p.n)
    pop.full <- rbind(pop.full, hh.temp)
  }
  
}
time2 <- Sys.time()
time2 - time1

# (C) Under this sampling approcah, similar HH types share similar HH indices.
# In this final step, randomize indexHome to break this relationship.
pop.full <- pop.full %>% arrange(indexHome, ageyrs)

index.df <- data.frame(indexHome=unique(pop.full$indexHome))
index.df$indexHome.new <- sample(index.df$indexHome, 
                                 size=dim(index.df)[1], replace=FALSE)

pop.full <- left_join(pop.full, index.df, by="indexHome")
pop.full <- pop.full %>% select(indexHome.new, ageyrs, male) %>%
  arrange(indexHome.new, ageyrs) %>% rename(indexHome=indexHome.new)

### (5) Pull out group quarters ###############################################
# Based on nursing home data and number of people in group settings, estimate #
# population members in NH and other institutions by age and sex.             #
# Update 2020.05.13 - make nursing homes and group quartes have 20 persons    #
# each, rather than 100 (i.e assuming only 20 effective contacts).            #
# The output of this step is two data.frames of individual population members #
# characterized by age, sex, and nursing home index or group quarter index.   #

group.n <- 37856

# (A) To compute number of persons aged 65-84 in NH, get total number 65+ in 
# nursing home, subtract the number 85+ in NH, then calculate percent. Multiply  
# these by the full percents, not the depleted sizes after assigning to HHs
nh.65 <- 0.015 
nh.85 <- 0.055 

in.nh.85 <- floor(sum((pop.all %>% filter(AgeYrs >= 85))$Count) * nh.85)
in.nh.6584 <- floor(sum((pop.all %>% filter(AgeYrs >= 65))$Count) * nh.65) - in.nh.85

# Get number of people age 18-64 years in group quarters
gq.1864 <- (group.n - in.nh.85 - in.nh.6584) 

# (B) Sample individuals into NHs, first 65-84 then 85-99
nh.df.6584 <- data.frame(ageyrs=numeric(0), male=logical(0))
nh.df.85 <- data.frame(ageyrs=numeric(0), male=logical(0))
for (a in 65:99){
  temp.df.m <- data.frame(ageyrs=rep(a, times=pop.samp[a+1, 2]),
                        male=rep(TRUE, times=pop.samp[a+1, 2])) # Males
  temp.df.f <- data.frame(ageyrs=rep(a, times=pop.samp[a+1, 3]),
                          male=rep(FALSE, times=pop.samp[a+1, 3])) # Females
  
  if (a < 85){
    nh.df.6584 <- rbind(nh.df.6584, temp.df.m, temp.df.f)
  } else {
    nh.df.85 <- rbind(nh.df.85, temp.df.m, temp.df.f)
  }
}

nh.df.6584$rand <- runif(n=dim(nh.df.6584)[1], 0, 1)
nh.df.85$rand <- runif(n=dim(nh.df.85)[1], 0, 1)

# Double up nh.df.85, since there aren't enough people for the number needed
nh.df.85 <- rbind(nh.df.85, nh.df.85)

# Randomly sort and select the correct number of persons
nh.df <- rbind((nh.df.6584 %>% arrange(rand))[1:in.nh.6584,],
               (nh.df.85 %>% arrange(rand))[1:in.nh.85,]) %>% select(ageyrs, male)

# Re-sort randomly and assign to specific nursing homes
nh.df <- nh.df[order(runif(dim(nh.df)[1], 0, 1)), ]

nh.count <- ceiling(dim(nh.df)[1]/20)
nh.index <- rep(0:(nh.count-1), times=20)
nh.index <- nh.index[order(nh.index)]

nh.df$indexNH <- nh.index[1:dim(nh.df)[1]] 
nh.df <- nh.df %>% select(indexNH, ageyrs, male)

# (C) Sample individuals aged 18-64 into group homes. Sex-segregated
gq.df <- data.frame(indexGH=numeric(gq.1864), ageyrs=numeric(gq.1864), 
                    male=logical(gq.1864), rand=runif(gq.1864, 0, 1))

gq.male.n <- round(gq.1864*sum(pop.samp[19:65,2])/sum(pop.samp[19:65, 2:3]))

males.gq <- f.samp.age("male", gq.male.n, 18, 64)
females.gq <- f.samp.age("female", gq.1864 - gq.male.n, 18, 64)

gq.df$ageyrs <- c(males.gq, females.gq)
gq.df$male <- c(rep(TRUE, times=gq.male.n), rep(FALSE, times=gq.1864-gq.male.n))

# Segregate by sex and assign to group quarters
gq.df <- gq.df %>% arrange(male, rand)

q.f <- dim(gq.df[gq.df$male==FALSE,])[1]
q.m <- dim(gq.df[gq.df$male==TRUE,])[1]

gq.index.f <- rep(1:ceiling(q.f/20), times=20)
gq.index.f <- gq.index.f[order(gq.index.f)]
gq.index.m <- rep(1:ceiling(q.m/20), times=20)
gq.index.m <- gq.index.m[order(gq.index.m)]

# Use 0:(n-1) instead of 1:n
gq.index.f <- gq.index.f - 1
gq.index.m <- gq.index.m - 1

gq.df$indexGQ <- numeric(dim(gq.df)[1])
gq.df$indexGQ[1:q.f] <- gq.index.f[1:q.f]
gq.df$indexGQ[(q.f+1):dim(gq.df)[1]] <- gq.index.m[1:q.m]

gq.df <- gq.df %>% select(indexGQ, ageyrs, male)


### (6) Parse into different sizes ############################################
# Split slices of the population to use in different size models: 5000, 50000,#
# 500,000, and full population. For the population slices, need to re-create  #
# household index so it is sequential from zero to max.                       #
# Update 2020.04/18: It's more computationally efficient to run 10 patches of #
# 50,000 than one population of 500,000. Make 10 variants of the 50k size.    #
# For now, only build and output the 50k size, 10 variants each.              #

# Get estimated number of households needed to get 5000, 50000, 500000 people
hh.total <- max(pop.full$indexHome)
# hh.5k <- floor(hh.total * 5000 / dim(pop.full)[1])
# hh.500k <- floor(hh.total * 500000 / dim(pop.full)[1])
hh.50k <- floor(hh.total * 50000 / dim(pop.full)[1])

# hh.index.5k <- (1:hh.total)[order(runif(hh.total))][1:hh.5k]
# hh.index.500k <- (1:hh.total)[order(runif(hh.total))][1:hh.500k]
hh.index.50k <- list()
for (h in 1:10){
  hh.index.50k[[h]] <- (1:hh.total)[order(runif(hh.total))][1:hh.50k]
}

# (A) Subset households and create new sequential indices (from 0:(n-1) for GAMA)
f.subset <- function(inset=pop.full, index.v){
  pop.sub <- inset %>% filter(indexHome %in% index.v)

  index.df <- data.frame(indexHome=index.v, indexHome.new=0:(length(index.v)-1))
  
  pop.sub <- left_join(pop.sub, index.df, by="indexHome") %>%
    select(indexHome.new, ageyrs, male) %>% rename(indexHome=indexHome.new) %>%
    arrange(indexHome, ageyrs)
    
  return(pop.sub %>% select(indexHome, ageyrs, male))
}

# pop.5k <- f.subset(index.v=hh.index.5k)
# pop.500k <- f.subset(index.v=hh.index.500k)
pop.50k <- list()
for (h in 1:10){
  pop.50k[[h]] <- f.subset(index.v = hh.index.50k[[h]])
}


# (B) Subset the group homes 
gq.total <- max(gq.df$indexGQ)
# gq.5k <- ceiling(gq.total * 5000/ dim(pop.full)[1])
# gq.500k <- ceiling(gq.total * 500000/ dim(pop.full)[1])
gq.50k <- ceiling(gq.total * 50000/ dim(pop.full)[1])


# gq.index.5k <- (1:gq.total)[order(runif(gq.total))][1:gq.5k]
# gq.index.500k <- (1:gq.total)[order(runif(gq.total))][1:gq.500k]
gq.index.50k <- list()
for (g in 1:10){
  gq.index.50k[[g]] <- (1:gq.total)[order(runif(gq.total))][1:gq.50k]
}

f.subset.gq <- function(inset=gq.df, index.v){
  pop.sub <- inset %>% filter(indexGQ %in% index.v)
  
  index.df <- data.frame(indexGQ=index.v, indexGQ.new=0:(length(index.v)-1))
  
  pop.sub <- left_join(pop.sub, index.df, by="indexGQ") %>%
    select(indexGQ.new, ageyrs, male) %>% rename(indexGQ=indexGQ.new) %>%
    arrange(indexGQ)
  
  return(pop.sub)
  
}

# gq.5k <- f.subset.gq(index.v=gq.index.5k)
# gq.500k <- f.subset.gq(index.v=gq.index.500k)
gq.50k <- list()
for (g in 1:10){
  gq.50k[[g]] <- f.subset.gq(index.v = gq.index.50k[[g]])  
}

# (C) Subset the nursing homes
nh.total <- max(nh.df$indexNH)
# nh.5k <- ceiling(nh.total * 5000 / dim(pop.full)[1])
# nh.500k <- ceiling(nh.total * 500000 / dim(pop.full)[1])
nh.50k <- ceiling(nh.total * 50000 / dim(pop.full)[1])

# nh.index.5k <- (1:nh.total)[order(runif(nh.total))][1:nh.5k]
# nh.index.500k <- (1:nh.total)[order(runif(nh.total))][1:nh.500k]
nh.index.50k <- list()
for (n in 1:10){
  nh.index.50k[[n]] <- (1:nh.total)[order(runif(nh.total))][1:nh.50k]
}


f.subset.nh <- function(inset=nh.df, index.v){
  pop.sub <- inset %>% filter(indexNH %in% index.v)

  index.df <- data.frame(indexNH=index.v, indexNH.new=0:(length(index.v)-1))
  
  pop.sub <- left_join(pop.sub, index.df, by="indexNH") %>%
    select(indexNH.new, ageyrs, male) %>% rename(indexNH=indexNH.new) %>%
    arrange(indexNH)
  
  return(pop.sub)
}

# nh.5k <- f.subset.nh(index.v=nh.index.5k)
# nh.500k <- f.subset.nh(index.v=nh.index.500k)
nh.50k <- list()
for (n in 1:10){
  nh.50k[[n]] <- f.subset.nh(index.v = nh.index.50k[[n]])
}

### (7) Distribute children into schools ######################################
# Based on school district data, assume schools have between 400 and 800      #
# students and 40 - 80 staff. Cluster children by age (6-10, 11-13, 14-17).   #
# Similar-aged children within a HH should got to the same school.            #
# This code will tend to cluster low households with lower school numbers.    #
# For daycares, there is a 31.1% chance that a child aged <6 is in some form  #
# of daycare/childcare/pre-school. Daycares are set to size 20 children and   #
# two adults.                                                                 #
# Currently ignoring home education.                                          #
# Update 2020.05.25: Ro is too high. Assume that children only make contact   #
# with up to 100 other students and 10 teachers at school.                    #

f.school <- function(pop.sch){
  # For pop.sch, a data.frame of the source population dataset, 
  # assigns all children aged 6-17 to a school
  # How to add 3% homeschooling?

  pop.sch$indexSchool <- NA
  pop.sch$indexPerson <- 1:dim(pop.sch)[1]
  
  # student.n <- dim(pop.sch %>% filter(ageyrs >=6, ageyrs <18))[1]
  
  # (A) Distribute students into schools
  # Index for schools (e=elementary, m=middle, h=high school, d=daycare)
  index.e <- 0 
  index.m <- 1 
  index.h <- 2
  index.d <- 3
  
  # Number of students in each school
  in.e <- 0 
  in.m <- 0
  in.h <- 0
  in.d <- 0
  
  # Iterate through the population
  # School index changes randomly between 400 and 800 students
  for (i in 1:dim(pop.sch)[1]){
    
    
    if ((pop.sch$ageyrs[i] < 6) & (runif(1, 0, 1) < 0.311)){
      # Childcare "schools"
      pop.sch$indexSchool[i] <- index.d
      in.d <- in.d + 1
      
      if (in.d >= 20){
        in.d <- 0
        index.d <- max(index.e, index.m, index.h, index.d) + 1
      }
    } else if (pop.sch$ageyrs[i] %in% 6:10){
      # Elementary schools
      pop.sch$indexSchool[i] <- index.e
      in.e <- in.e + 1
      
      if (in.e >= 100){
        in.e <- 0
        index.e <- max(index.e, index.m, index.h, index.d) + 1
      }
      
    } else if (pop.sch$ageyrs[i] %in% 11:13) {
      # Middle schools
      pop.sch$indexSchool[i] <- index.m
      in.m <- in.m + 1
      
      if (in.m >= 100){
        in.m <- 0
        index.m <- max(index.e, index.m, index.h, index.d) + 1
      }
      
    } else if (pop.sch$ageyrs[i] %in% 14:17) {
      # High schools
      pop.sch$indexSchool[i] <- index.h
      in.h <- in.h + 1
      
      if (in.h >= 100){
        in.h <- 0
        index.h <- max(index.e, index.m, index.h, index.d) + 1
      }
      
    }
  }
  
  
  # (B) Randomly assign adults into schools as staff
  staff.df <- as.data.frame(table(pop.sch$indexSchool), stringsAsFactors = FALSE)
  staff.df$Var1 <- as.numeric(staff.df$Var1)
  staff.df$Freq <- ceiling(staff.df$Freq/10)
  names(staff.df) <- c("indexSchool", "staff.n")
  
  staff.match <- data.frame(indexStaff=numeric(0), indexPerson=numeric(0))
  
  for (i in 1:dim(staff.df)[1]){
    temp.df <- data.frame(indexStaff=staff.df$indexSchool[i],
                        indexPerson= sample(pop.sch$indexPerson[pop.sch$ageyrs <65 & pop.sch$ageyrs >= 18],
                                            size=staff.df$staff.n[i], replace=FALSE),
                        stringsAsFactors = FALSE)
    staff.match <- rbind(staff.match, temp.df)
  }
  
  # In case someone gets sampled to multiple schools
  staff.match <- staff.match %>% distinct(indexStaff, indexPerson)
  
  pop.sch <- left_join(pop.sch, staff.match, by="indexPerson")
  pop.sch$indexSchool <- ifelse(is.na(pop.sch$indexStaff)==FALSE, pop.sch$indexStaff,
                                pop.sch$indexSchool)
  
 return(pop.sch %>% select(indexHome, ageyrs, male, indexSchool))
}

# pop.5k <- f.school(pop.5k)
# pop.500k <- f.school(pop.500k)
for (s in 1:10){
  pop.50k[[s]] <- f.school(pop.50k[[s]])
}

### (8) Distribute adults into workplaces #####################################
# Import census data on percent of adults in the workforce by age and sex.    #
# Based on the US Small Business Association, in WA in 2015, the distribution #
# of employment by business size was:                                         #
# 520,000 workers in businesses of < 20 people                                #
# 456,000 workers in businesses of 20-99 people                               #
# 376,000 workers in businesses of 100-500 people                             #
# 1,260,000 workers in businesses of >500 people.                             #
# Assume that, within a business, no more than 500 people are effectively     #
# mixing at once.                                                             #
# Use gamma distribution to preferentially select smaller business sizes      #
# Skews a bit toward mid-size (20-99) vs small (<20), but that's reasonable   #
# to account for customers at small businesses.                               #
# After assigning to businesses, randomly pull out some workers and assign    #
# them to nursing homes or group quarters. For NH, use CMMS guidance of 3hr   #
# staff time per resident to estimate number of staff as (residents/3).       #
# Assume the same for group quarters.                                         #
# Update 2020.05.25: Ro is too high; assume that workers at large busisnesses #
# only make effective contact with up to 100 people.                          #

employed.df <- read.csv(paste0(path, "export_workforce.csv"), stringsAsFactors = FALSE)

f.business <- function(pop.bus){
  # For an input data.frame pop.bus, randomly assign adults age 18-74 to
  # workplaces based on workplace sizes and age/sex-specific probabilities
  # of being employed
  
  pop.bus <- left_join(pop.bus, employed.df, by=c("ageyrs", "male"))
  # Randomize so that households don't cluster in both businesses and schools
  pop.bus$rand <- runif(dim(pop.bus)[1])
  pop.bus <- pop.bus %>% arrange(rand) %>% select(-rand)
  
  pop.bus$indexBus <- rep(NA, times=dim(pop.bus)[1])
  
  bus.index <- 0
  #bus.size <- ifelse(runif(1) <0.05, 500, round(10+20*rgamma(1, 0.5, 0.5)))
  bus.size <- min(round(10+20*rgamma(1, 0.5, 0.5)), 100)
  in.b <- 0
  
  for (i in 1:dim(pop.bus)[1]){
    if (pop.bus$ageyrs[i] >= 18 & pop.bus$ageyrs[i] < 75){
      if(runif(1) < pop.bus$work.pct[i]){
        pop.bus$indexBus[i] <- bus.index
        in.b <- in.b + 1
        
        if (in.b == bus.size){
          in.b <- 0
          bus.index <- bus.index + 1
          #bus.size <- ifelse(runif(1) <0.05, 500, round(10+20*rgamma(1, 0.5, 0.5)))
          bus.size <- min(round(10+20*rgamma(1, 0.5, 0.5)), 100)
        }
      }
    }
  }
  
  return(pop.bus %>% select(indexHome, ageyrs, male, indexSchool, indexBus) %>%
           arrange(indexHome, ageyrs))
}

# (A) Assign to businesses
# pop.5k <- f.business(pop.bus=pop.5k)
# pop.500k <- f.business(pop.bus=pop.500k)

for (w in 1:10){
  pop.50k[[w]] <- f.business(pop.bus=pop.50k[[w]])
}

# (B) Take some workers and assign them to group quarters and nursing homes
f.nh.gq <- function(pop.bus=pop.full, nh.sub=nh.df, gq.sub=gq.df){
  pop.bus$indexNH <- NA
  pop.bus$indexGQ <- NA
  
  # Sample 3 workers per resident
  n.nh <- as.data.frame(table(nh.sub$indexNH), stringsAsFactors=FALSE)
  n.gq <- as.data.frame(table(gq.sub$indexGQ), stringsAsFactors=FALSE)
  n.nh$indexNH <- as.numeric(n.nh$Var1)
  n.nh$staff.n <- round(n.nh$Freq/3)
  n.gq$indexGQ <- as.numeric(n.gq$Var1)
  n.gq$staff.n <- round(n.gq$Freq/3)
  index.nh <- numeric(0)
  index.gq <- numeric(0)
   
  for(r in 1:dim(n.nh)[1]){
    index.nh <- c(index.nh, rep(n.nh$indexNH[r], times=n.nh$staff.n[r]))
  }
  for (r in 1:dim(n.gq)[1]){
    index.gq <- c(index.gq, rep(n.gq$indexGQ[r], times=n.gq$staff.n[r]))
  }
  
  swap.nh <- sample(which(is.na(pop.bus$indexBus)==FALSE), length(index.nh), replace=FALSE)
  swap.gq <- sample(which(is.na(pop.bus$indexBus)==FALSE), length(index.gq), replace=FALSE)
  swap.gq2 <- swap.gq[which(!(swap.gq %in% swap.nh))] # Get rid of GQ when also in NH
  
  pop.bus$indexNH[swap.nh] <- index.nh
  pop.bus$indexBus[swap.nh] <- NA
  pop.bus$indexGQ[swap.gq] <- index.gq
  pop.bus$indexBus[swap.gq] <- NA
  
  return(pop.bus)
}

# pop.5k <- f.nh.gq(pop.5k, nh.5k, gq.5k)
# pop.500k <- f.nh.gq(pop.500k, nh.500k, gq.500k)
for (b in 1:10){
  pop.50k[[b]] <- f.nh.gq(pop.50k[[b]], nh.50k[[b]], gq.50k[[b]])
}


### (9) Export results ########################################################
# The GAMA code split the household population into different classes based   #
# on age, and not all the classes need all the variables. Make the splits and #
# export.                                                                     #

f_agesplit <- function(inset, size, part_of_list=FALSE, list.n=0){
  
  # Replace NA's with -1
  inset$indexSchool <- ifelse(is.na(inset$indexSchool)==TRUE, -1, inset$indexSchool)
  inset$indexBus <- ifelse(is.na(inset$indexBus)==TRUE, -1, inset$indexBus)
  inset$indexNH <- ifelse(is.na(inset$indexNH)==TRUE, -1, inset$indexNH)
  inset$indexGQ <- ifelse(is.na(inset$indexGQ)==TRUE, -1, inset$indexGQ)
  
  inset <- inset %>% arrange(ageyrs, indexHome)
  
  toddler <- inset %>% filter(ageyrs < 6, indexSchool==-1) %>% 
    select(indexHome, ageyrs, male)
  
  child <- inset %>% filter(ageyrs < 18, indexSchool>=0) %>% 
    select(indexHome, ageyrs, male, indexSchool)
  
  adult <- inset %>% filter(ageyrs >= 18 & ageyrs <75)
  
  senior <- inset %>% filter(ageyrs >= 75) %>% select(indexHome, ageyrs, male)
  
  if (part_of_list==FALSE){
    #write.csv(toddler, paste0(path, "sim_Toddler_", size, ".csv"), row.names = FALSE)
    write.csv(child, paste0(path, "sim_Child_", size, ".csv"), row.names = FALSE)
    write.csv(adult, paste0(path, "sim_Adult_", size, ".csv"), row.names = FALSE)
    #write.csv(senior, paste0(path, "sim_Senior_", size, ".csv"), row.names = FALSE)
  } else {
    #write.csv(toddler, paste0(path, "sim_Toddler_", size, "_", list.n, ".csv"), row.names = FALSE)
    write.csv(child, paste0(path, "sim_Child_", size, "_", list.n, ".csv"), row.names = FALSE)
    write.csv(adult, paste0(path, "sim_Adult_", size, "_", list.n, ".csv"), row.names = FALSE)
    #write.csv(senior, paste0(path, "sim_Senior_", size, "_", list.n, ".csv"), row.names = FALSE)
  }
}

# f_agesplit(inset=pop.5k, size="5k")
# f_agesplit(inset=pop.500k, size="500k")


for (s in 1:10){
  f_agesplit(inset=pop.50k[[s]], size="50k", part_of_list = TRUE, list.n = s-1)
  #write.csv(gq.50k[[s]], paste0(path, "sim_GQ_50k_", s-1, ".csv"), row.names=FALSE)
  #write.csv(nh.50k[[s]], paste0(path, "sim_NH_50k_", s-1, ".csv"), row.names=FALSE)
}

write.csv(pop.full, paste0(path, "sim_HH_full.csv"), row.names=FALSE)
write.csv(pop.5k, paste0(path, "sim_HH_5k.csv"), row.names=FALSE)
#write.csv(pop.50k, paste0(path, "sim_HH_50k.csv"), row.names=FALSE)
#write.csv(pop.500k, paste0(path, "sim_HH_500k.csv"), row.names=FALSE)

write.csv(gq.df, paste0(path, "sim_GQ_full.csv"), row.names=FALSE)
write.csv(gq.5k, paste0(path, "sim_GQ_5k.csv"), row.names=FALSE)
#write.csv(gq.50k, paste0(path, "sim_GQ_50k.csv"), row.names=FALSE)
#write.csv(gq.500k, paste0(path, "sim_GQ_500k.csv"), row.names=FALSE)

write.csv(nh.df, paste0(path, "sim_NH_full.csv"), row.names=FALSE)
write.csv(nh.5k, paste0(path, "sim_NH_5k.csv"), row.names=FALSE)
#write.csv(nh.50k, paste0(path, "sim_NH_50k.csv"), row.names=FALSE)
#write.csv(nh.500k, paste0(path, "sim_NH_500k.csv"), row.names=FALSE)


