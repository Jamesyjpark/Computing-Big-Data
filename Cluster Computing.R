load("/Users/yeongjunpark1/Downloads/breastcohort.RData")
length(unique(breastcohort$FamID))

# family distribution
library(dplyr)
library(ggplot2)
his <-
  hist(distribution$count, col = "grey", main = "Distribution of Family
       Size", xlab = "Family Size", ylab = "Number of Family")

x.fit <- seq(min(distribution$count), max(distribution$count))
y.fit <- dnorm(x.fit, mean=mean(distribution$count),
               sd=sd(distribution$count))
y.fit <- y.fit*diff(his$mids[1:2])*length(distribution$count)
lines(x.fit, y.fit, col = "black", lwd = 2)

dens <- density(distribution$count)
plot(dens, col = "grey", "Density of Family Size", xlab = "Family Size")
polygon(dens, col = "grey", border = "blue")

# of families in the cohort
cancer <-
  breastcohort %>%
  filter(AffectedBreast == 1) %>%
  select(FamID, ID) %>%
  group_by(FamID) %>%
  summarize(count = n()) %>%
  distinct(FamID, count, .keep_all = TRUE)

his <- hist(cancer$count, col = "grey", main = "The number of relatives with
            breast cancer within a family",
            xlab = "Number of Breast Cancer", ylab = "Number of Family")

x.fit <- seq(min(cancer$count), max(cancer$count))
y.fit <- dnorm(x.fit, mean=mean(cancer$count), sd=sd(cancer$count))
y.fit <- y.fit*diff(his$mids[1:2])*length(cancer$count)
lines(x.fit, y.fit, col = "black", lwd = 2)

cancer %>%
  ggplot(aes(FamID, count)) + geom_violin() + theme_classic()

# Agebreast distribution
proband <-
  breastcohort %>%
  filter(ID == 50)

hist(proband$AgeBreast, col = "grey", main = "Distribution of AgeBreast of
     the probands",
     xlab = "AgeBreast", ylab ="Number of probands")

cancer.relative <-
  breastcohort %>%
  filter(AffectedBreast == 1) %>%
  select(FamID, ID, AgeBreast) %>%
  group_by(FamID) %>%
  summarize(AgeBreast = mean(AgeBreast))

hist(cancer.relative$AgeBreast, col = "grey",
     main = "Distribution of AgeBreast of the relatives",
     xlab = "AgeBreast", ylab = "Number of Relativers")

boxplot(cancer.relative$AgeBreast, main = "Distribution of AgeBreast of the relatives",
        xlab = "AgeBreast", ylab = "Number of Relativers")

# boxplot
proband %>%
  ggplot(aes(FamID, AgeBreast)) + geom_boxplot() + theme_classic() +
  
  
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)

## Parallel computing
library(BayesMendel)
library(dplyr)
library(tidyverse)
library(parallel)
library(DBI)

con <- src_mysql(dbname = "bst262health", host = "db-bst262",
                 username = "bst262user", password = "d7@yM2Z8!a")
breastcohort <- tbl(con, "breastcohort")
# MakeRelationship function requres only ID, Gender, Father ID, Mother ID
# First degree - 1) Mother, 2) Father, 3) Siblings, 4) Offspring- code = 2, 3, 4
# Second degree - 1) grandparents, 2) aunts, 3) uncles, 4) nieces, 5)nephews- code = 5,6, 7, 8, 13
# Parallelize this task

cl <- makeCluster(detectCores(), type="FORK")
set.seed(54308) # set the random seed using the last five digits of HUID
# The original dataframe is subsetted to only incldue 50000 random observations
breastcohort <- breastcohort[sample(nrow(breastcohort), 50000),]
breastcohort.split <- split(breastcohort, breastcohort$FamID) # breastcohort split

fam <- parSapply(cl, breastcohort.split, MakeRelationship, counselee.id = 50)

# Now the task is split
breastcohort <-
  breastcohort %>% mutate(un.fam = unlist(fam),
                          relation = case_when(un.fam %in% c(2,3,4) ~ 1,
                                               un.fam %in% c(5,6,7,8,13) ~ 2,
                                               un.fam %in% c(1,14,15) ~ 0))


### Risk prediction
set.seed(54308) # set the random seed using the last five digits of HUID

random200 <-
  probands[sample(nrow(probands),200),]

# Unparallelized
library(data.table)
random.probands <- data.table(random200)
# start the clock
ptm <- proc.time()
temp <- NULL
init <- 1
for (i in unique(random.probands$FamID)){
  temp[[init]] <- brcapro(breastcohort[breastcohort$FamID == i,],
                          counselee.id = 50,
                          params=brcaparams(age.by=10),
                          print=FALSE)@predictions
  temp[[init]] <- merge(temp[[init]], i)
  init <- init+1
}

result <- do.call(rbind, temp) # also- do something like param so only 10- year risks returned
proc.time() - ptm
result[c(1:4),]

## Parallelized
ptm <- proc.time() # start the clock after I am connected to the SQL database
set.seed(54308) # set the random seed using the last five digits of HUID
proband <-
  breastcohort %>%
  filter(ID == 50)

random200 <-
  probands[sample(nrow(probands),200),]

probands.split <- split(random200, random200$FamID) # probands split
cl <- makeCluster(detectCores(), type="FORK") # split the cores - 4 log-in
nodes ussed
par.split <- parSapply(cl, probands.split, brcapro, counselee.id = 50,
                       params=brcaparams(age.by=10), print=FALSE) # parallelized
par.split[[1]]@predictions$”breast ca risk”[1] # return only vector values
proc.time() - ptm # stop the clock
