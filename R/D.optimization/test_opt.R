#######################################
# Test optimal barrier removal problem:
# Solving for the opt removal plan
# Author: Sunny Jardine
# Date: February, 2021
# Ref: https://roi.r-forge.r-project.org/index.html
#######################################

library(dplyr)
library(DescTools)
library(vegan)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.lpsolve)
library(ROI.plugin.neos)
library(ROI.plugin.nloptr)
library(ROI.plugin.quadprog)
library(ROI.plugin.ipop)
library(ROI.plugin.ecos)
library(ROI.plugin.scs)
library(ROI.plugin.alabama)

## parameters
# directly downstream (matrix)
D <- matrix(0, nrow = 10, ncol = 10)
p <- matrix(c(1, 1, 3, 3, 6, 6, 9, 2, 3, 4, 5, 7, 8, 10), 
            nrow = 7, ncol = 2)
S <- matrix(1, nrow = nrow(D), ncol = ncol(D))
D[p] <- S[p]
di <- colSums(D)
  
# barrier attributes (vectors)
h <- c(4, 1, 3, 4, 1, 3, 1, 3, 1, 4) #habitat
tn <- c(rep(1, 8), rep(2, 2)) #tribal nation
s <- c(rep(1, 5), rep(2, 3), rep(3, 2)) #stock
rc <- c(rep(10, 9), 20) #replacement cost

# counts
nb <- 10 #number of barriers
nt <- 2 #number of tribal nations
ns <- 3 #number of fish stocks

# manager inputs
B <- 40 #budget
wh <- 1 #habitat weight
we <- 0 #equity weight
wr <- 0 #risk (avoidance) weight

## optimization 

tstx1 <- c(1, 0, 1, 1, 0, 1, 0, 0, 0, 0)
tstx2 <- c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0)
tstx3 <- c(1, 0, 0, 0, 0, 0, 0, 0, 1, 1) 

# objective function
habitat <- function(x) {sum(h * x)}
equity <- function(x) {
  1 - Gini(sapply(1 : nt, 
              FUN = function (nat) sum(h * x * (tn == nat))))}
risk <- function(x) {
  diversity(sapply(1 : ns, 
                   FUN = function (sto) sum(h * x * (s == sto))), 
                   index = "shannon")}

mObj <- F_objective(F = function(x) wh * habitat(x) +
                                   we * equity(x) +
                                   wr * risk(x), n = nb) #multi-objective


# constraints
bc <- L_constraint(L = rc, dir = "<=", rhs = B) #budget 
hc <- L_constraint(L = diag(nb) - t(D), 
                   dir = rep("<=", 10), rhs = 1 - di) #hydrology
  
# problem
prob <- OP(objective = h, 
           constraints = c(bc, hc),
           bounds = V_bound(li = 1 : nb, lb = rep.int(0, nb), 
                            ui = 1 : nb, ub = rep.int(1, nb)), 
           types = rep.int("B", nb), 
           maximum = TRUE)

# solution
ROI_applicable_solvers(prob)
soln <- ROI_solve(prob, "glpk", 
                  control = list("verbose" = TRUE, 
                                 "presolve" = TRUE))
ROI::solution(soln)

#notes: (1) the test problem needs to be modified because you get the same optimal solution
# with and without the hydrology constraint; (2) from what I can tell the ROI package
# does not support non-linear mixed integer programming problems and the gini and shannon
# indices create non-linearity
