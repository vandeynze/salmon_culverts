#######################################
# Test optimal barrier removal problem:
# with mixed non-linear integer programming 
# Author: Sunny Jardine
# Date: February, 2021
# Ref: https://cran.r-project.org/web/packages/DEoptimR/DEoptimR.pdf
# https://ieeexplore.ieee.org/document/4016057
#######################################

library(dplyr)
library(DEoptimR)
library(here)

source(here("R", "D.optimization", "test_vis.R")) #vis function

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
TH <- rbind(h * as.numeric(tn == 1), h * as.numeric(tn == 2)) #tribal habitat
s <- c(rep(1, 5), rep(2, 3), rep(3, 2)) #stock
SH <- rbind(h * as.numeric(s == 1), h * as.numeric(s == 2), 
            h * as.numeric(s == 3)) #stock habitat
brc <- c(rep(10, 9), 20) #barrier replacement cost

# counts
nb <- 10 #number of barriers
nt <- 2 #number of tribal nations
ns <- 3 #number of fish stocks

# manager inputs
B <- 40 #budget
th_min <- 0.3
sh_min <- 0.05

# known solutions
sxh <- c(1, 0, 1, 1, 0, 1, 0, 0, 0, 0) #soln max habitat
sxe <- c(1, 0, 0, 0, 0, 0, 0, 0, 1, 1) #soln w ec
sxr <- c(1, 0, 1, 0, 0, 1, 0, 0, 1, 0) #soln w rc

## Visualize
vis(rep(0, nb))

## optimization 
# objective functions
# 1) Linear objective
obj <- L_objective(h) 

# constraints
bc <- L_constraint(L = brc, dir = "<=", rhs = B) #budget 
hc <- L_constraint(L = diag(nb) - t(D), 
                   dir = rep("<=", 10), rhs = 1 - di) #hydrology
ec <- L_constraint(L = th_min * rbind(h, h) - TH, 
                   dir = rep("<=", 2), rhs = matrix(0, nrow = 2, ncol = 1)) #equity
rc <- L_constraint(L = sh_min * rbind(h, h, h) - SH,
                   dir = rep("<=", 3), rhs = matrix(0, nrow = 3, ncol = 1)) #risk 

# problem & solution linear obj
# bc and hc
prob1 <- OP(objective = obj, 
           constraints = c(bc, hc),
           bounds = V_bound(li = 1 : nb, lb = rep.int(0, nb), 
                            ui = 1 : nb, ub = rep.int(1, nb)), 
           types = rep.int("B", nb), 
           maximum = TRUE)

soln1 <- ROI_solve(prob1, "glpk", 
                  control = list("verbose" = TRUE, 
                                 "presolve" = TRUE))
vis(ROI::solution(soln1))

# bc, hc, and ec
prob2 <- OP(objective = obj, 
            constraints = c(bc, hc, ec),
            bounds = V_bound(li = 1 : nb, lb = rep.int(0, nb), 
                             ui = 1 : nb, ub = rep.int(1, nb)), 
            types = rep.int("B", nb), 
            maximum = TRUE)

soln2 <- ROI_solve(prob2, "glpk", 
                   control = list("verbose" = TRUE, 
                                  "presolve" = TRUE))
vis(ROI::solution(soln2))

# bc, hc, and rc
prob3 <- OP(objective = obj, 
            constraints = c(bc, hc, rc),
            bounds = V_bound(li = 1 : nb, lb = rep.int(0, nb), 
                             ui = 1 : nb, ub = rep.int(1, nb)), 
            types = rep.int("B", nb), 
            maximum = TRUE)

soln3 <- ROI_solve(prob3, "glpk", 
                   control = list("verbose" = TRUE, 
                                  "presolve" = TRUE))
vis(ROI::solution(soln3))

# # 2) Non-linear objective

#library(DescTools)
#library(vegan)
#library(ROI.plugin.neos)

## manager inputs
# wh <- 1 #habitat weight
# we <- 0 #equity weight
# wr <- 0 #risk (avoidance) weight

# habitat <- function(x) {sum(h * x)}
# 
# equity <- function(x) {
#   1 - Gini(sapply(1 : nt, 
#               FUN = function (nat) sum(h * x * (tn == nat))))}
# 
# risk <- function(x) {
#   diversity(sapply(1 : ns, 
#                    FUN = function (sto) sum(h * x * (s == sto))), 
#                    index = "shannon")}
# 
# nl_obj <- F_objective(F = function(x) wh * habitat(x) +
#                                    we * equity(x) +
#                                     wr * risk(x), n = nb) #multi-objective

# neos solver 
#soln_neos <- ROI_solve(prob, "neos", email = "jardine@uw.edu")
#ROI::solution(soln_neos)

# # solution non-linear obj
# nl_prob <- OP(objective = nl_obj, 
#            constraints = c(bc, hc),
#            bounds = V_bound(li = 1 : nb, lb = rep.int(0, nb), 
#                             ui = 1 : nb, ub = rep.int(1, nb)), 
#            types = rep.int("B", nb), 
#            maximum = TRUE)
# 
# nl_soln_neos <- ROI_solve(nl_prob, "neos", email = "jardine@uw.edu")
# ROI::solution(nl_soln_neos)

#notes: (1) the test problem needs to be modified because you get the same optimal solution
# with and without the hydrology constraint; (2) from what I can tell the ROI package
# does not support non-linear mixed integer programming problems and the gini and shannon
# indices create non-linearity
