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
library(DescTools)
library(vegan)
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
s <- c(rep(1, 5), rep(2, 3), rep(3, 2)) #stock
brc <- c(rep(10, 9), 20) #barrier replacement cost

# counts
nb <- 10 #number of barriers
nt <- 2 #number of tribal nations
ns <- 3 #number of fish stocks

# manager inputs
B <- 40 #budget
wh <- 1 #habitat weight
we <- 0 #equity weight
wd <- 0 #diversification weight

# known solutions
sxh <- c(1, 0, 1, 1, 0, 1, 0, 0, 0, 0) #soln max habitat
sxe <- c(1, 0, 0, 0, 0, 0, 0, 0, 1, 1) #soln max equity
sxd <- c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0) #soln max diversification

## Visualize
vis(rep(0, nb))

## optimization 
# objective function

habitat <- function(x) {sum(h * x)}

equity <- function(x) {
  1 - Gini(sapply(1 : nt, 
                  FUN = function (nat) sum(h * x * (tn == nat))))}

div <- function(x) {
  diversity(sapply(1 : ns, 
                   FUN = function (sto) sum(h * x * (s == sto))), 
            index = "shannon")}

nl_obj <- function(x) {-1 * (
  wh * habitat(floor(x)) + we * equity(floor(x)) +
    wd * div(floor(x)))} #multi-objective (minimization)

# constraints
con <- function(x) {c(sum(brc * floor(x)) - B, 
      (diag(nb) - t(D)) %*% floor(x) - 1 + di)} #c(budget, hydrograph) 

# solution 
soln1 <- JDEoptim(rep(0, nb), rep(2, nb),
                fn = nl_obj, constr = con,
                tol = 1e-7, trace = TRUE, triter = 50)


floor(soln1$par)



######################

prob <-
  list(obj = function(x) {
    x1 <- floor(x[1]); x2 <- floor(x[2])
    x3 <- floor(x[3]); x4 <- floor(x[4])
    x5 <- floor(x[5]); x6 <- floor(x[6])
    x7 <- floor(x[7]); x8 <- floor(x[8])
    x9 <- floor(x[9]); x10 <- floor(x[10]) 
    wh * (x1*h[1]+x2*h[2]+x3*h[3]+x4*h[4]+x5*h[5]+x6*h[6]+x7*h[7]+x8*h[8]+x9*h[9]+x10*h[10])+
    we * Gini(c(x1*h[1]+x2*h[2]+x3*h[3]+x4*h[4]+x5*h[5]+x6*h[6]+x7*h[7]+x8*h[8], x9*h[9]+x10*h[10]))+
    wd * diversity(c(x1*h[1]+x2*h[2]+x3*h[3]+x4*h[4]+x5*h[5], x6*h[6]+x7*h[7]+x8*h[8], x9*h[9]+x10*h[10]))  
  },
  con = function(x) {
    x1 <- floor(x[1]); x2 <- floor(x[2])
    x3 <- floor(x[3]); x4 <- floor(x[4])
    x5 <- floor(x[5]); x6 <- floor(x[6])
    x7 <- floor(x[7]); x8 <- floor(x[8])
    x9 <- floor(x[9]); x10 <- floor(x[10])
    c(x1*brc[1]+x2*brc[2]+x3*brc[3]+x4*brc[4]+x5*brc[5]+x6*brc[6]+x7*brc[7]+x8*brc[8]+x9*brc[9]+x10*brc[10] - B)
  })
res <- JDEoptim(rep(0, nb), rep(2, nb),
                fn = prob$obj,
                constr = prob$con,
                tol = 1e-7, trace = TRUE, triter = 50)