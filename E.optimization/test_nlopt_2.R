#######################################
# Test optimal barrier removal problem:
# with mixed non-linear integer programming 
# Author: Sunny Jardine
# Date: February, 2021
# Ref: http://sekhon.berkeley.edu/rgenoud/genoud.html
# http://sekhon.berkeley.edu/rgenoud/
#######################################

library(dplyr)
library(rgenoud)
library(DescTools)
library(vegan)
library(here)

source(here("R", "E.optimization", "test_vis.R")) #vis function

## parameters
# counts
nb <- 10 #number of barriers
nt <- 2 #number of tribal nations
ns <- 3 #number of fish stocks

# directly downstream (matrix)
D <- matrix(0, nrow = nb, ncol = nb)
p <- matrix(c(1, 1, 3, 3, 6, 6, 9, 2, 3, 4, 5, 7, 8, 10), 
            nrow = 7, ncol = 2)
S <- matrix(1, nrow = nrow(D), ncol = ncol(D))
D[p] <- S[p]
di <- colSums(D)
  
# barrier attributes 
h <- c(4, 1, 3, 4, 1, 3, 1, 4, 1, 4) #habitat vec
tn <- rbind(c(rep(1, 8), rep(0, 2)), c(rep(0, 8), rep(1, 2))) #tribal nation mat
s <-  rbind(c(rep(1, 5), rep(0, 5)),
            c(rep(0, 5), rep(1, 3), rep(0, 2)),
            c(rep(0, 8), rep(1, 2))) #stock mat
brc <- c(rep(10, 9), 20) #barrier replacement cost vec

## Visualize
pdf("opt_syst.pdf")
vis(rep(0, nb), "a)")
dev.off()

# manager inputs
B <- 40 #budget
wh <- 0 #habitat weight
we <- 0 #equity weight
wd <- 1 #diversification weight

## optimization 
# objective function

habitat <- function(x) {sum(h * x)}

equity <- function(x) {
  1 - Gini(c(sum(h * x * tn[1, ]), sum(h * x * tn[2, ])))}

div <- function(x) {
  diversity(c(sum(h * x * s[1, ]), sum(h * x * s[2, ]), sum(h * x * s[3, ])), 
            index = "shannon")}

nl_obj <- function(x) {ifelse(sum(brc * x) - B > 0, -99999999, #budget constraint
                       ifelse(sum((diag(nb) - t(D)) %*% x - 1 + di > 0) > 0 , -99999999, #hydrography constraint 
                       wh * habitat(x) + we * equity(x) 
                       + wd * div(x)))} #multi-objective max with budget constraint

# solution 
soln1 <- genoud(nl_obj, nvars = nb, max = TRUE, Domains = cbind(rep(0, nb), rep(1, nb)),
                data.type.int = TRUE, pop.size = 3000)

pdf(ifelse(wh == 1, "opt_h.pdf", ifelse(we == 1, "opt_e.pdf", "opt_d.pdf")))
vis(soln1$par, ifelse(wh == 1, "b)", ifelse(we == 1, "c)", "d)")))
dev.off()
