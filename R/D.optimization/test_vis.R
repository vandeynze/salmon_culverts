#######################################
# Test optimal barrier removal problem:
# Graphing the test problem
# Author: Sunny Jardine
# Date: February, 2021
# Refs: https://guangchuangyu.github.io/ggtree-book/chapter-ggtree.html
#######################################

#if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
#BiocManager::install(version = "3.12")
#BiocManager::install("ggtree")

library(tidyverse)
library(ggtree)

set.seed(2017)
tree <- rtree(4)
x <- as_tibble(tree)

ggtree(tree, layout="slanted")


