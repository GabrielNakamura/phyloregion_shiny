## zoom in and out phylogenies 

library(phyloregion)
library(magrittr)
library(plotly)
library(ggtree)
library(ggplot2)
data("africa")
tree <- africa$phylo
tree$tip.label





plot_interact <- 
  function(tree, type = "circular", tip.label = FALSE){
    plot_tree <- ggtree::ggtree(tree, layout = type)
} 

plot_tree <- ggtree::ggtree(tree, layout = "circular")
plot_tree %>% plotly::ggplotly()
help(package = "phyloregion")
