## zoom in and out phylogenies 

library(phyloregion)
library(magrittr)
library(plotly)
library(ggtree)
library(ggplot2)
data("africa")
tree <- africa$phylo
tree$tip.label




phylo <- africa$phylo
tree <- phylo
plot_interact <- 
  function(tree,  height, width, type = "circular", tip.label = FALSE){
    plot_tree <- ggtree::ggtree(tree, layout = type)
    plot_tree <- ggtree::ggtree(tree, layout = "rectangular")
    plot_tree %>% plotly::ggplotly(height = height, width = width)
} 
plot_interact(tree = phylo, 
              type = "circular",
              tip.label = FALSE)
plot_tree <- ggtree::ggtree(tree, layout = "circular")
plot_tree %>% plotly::ggplotly()
help(package = "phyloregion")
