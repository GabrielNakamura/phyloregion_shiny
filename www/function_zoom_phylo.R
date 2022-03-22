## zoom in and out phylogenies 

plot_interact <- 
  function(tree,  height, width, type = "circular", tip.label = FALSE){
    plot_tree <- ggtree::ggtree(tree, layout = type)
    plot_tree <- ggtree::ggtree(tree, layout = "rectangular") +
      theme_tree2()
    plot_tree %>% plotly::ggplotly(height = height, width = width)
} 
