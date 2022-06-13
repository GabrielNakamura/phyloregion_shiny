## zoom in and out phylogenies 

plot_interact <- 
  function(tree,  height, width, type = "circular", tip.label = FALSE){
    plot_tree <- ggtree::ggtree(tree, layout = type) +
      theme_tree2()
    plot_tree %>% plotly::ggplotly(height = height, width = width)
} 

# diversity calculation

plot_div <-
  function(comm, tree, type){
    if(type == "PE"){
      res <- phylo_endemism(comm, tree)
    }
    if(type == "WPE"){
      res <- weighted_endemism(comm)
    }
    if(type == "PD"){
      res <- PD(x = comm, phy = tree)
    }
    return(res)
  }


