visualize.na.values <- function(database, label = NULL){

  title <- ggdraw() +
    draw_label(paste("Quality Control:", label),fontface = 'bold',x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))

  # Plot the NA with percentage
  plot_1 <- vis_miss(database)

  # Plot NA per group
  plot_2 <- gg_miss_var(database, facet = Month)

  # Create a cowplot
  out.plot <- cowplot::plot_grid(plot_1, plot_2)

  final_plot <- cowplot::plot_grid(title, out.plot, ncol = 1, rel_heights = c(0.1,1))

  return(final_plot)
}

visualize.na.values.without.groups <- function(database, label = NULL){
  title <- ggdraw() +
    draw_label(paste("Quality Control:", label),fontface = 'bold',x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))

  plot_1 <- vis_miss(database)

  plot_2 <- gg_miss_var(database)

  out.plot <- cowplot::plot_grid(plot_1, plot_2)

  final_plot <- cowplot::plot_grid(title, out.plot, ncol = 1, rel_heights = c(0.1,1))

  return(final_plot)
}

