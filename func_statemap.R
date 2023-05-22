# func_statemap

state_map <- function(map_data, color_var, color, legend_name, area_name) {
  ggplot(map_data, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = color_var), color = "grey50") +
    ggtitle(area_name) +
    borders("state", area_name, colour = "black") +
    scale_fill_gradient(
      name = legend_name,
      high = color,
      low = "white",
      na.value = "grey50"
    ) +
    coord_quickmap() +
    theme_void() +
    theme(
      plot.title = element_text(size = 30, hjust = 0.5),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15),
      legend.key.width = unit(.7, "cm"),
      legend.key.height = unit(.7, "cm")
    )
}
