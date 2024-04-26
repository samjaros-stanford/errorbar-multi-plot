require(ggplot2)
require(ggpubr)
require(tidyverse)

bar_fill_color = "#648fff"
bar_outline_color = "white"
x_labels = c(
  "ICEinc" = "Income",
  "ICErace" = "Race",
  "ICEwbinc" = "Income + Race",
  "ICEwnhinc" = "Income + Race\\Ethnicity",
  "ICEhome" = "Home Ownership"
)

plot_data = read.csv("hist_plot_data.csv") %>%
  select(-zip, -GEOID) %>%
  pivot_longer(cols = everything(),
               names_to = "ICE_var",
               values_to = "ICE_value")

make_hist_plot = function(ICE_filter){
  ggplot(filter(plot_data, ICE_var==ICE_filter), aes(x=ICE_value, y=after_stat(density))) +
    # Define data display
    geom_histogram(bins=11, color=bar_outline_color, fill=bar_fill_color) +
    scale_x_continuous(name=x_labels[ICE_filter], limits=c(-1,1)) +
    scale_y_continuous(limits=c(0,2)) +
    # Define display options
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size=9),
          plot.margin = margin(t=10,r=5, l=10))
}

ice_vars = c("ICErace", "ICEinc", "ICEwbinc", "ICEwnhinc", "ICEhome")

combined_hist = ggarrange(plotlist=lapply(ice_vars, make_hist_plot))

final_figure = annotate_figure(combined_hist, 
                               left=text_grob("Density", rot=90, face ="bold", size=11),
                               bottom=text_grob("ICE Domain", face="bold", size=11))
  #draw_label(expression(bold("Figure 2.")~"Boxplots of Index of Concentration at the Extremes of"), size= 10, x=0.5, y=0.985) +
  #draw_label("residential census tracts among VA Nursing Home Residents in FY2007-2018", size=10, x=0.5, y=0.963)
  #draw_label("Level in Relation to the Index of Concentration at the Extremes", size=10, x=0.5, y=0.953)

ggsave("hist_plot.png", final_figure,
       width=2000, height=1200, unit="px")


