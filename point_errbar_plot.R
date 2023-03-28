require(ggplot2)
require(ggpubr)
require(tidyverse)

point_colors = c("#fe6100", "#648fff")
point_shapes = c(19, 17)
errbar_color = "black"
line_color = "#dc267f"
x_labels = c(
  "0ICEinc" = expression(ICE[inc]),
  "1ICErace" = expression(ICE[race]),
  "2ICEinc+wb" = expression(ICE[inc+wb]),
  "3ICEinc+wnh" = expression(ICE[inc+wnh]),
  "4ICEhome" = expression(ICE[home])
)
# Note: adding \n to a string starts a new line
y_labels = c(
  "hbp" = "High Blood Pressure\nRate Ratio",
  "sbp" = "Systolic Blood Pressure\nBeta Coefficient",
  "dbp" = "Diastolic Blood Pressure\nBeta Coefficient"
)

plot_data = read.csv("plot_data.csv") %>%
  mutate(variable = factor(variable)) %>%
  mutate(type = factor(type, levels=c("Unadjusted", "Adjusted")))

make_plot = function(outcome_filter){
  ggplot(filter(plot_data, outcome==outcome_filter), 
         aes(x=variable, y=rr, ymin=l_rr, ymax=u_rr, color=type, shape=type)) +
    # Draw data
    geom_hline(color=line_color, yintercept=1) +
    geom_errorbar(color=errbar_color, position=position_dodge(width=0.5), width=0.2) + 
    geom_point(position=position_dodge(width=0.5), size=3) +
    # Scale asthetics
    scale_color_manual(values=point_colors) +
    scale_shape_manual(values=point_shapes) +
    scale_x_discrete("ICE Measure", labels=x_labels) +
    scale_y_continuous(y_labels[outcome_filter]) +
    # Plot theme
    theme_minimal() +
    theme(legend.margin = margin(),
          legend.position = "top",
          legend.spacing = unit(0, "cm"),
          legend.title = element_blank())
}
  
outcomes = c("hbp", "sbp", "dbp")

combined_plot = ggarrange(plotlist=lapply(outcomes, make_plot), 
                          ncol=1, labels=c("a", "b", "c"))

ggsave("errorbar-multi-plot.png", combined_plot, 
       width=1200, height=2400, units="px", bg="white")
