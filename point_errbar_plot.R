require(cowplot)
require(ggplot2)
require(ggpubr)
require(tidyverse)

point_colors = c("#fe6100", "#648fff")
point_shapes = c(19, 17)
errbar_color = "black"
line_color = "#dc267f"
x_labels = c(
  "0ICEinc" = "Income",
  "1ICErace" = "Race",
  "2ICEinc+wb" = "Income +\nRace",
  "3ICEinc+wnh" = "Income +\nRace\\Eth",
  "4ICEhome" = "Home\nOwnership"
)
# Note: adding \n to a string starts a new line
y_labels = c(
  "hbp" = "High Blood Pressure\nRate Ratio",
  "sbp" = "Systolic Blood Pressure\nBeta Coefficient (mm Hg)",
  "dbp" = "Diastolic Blood Pressure\nBeta Coefficient (mm Hg)"
)

plot_data = read.csv("point_plot_data.csv") %>%
  mutate(variable = factor(variable)) %>%
  mutate(type = factor(type, levels=c("Unadjusted", "Adjusted")))

make_plot = function(outcome_filter){
  yint = ifelse(outcome_filter=="hbp",1,0)
  ggplot(filter(plot_data, outcome==outcome_filter), 
         aes(x=variable, y=rr, ymin=l_rr, ymax=u_rr, color=type, shape=type)) +
    # Draw data
    geom_hline(color=line_color, yintercept=yint) +
    geom_errorbar(color=errbar_color, position=position_dodge(width=0.5), width=0.2) + 
    geom_point(position=position_dodge(width=0.5), size=3) +
    # Scale asthetics
    scale_color_manual(values=point_colors) +
    scale_shape_manual(values=point_shapes) +
    scale_x_discrete("ICE Domain", labels=x_labels) +
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
                          nrow=1)

title = expression(atop(bold("Figure 1.")~"High, Systolic, and Diastolic Blood Pressure:", 
                        "Long-term VA Nursing Home Residents in FY2007-2018 at the Census", 
                        "Tract Level in Relation to the Index of Concentration at the Extremes"))

title = expression(paste(bold("Figure 1.")~"High, Systolic, and Diastolic Blood Pressure:\n 
                         Long-term VA Nursing Home Residents in FY2007-2018 at the Census\n 
                          "))

final_figure = annotate_figure(combined_plot, 
                               top=text_grob("", size=40)) +
  draw_label(expression(bold("Figure 1.")~"High, Systolic, and Diastolic Blood Pressure: Long-term"), size= 10, x=0.5, y=0.99) +
  draw_label("VA Nursing Home Residents in FY2007-2018 at the Census Tract", size=10, x=0.5, y=0.972) +
  draw_label("Level in Relation to the Index of Concentration at the Extremes", size=10, x=0.5, y=0.953)

ggsave("point-errbar-plot.png", combined_plot, 
       width=3500, height=900, units="px", bg=NA)
