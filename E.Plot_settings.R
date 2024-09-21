# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 20/09/2024

" This file contains the settings for the theme and color fill for the plots in 
the paper
"

custom_theme <- function(leg_position, leg_direction) {
  theme_light() +
    theme(plot.title = element_text(size = 10),
          text = element_text(size = 10),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
          legend.text = element_text(size = 10),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), units = 'cm'),
          legend.position = leg_position,
          legend.direction = leg_direction,
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          legend.spacing = unit(0.1, "cm"))
}

custom_fill_log <- function(nbreaks, breaks, labels, limit, title, 
                            barwidth, barheight) {
  breaks <- breaks
  scale_fill_viridis_c(option = "G",
                       direction = -1,
                       begin = 0, 
                       end = 1,
                       na.value = "grey",
                       trans = pseudo_log_trans(sigma = 10^(-nbreaks), base = 10),
                       breaks = breaks,
                       labels = labels,
                       limits = c(0, limit),
                       guide = guide_colourbar(title = title,
                                               title.position = "top",
                                               title.hjust = 0.5,
                                               title.vjust = 2,
                                               barwidth = unit(barwidth, "cm"),
                                               barheight = unit(barheight, "cm")))
}

custom_fill_lin <- function(title, barwidth, barheight) {
  scale_fill_viridis_c(option = "G",
                       direction = -1,
                       begin = 0, 
                       end = 1,
                       na.value = "grey",
                       guide = guide_colourbar(title = title,
                                               title.position = "top",
                                               title.hjust = 0.5,
                                               title.vjust = 2,
                                               barwidth = unit(barwidth, "cm"),
                                               barheight = unit(barheight, "cm")))
}
