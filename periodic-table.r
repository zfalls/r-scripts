#!/usr/bin/env Rscript

#LOAD Library
library(ggplot2)

pt <- read.table("./periodic-table.dat")
colnames(pt) = c("symb", "row", "col", "tc", "h")

plot <- ggplot(pt, aes(pt$col, pt$row, label=pt$symb))+
    scale_y_reverse( lim=c(13,-3)) +
    scale_x_continuous( lim=c(1,18)) +
    theme_bw() +
    geom_point(shape=22, size=10, stroke = 2, aes(fill=pt$tc, color=pt$h)) +
    geom_text() +
    theme(
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.direction="horizontal",
        legend.position=c(0.5,0.2),
        legend.box = "horizontal",
        legend.text.align = 0.65,
        legend.key.width = unit(0.85, "cm"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.title.align = 1,
        legend.text = element_text(colour="black", size=10, face="bold"),
        legend.background = element_rect(fill="white",
                                  size=0.5, linetype="solid", 
                                  colour ="black"),
    ) +
    scale_fill_gradient2(limits=c(0,50), low="blue",mid="green",high="red", midpoint=25, na.value="white") + 
    scale_color_gradient2(limits=c(0.00,1.00), low="purple",mid="orange",high="black", midpoint=.5, na.value="white") + 
    labs(fill = expression('T'['c']),
        color = expression("Mol. Frac of H"[2])
    ) +
    
png("periodic-table.png")
plot
