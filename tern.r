#!/usr/bin/env Rscript


#Load library
library(ggtern)
require(ggplot2)

#Load the data
df <- read.table("./rplot.dat")
colnames(df) = c("x","y","Value")
dft <- data.frame(Value=df$Value)
df$Value <- NULL
df$x <- df$x / 100
df$y <- df$y / 100
crd = coord_tern()
fwd = xy2tlr(df,crd)
fwd$Value <- dft$Value
colnames(fwd) = c("Li","H","F","Value")

plot <- ggtern(data = fwd,aes(x=Li,y=H,z=F,value=Value)) + 
    stat_interpolate_tern(
        n=400,
        bins=1000,
        binwidth=10,
        base='identity',
        aes(color=..level..,value = Value),
        na.rm = TRUE
    ) + 
    geom_interpolate_tern(
        aes(value=Value,color=..level..),
        binwidth = 1,
        bins=10,
        color='black',
        base='identity',
    ) +
    scale_T_continuous(breaks=.5,labels="HF") +
    scale_L_continuous(breaks=.5,labels="LiH") +
    scale_R_continuous(breaks=.5,labels="LiF") +
    theme_bw() +
    theme_showarrows() +
    theme_gridsonbottom() +
    percent_custom("Mol. %") +
    theme_hideprimary() +
    theme_hidesecondary() +
    theme(axis.title=element_text(size=16),
        axis.text=element_text(size=16),
        tern.axis.text.T=element_text(vjust=-.1),
        tern.axis.text.L=element_text(angle=60,hjust=.9),
        tern.axis.text.R=element_text(angle=-60,vjust=1),
        legend.justification=c(0,1), 
        legend.direction="vertical",
        legend.position=c(0,1),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.key.height = unit(.85, "cm")
    ) +
    geom_point(aes(fill=Value),color="black",size=5,shape=21) + 
    scale_color_gradient2(low="blue",mid="white",high="red",midpoint=-2.5) + 
    scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=-2.5) + 
    guides(color = guide_colorbar(order=1),fill="none",alpha="none") +
    labs(color = "Enthalpy (eV/atom)")
png("tern.png")
plot
