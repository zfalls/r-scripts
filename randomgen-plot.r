#!/usr/bin/env RScript

#Load Library
library(ggplot2)

files <- list.files(pattern = "\\.(txt)$")

tio2_energy <- -39.80035761
emin <- tio2_energy * 16.0
etol <- 1e-3

enthalpy <- c()
genmin <- 0
total <- 0

for (file in files) {
    print(paste("Reading file", file))
    temp <- read.table(file, header=FALSE, sep="\t", skip = 1)
    run <- data.frame(temp$V4)
    colnames(run) = c("enthalpy")
    enthalpy <- c(enthalpy, run$enthalpy)
    for (i in 1:nrow(run)) {
        total <- total + 1
        if (abs(emin - run$enthalpy[i]) < etol) {
            genmin <- genmin + 1
        }
    }
}

print(paste(genmin, "Of", total, "Total Structures were Minimum"))
print(paste(genmin/total*100.0,"%", sep=""))
gen <- data.frame(enthalpy)
randomgen <- ggplot(gen, aes(x=enthalpy)) +
    geom_histogram(binwidth=0.1, data=subset(gen, enthalpy<=emin), aes(fill="Rutile"), color = "black") +
    geom_histogram(binwidth=0.1, data=subset(gen, enthalpy>emin), aes(fill="Other"), color = "black") +
    scale_x_continuous(limits = c(emin-1,NA), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,NA), expand = c(0, 0)) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(
                                  size=1, linetype = "solid",
                                  colour = "black"),
        axis.title = element_text(colour = "black", size=16),
        axis.text = element_text(colour = "black", size=14),
        legend.position=c(0.1,0.9),
        legend.title=element_blank(),
        legend.text = element_text(colour = "black", size=14),
        legend.background = element_rect(fill = "white",
                                  size=0.5, linetype = "solid",
                                  colour = "black"),
    ) +
    scale_fill_manual(values=c("Rutile" = "red", "Other" = "blue"),
        breaks=c("Rutile")
    ) +
    #guides(fill = guide_legend(override.aes = list(size = 5))) +
    labs(x="Enthalpy (eV)", y = "Number of Structures") +

ggsave("randomgen-plot.pdf", dpi=300, width = 20, height = 15, units = "cm")
ggsave("randomgen-plot.eps", dpi=300, width = 20, height = 15, units = "cm")
ggsave("randomgen-plot.png", dpi=300, width = 20, height = 15, units = "cm")
