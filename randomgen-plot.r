#!/usr/bin/env RScript

#Load Library
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (tio2 or srtio3)", call.=FALSE)
}
if (args[1] != "tio2" & args[1] != "srtio3") {
  stop("Invalid argument. Must be tio2 or srtio3", call.=FALSE)
}

files <- list.files(pattern = "\\.(txt)$")

if (args[1] == "tio2") {
    energy <- -39.80035761
    emin <- energy * 16.0
    etol <- 1e-3
} else if (args[1] == "srtio3") {
    energy <- -150.03120626
    emin <- energy * 10.0
    etol <- 1.0 
}

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
        if (abs(emin - run$enthalpy[i]) <= etol) {
            genmin <- genmin + 1
        }
    }
}

gen <- data.frame(enthalpy)

if (args[1] == "tio2") {
    randomgen <- ggplot(gen, aes(x=enthalpy)) +
        geom_histogram(binwidth=0.1, 
            data=subset(gen, abs(emin - enthalpy)<=etol), 
            aes(fill="Rutile"), 
            #size = 0.2, 
            #color = "black"
        )
} else if (args[1] == "srtio3") {
    randomgen <- ggplot(gen, aes(x=enthalpy)) +
        geom_histogram(binwidth=0.1, 
            data=subset(gen, abs(emin - enthalpy)<=etol), 
            aes(fill="Perovskite"), 
            #size = 0.2, 
            #color = "black"
        )
}

randomgen <- randomgen +
    geom_histogram(binwidth=0.1, 
        data=subset(gen, abs(emin - enthalpy)>etol), 
        aes(fill="Other"), 
        #size = 0.2, 
        #color = "black"
    ) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(
                                  size=1, linetype = "solid",
                                  colour = "black"),
        axis.title = element_text(colour = "black", size=16),
        axis.text = element_text(colour = "black", size=14),
        legend.position=c(0.8,0.2),
        legend.title=element_blank(),
        legend.text = element_text(colour = "black", size=14),
        legend.background = element_rect(fill = "white",
                                  size=0.5, linetype = "solid",
                                  colour = "black"),
    ) +
    #guides(fill = guide_legend(override.aes = list(size = 5))) +
    labs(x="Enthalpy (eV)", y = "Number of Structures")

if (args[1] == "tio2") {
    randomgen <- randomgen +
        scale_fill_manual(values=c("Rutile" = "#FF9600", "Other" = "#005199"),
            breaks=c("Rutile")
        ) +
        scale_y_continuous(limits = c(0,NA), expand = c(0, 0)) +
        scale_x_continuous(limits = c(emin-1,-613.0), expand = c(0, 0)) +
        coord_flip()
} else if (args[1] == "srtio3") {
    randomgen <- randomgen +
        scale_fill_manual(values=c("Perovskite" = "#FF9600", "Other" = "#005199"),
            breaks=c("Perovskite")
        ) +
        scale_y_continuous(limits = c(0,NA), expand = c(0, 0)) +
        scale_x_continuous(limits = c(emin-1,-1475.0), expand = c(0, 0)) +
        coord_flip()
}

ggsave("randomgen-plot.pdf", dpi=300, width = 12, height = 10, units = "cm")
ggsave("randomgen-plot.eps", dpi=300, width = 12, height = 10, units = "cm")
ggsave("randomgen-plot.png", dpi=300, width = 12, height = 10, units = "cm")

cat(paste("Number of Min. Structures =", genmin), file = "summary", sep = "\n")
cat(paste("Total Structures =", total), file = "summary", sep = "\n", append = TRUE)
cat(paste("Percent Min. =", genmin/total*100.0,"%"), file = "summary", sep="\n", append = TRUE)
