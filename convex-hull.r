#!/usr/bin/env Rscript

library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).dat", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  args[2] = "out.png"
}

colors <- c("white", "black", "red", "blue", "green", "orange", "purple")

#Load the data
df <- read.table(args[1])
numcols <- ncol(df)

plot <- ggplot(data=df)
for (i in 2:numcols){
    plot <- plot + geom_point(data = df, aes_(x = as.name(names(df)[1]), y = as.name(names(df)[i])), color=colors[i])
}

nrows <- nrow(df)
for (j in 2:numcols) {
seg <- 0
idx <- 1
curridx <- 1
nxtidx <- 2
lastpt <- 1
currpt <- 1
nxtpt <- 2
while (nxtidx <= nrows){
    slope <- as.double((df[[1]][[curridx]] - df[[j]][[nxtidx]]) / (df[[1]][[curridx]] - df[[j]][[nxtidx]]))
    for (i in nxtidx:nrows){
        slopetmp <- as.double((df[[j]][[curridx]] - df[[j]][[i]]) / (df[[1]][[curridx]] - df[[1]][[i]]))
        if (slopetmp <= slope){
            slope <- slopetmp
            idx <- i
        }
        if (i == nrows && slope == as.double((df[[1]][[curridx]] - df[[j]][[nxtidx]]) / (df[[1]][[curridx]] - df[[j]][[nxtidx]]))) {
            idx <- i
        }
    }
    if (curridx == 1){
        seg <- data.frame(x1=df[[1]][[curridx]], x2=df[[1]][[idx]], y1=df[[j]][[curridx]], y2=df[[j]][[idx]])
    } else {
        seg2 <- data.frame(x1=seg$x2[[lastpt]], x2=df[[1]][[idx]], y1=seg$y2[[lastpt]], y2=df[[j]][[idx]])
        seg <- rbind(seg, seg2)
    }
    curridx <- idx
    nxtidx <- idx + 1
    lastpt <- currpt
    currpt <- lastpt + 1
    nxtpt <- currpt + 1
}

nsegs <- nrow(seg)
for (i in 1:nsegs){
    plot <- plot + geom_segment(data = seg, x = seg$x1[[i]], y = seg$y1[[i]], xend = seg$x2[[i]], yend = seg$y2[[i]], color=colors[j])
}
}

colors <- c("black", "red", "blue", "green", "orange", "purple")

plot <- plot +
    geom_segment(aes(x=0,y=0,xend=1,yend=0), linetype=2) +
    theme_bw() +
    labs(x=expression("Mol. Frac. of H"[2]),
        y="Enthalpy (eV/atom)") +
    scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
    guides(color = guide_colorbar(order=1))

png(args[2])
plot
