#!/usr/bin/env RScript

#Load Library
library(ggplot2)

files <- list.files(pattern = "\\.(txt)$") 

#Set min energy for TiO2
etol <- 1e-3
tio2_energy <- -39.80035761
tio2_ave_energy <- -622.52807
emin <- tio2_energy * 16.0

xs <- data.frame()
ys <- data.frame()
fs <- data.frame()
firstdone <- c()
finished <- 0

killed <- 0
duplicate <- 0
supercell <- 0
optimized <- 0

nRuns <- 0

for (file in files) {
    x <- c()
    y <- c()
    lowest = 0
    done = FALSE

    print(paste("Reading file", file))
    nRuns <- nRuns + 1    
    temp <- read.table(file, header=FALSE, sep="\t", skip = 1, nrows = 600)
    run <- data.frame(temp$V1, temp$V2, temp$V3, temp$V4, temp$V8)
    colnames(run) = c("index", "gen", "id", "enthalpy", "status")
    
    for (i in 1:nrow(run)) {
        if (run$gen[i] != 1) {
#            if (run$status[i] != "Killed" && abs(emin) - abs(run$enthalpy[i]) > -etol && abs(run$enthalpy[i]) > 10) {
                x <- c(x, run$index[i])
                y <- c(y, run$enthalpy[i])
#            }
            if (done == FALSE) {
                if (abs(emin - run$enthalpy[i]) < etol) {
                    lowest <- i
                    finished <- finished + 1
                    done <- TRUE
                    optimized <- optimized+1    
                } else {
                    if (run$status[i] == "Killed") killed <- killed+1    
                    if (run$status[i] == "Duplicate") duplicate <- duplicate+1    
                    if (run$status[i] == "Supercell") supercell <- supercell+1    
                    if (run$status[i] == "Optimized") optimized <- optimized+1    
                }
            }
        }
    }
    
    firstdone <- c(firstdone, lowest)
    t <- y[1]
    f <- c()

    for (i in 1:length(y)) {
        if (y[i] < t) t = y[i]
        f <- c(f, t)
    }
    
    if (nRuns == 1) {
        xs <- data.frame(x)
        ys <- data.frame(y)
        fs <- data.frame(f)
    } else {
        xs <- cbind(xs, x)
        ys <- cbind(ys, y)
        fs <- cbind(fs, f)
    }

    # Plot each run summary
    #df <- data.frame(x=x, y=y, f=f)
    #ggplot(data = df, aes(x = x, y = f)) +
    #    geom_line() +
    #    theme_bw()
    #ggsave(paste("run", nRuns,"-results.png", sep = ""))
}

indfs <- c()
minfs <- c()
maxfs <- c()
avefs <- c()

index <- 1

for (point in 1:nrow(xs)) {
    avef <- 0.0
    minf <- 1000000.0
    maxf <- -1000000.0
    count <- 0.0
    for (run in 1:ncol(xs)) {
        tf <- fs[point,run]
        avef <- avef + tf
        count <- count + 1
        if (minf > tf) minf <- tf
        if (maxf < tf) maxf <- tf
    }
    avef <- avef / count
    indfs <- c(indfs, index)
    minfs <- c(minfs, minf)
    maxfs <- c(maxfs, maxf)
    avefs <- c(avefs, avef)
    for (run in 1:ncol(xs)) {
        tf <- fs[point,run]
    }
    index <- index + 1
}

e_0 <- tio2_ave_energy - emin

hartke <- data.frame(indfs, minfs, maxfs, avefs)

fit <- nls(formula = avefs ~ e_0 * (exp(a*indfs^b)) + emin,
        data = hartke,
        start = list(a=-1, b=1))
coeffs <- coef(fit)

#Using best fit function coefficients
#Calculate Halflife
f_hf <- function(x) {
    coeffs[1] * x ^ coeffs[2] - log(1.0/2.0)
}

tol <- 1e-10
guess <- 100
diff <- 1e-1

x <- guess
val <- f_hf(x)
while (abs(val) > tol) {
    x <- abs(x)
    dx <- (f_hf(x+diff) - val) / diff
    x <- x - (val / dx)
    
    x <- abs(x)
    val <- f_hf(x)
}

hf <- x
hfenergy <- e_0 * (exp(coeffs[1] * hf ^ coeffs[2])) + emin


#Estimated finish value for runs that did not complete
f_est <- function(x) {
    e_0 * (exp(coeffs[1] * x ^ coeffs[2])) - cutoff
}

guess <- 10
diff <- 1e-5
cutoff <- 0.01
x <- guess
val <- f_est(x)
while (abs(val) > tol) {
    dx <- (f_est(x+diff) - val) / diff
    x <- x - (val / dx)
    x <- abs(x)
    if (x == Inf) {
        x <- 1e10
        print(paste("Cannot converge, set to 1e10"))
        break
    }
    val = f_est(x)
    if (is.nan(val)) {
        val = 1
    }
}
est <- x
for (i in length(firstdone)) {
    if (firstdone[i] == 0)
        firstdone[i] = est
}

est_done <- mean(firstdone)


#Plot the Hartke Plot
hartkeplot <- ggplot(hartke) +
    geom_line(data = hartke, aes(x = indfs, y = minfs, color = "Best-best structure"), size = 1) +
    geom_line(data = hartke, aes(x = indfs, y = maxfs, color = "Worst-best structure"), size = 1) +
    geom_line(data = hartke, aes(x = indfs, y = avefs, color = "Average-best structure"), size = 1) +
    geom_smooth(data = hartke, 
        aes(x = indfs, y = avefs, color = "Fitted exponential"), 
        linetype = 2,
        size = 1,
        method = "nls",
        formula = 'y ~ e_0 * (exp(a*x^b)) + emin',
        method.args = list(start=list(a=-1,b=1)),
        se=FALSE
    ) +
    geom_point(x = hf, y = hfenergy, size = 7, aes(shape = "Halflife"), show.legend = TRUE) +
    scale_colour_manual(
        values=c("Best-best structure" = "green", 
            "Worst-best structure" = "blue", 
            "Average-best structure" = "red",
            "Fitted exponential" = "black"),
            breaks=c("Best-best structure", 
            "Worst-best structure", 
            "Average-best structure",
            "Fitted exponential")
    ) +
    scale_shape_manual(
        values=c("Halflife" = 4),
        breaks=c("Halflife")
    ) +
    guides(colour = guide_legend(override.aes = list(shape=32, size = 1)), 
        shape = guide_legend(override.aes = list(shape=4, size=5))
    ) +
    theme_bw() +
    theme(
        legend.position=c(0.8,0.8),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(
                                  size=1, linetype = "solid",
                                  colour = "black"),
        legend.text = element_text(colour = "black", size=14),
        legend.background = element_rect(fill = "white",
                                  size=0.5, linetype = "solid",
                                  colour = "black"),
        axis.title = element_text(colour = "black", size=16),
        axis.text = element_text(colour = "black", size=14),
    ) +
    labs(x="Structure number", y="Enthalpy (eV)") +
    scale_x_continuous(limits = c(0,NA), expand = c(0, 0)) +
    scale_y_continuous(limits = c(NA,NA), expand = c(0.01, 0.01)) +

ggsave("hartke-plot.pdf", dpi=300, width = 20, height = 15, units = "cm")
ggsave("hartke-plot.eps", dpi=300, width = 20, height = 15, units = "cm")
ggsave("hartke-plot.png", dpi=300, width = 20, height = 15, units = "cm")

total <- killed + duplicate + supercell + optimized

cat(paste("Energy tolerance =", etol), file = "summary", sep = "\n")
cat(paste("Energy minimum for 16 FU TiO2 =", emin), file = "summary", sep ="\n", append = TRUE)
cat("\n", file = "summary", append = TRUE)
cat(paste("Number of runs =", nRuns), file = "summary", sep = "\n", append = TRUE)
cat(paste("Total of Structures =", total), file = "summary", sep = "\n", append = TRUE)
cat(paste("Percent of Runs Completed =", finished/total, "%"), file = "summary", sep = "\n", append = TRUE)
cat("\n", file = "summary", append = TRUE)
cat(paste("Halflife =", hf), file = "summary", sep = "\n", append = TRUE)
cat(paste("Finish by structure =", est_done), file = "summary", sep = "\n", append = TRUE)
cat("\n", file = "summary", append = TRUE)
cat(paste("Number of structures killed =", killed), file = "summary", sep = "\n", append = TRUE)
cat(paste("Number of duplicate structures =", duplicate), file = "summary", sep = "\n", append = TRUE)
cat(paste("Number of supercell structures =", supercell), file = "summary", sep = "\n", append = TRUE)
cat(paste("Number of optimized structures =", optimized), file = "summary", sep = "\n", append = TRUE)
cat("\n", file = "summary", append = TRUE)
cat(paste("Percent of structures killed =", killed/total*100, "%"), file = "summary", sep = "\n", append = TRUE)
cat(paste("Percent of duplicate structures =", duplicate/total*100, "%"), file = "summary", sep = "\n", append = TRUE)
cat(paste("Percent of supercell structures =", supercell/total*100, "%"), file = "summary", sep = "\n", append = TRUE)
cat(paste("Percent of optimized structures =", optimized/total*100, "%"), file = "summary", sep = "\n", append = TRUE)
