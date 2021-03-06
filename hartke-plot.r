#!/usr/bin/env RScript

#Load Library
library(ggplot2)
library(MASS)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (tio2 or srtio3)", call.=FALSE)
}
if (args[1] != "tio2" & args[1] != "srtio3") {
  stop("Invalid argument. Must be tio2 or srtio3", call.=FALSE)
}
files <- list.files(pattern = "\\.(txt)$") 

# Set min energy for TiO2
if (args[1] == "tio2") {
    energy <- -39.80035761
    ave_energy <- -622.52807
    emin <- energy * 16.0
    etol <- 1e-2
} else if (args[1] == "srtio3") {
# OR
# Set min energy for TiO2
    energy <- -150.03120626
    ave_energy <- -1484.52192
    emin <- energy * 10.0
    etol <- 1.0
} else {
  stop("Invalid argument. Must be tio2 or srtio3", call.=FALSE)
}

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
#            if (run$status[i] != "Killed" & abs(emin) - abs(run$enthalpy[i]) > -etol & abs(run$enthalpy[i]) > 10) {
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
#    df <- data.frame(x=x, y=y, f=f)
#    ggplot(data = df, aes(x = x, y = f)) +
#        geom_line() +
#        theme_bw()
#    ggsave(paste("run", nRuns,"-results.png", sep = ""))
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

e_0 <- ave_energy - emin

hartke <- data.frame(indfs, minfs, maxfs, avefs)

#Average Best fit Function test
#favg <- function(x) {
#    e_0 * (exp(-0.01*x^0.7)) + emin
#}

# Plot average
#df <- data.frame(x=xs, y=ys, f=fs)
#avg <- ggplot(data = hartke) +
#    geom_line(data = hartke, aes(x = indfs, y = minfs, color = "Best-best structure"), size = 1) +
#    geom_line(data = hartke, aes(x = indfs, y = maxfs, color = "Worst-best structure"), size = 1) +
#    geom_line(data = hartke, aes(x = indfs, y = avefs, color = "Average-best structure"), size = 1) +
#    stat_function(data = hartke, fun = favg, aes(x = indfs, y = avefs)) +
#    theme_bw()
#ggsave(paste("average-results.png", sep = ""))

if (args[1] == "tio2") {
    coeffs <- c(-1,1)
} else if (args[1] == "srtio3") {
    coeffs <- c(-0.01,0.7)
}

fit <- nls(formula = avefs ~ e_0 * (exp(a*indfs^b)) + emin,
        data = hartke,
        start = list(a=coeffs[1], b=coeffs[2]),
        nls.control(maxiter = 5000),
)
coeffs <- coef(fit)

#Using best fit function coefficients
#Calculate Halflife
f_hf <- function(x) {
    coeffs[1] * x ^ coeffs[2] - log(1.0/2.0)
}

if (emin > min(minfs)) {
    print("WARNING: Emin > found Emin")
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
    e_0 * (exp(coeffs[1] * x ^ coeffs[2]))
}

guess <- 500
diff <- 1e-3
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

firstdone_val <- 10000
for (i in 1:length(firstdone)) {
    if (firstdone[i] < firstdone_val && firstdone[i] != 0) {
        firstdone_val <- firstdone[i]
    }    
}

firstdone_corrected <- c()
for (i in 1:length(firstdone)) {
    if (firstdone[i] != 0) {
        firstdone_corrected[length(firstdone_corrected) + 1] <- firstdone[i]
    } 
}

est_done <- mean(firstdone_corrected)
std_dev <- sd(firstdone_corrected)
err_mean <- std_dev / sqrt(length(firstdone_corrected))

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
        method.args = list(start=list(a=coeffs[1],b=coeffs[2])),
        se=FALSE
    ) +
    geom_point(x = hf, y = hfenergy, size = 7, aes(shape = "Halflife"), show.legend = TRUE) +
    scale_colour_manual(
        values=c("Best-best structure" = "#FF9600", 
            "Worst-best structure" = "#005199", 
            "Average-best structure" = "#686868",
            "Fitted exponential" = "#000000"),
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
        legend.position=c(0.66,0.7),
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
    labs(x="Structure number", y="Enthalpy (eV)")

if (args[1] == "tio2") {
    hartkeplot <- hartkeplot +
        scale_x_continuous(limits = c(0,NA), expand = c(0, 0)) +
        scale_y_continuous(limits = c(emin-1,-613.0), expand = c(0, 0))
} else if (args[1] == "srtio3") {
    hartkeplot <- hartkeplot +
        scale_x_continuous(limits = c(0, NA), expand = c(0, 0)) +
        scale_y_continuous(limits = c(emin-1, -1475.0), expand = c(0, 0))
}

ggsave("hartke-plot.pdf", dpi=300, width = 12, height = 10, units = "cm")
ggsave("hartke-plot.eps", dpi=300, width = 12, height = 10, units = "cm")
ggsave("hartke-plot.png", dpi=300, width = 12, height = 10, units = "cm")

total <- killed + duplicate + supercell + optimized

cat(paste("Energy tolerance =", etol), file = "summary", sep = "\n")
cat(paste("Energy minimum =", emin), file = "summary", sep ="\n", append = TRUE)
cat(paste("Coefficients: a =", coeffs[1], "b =", coeffs[2]), file = "summary", sep = "\n")
cat("\n", file = "summary", append = TRUE)
cat(paste("Number of runs =", nRuns), file = "summary", sep = "\n", append = TRUE)
cat(paste("Number of runs completed =", finished), file = "summary", sep = "\n", append = TRUE)
cat(paste("Total of Structures =", total), file = "summary", sep = "\n", append = TRUE)
cat(paste("Percent of Runs Completed =", finished/nRuns*100, "%"), file = "summary", sep = "\n", append = TRUE)
cat("\n", file = "summary", append = TRUE)
cat(paste("Halflife =", hf), file = "summary", sep = "\n", append = TRUE)
cat("\n", file = "summary", append = TRUE)
cat(paste("Calc. Finish by structure =", est), file = "summary", sep = "\n", append = TRUE)
cat(paste("Avg. Finish by structure =", est_done), file = "summary", sep = "\n", append = TRUE)
cat(paste("Std. Dev. =", std_dev), file = "summary", sep = "\n", append = TRUE)
cat(paste("Error =", err_mean), file = "summary", sep = "\n", append = TRUE)
cat("\n", file = "summary", append = TRUE)
cat(paste("Best Finish by structure =", firstdone_val), file = "summary", sep = "\n", append = TRUE)
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
