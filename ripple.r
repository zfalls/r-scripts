#!/usr/bin/env Rscript

#LOAD LIbrary
library(rgl)

box <- data.frame(
    x = c(0.95,0.05,0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.05,0.05,0.05,0.95,0.95,0.95,0.05,0.95,0.05,0.05,0.05,0.05,0.05,0.05,0.05),
    y = c(0.95,0.95,0.95,0.95,0.95,0.05,0.95,0.05,0.95,0.95,0.95,0.95,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.95,0.05,0.95),
    z = c(0.95,0.95,0.95,0.05,0.95,0.95,0.05,0.05,0.05,0.05,0.95,0.05,0.95,0.05,0.95,0.95,0.05,0.05,0.05,0.95,0.95,0.95,0.05,0.05)
)
dir.create("animation")
par3d(windowRect = c(0, 50, 800, 800),
    userMatrix = rotationMatrix(-30*pi/180, 0, 1, 0.5) %*% par3d("userMatrix"))
totalFrames <- 20
for (frame in 0:totalFrames) {
    rho <- frame*0.01
    plot3d(0.05, 0.05, 0.05,
        col = "white", 
        xlab="", ylab="", zlab="", 
        xlim=c(-0.5,1.5),
        ylim=c(-0.5,1.5),
        zlim=c(-0.5,1.5),
        axes = FALSE)
    segments3d(box$x, box$y, box$z, col = "black")
    for (i in 1:9) {
            a <- i*0.1
        for (j in 1:9) {
            b <- j*0.1
            for (k in 1:9) {
                c <- (k*0.1) + rho*cos(1*a*2*pi)*cos(1*b*2*pi)
                if (k %% 2 == 0)
                    plot3d(a, b, c, type = "s", radius = .05, col = "blue", 
                    add = TRUE, 
                    xlab="", ylab="", zlab="", 
                    axes = FALSE)
                if (k %% 2 != 0)
                    plot3d(a, b, c, type = "s", radius = .05, col = "red", 
                    add = TRUE, 
                    xlab="", ylab="", zlab="", 
                    axes = FALSE)
            }
        }
    }
    newFrame <- totalFrames*2 - frame
    rgl.snapshot(filename=paste("animation/ripple-",sprintf("%03d", frame), ".png", sep=""))
    system(paste("convert -crop 400x400+200+175 +repage animation/ripple-",sprintf("%03d", frame), ".png animation/ripple-",sprintf("%03d", frame), ".png", sep=""))
    file.copy(paste("animation/ripple-",sprintf("%03d", frame),".png", sep=""), paste("animation/ripple-", sprintf("%03d", newFrame), ".png", sep=""))
    clear3d()
}
system(paste("convert -delay 1 animation/*.png animation/ripple.gif", sep=""))
