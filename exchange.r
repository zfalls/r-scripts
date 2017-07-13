#!/usr/bin/env Rscript

#LOAD LIbrary
library(rgl)

dir.create("exchange", showWarnings = FALSE)
par3d(windowRect = c(0, 50, 800, 800),
    userMatrix = rotationMatrix(-30*pi/180, 0, 1, 0.5) %*% par3d("userMatrix"))
box <- data.frame(
    x = c(0.35,0.05,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.05,0.05,0.05,0.35,0.35,0.35,0.05,0.35,0.05,0.05,0.05,0.05,0.05,0.05,0.05),
    y = c(0.35,0.35,0.35,0.35,0.35,0.05,0.35,0.05,0.35,0.35,0.35,0.35,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.35,0.05,0.35),
    z = c(0.35,0.35,0.35,0.05,0.35,0.35,0.05,0.05,0.05,0.05,0.35,0.05,0.35,0.05,0.35,0.35,0.05,0.05,0.05,0.35,0.35,0.35,0.05,0.05)
)
totalFrames <- 20
for (frame in 0:totalFrames) {
    plot3d(0.05, 0.05, 0.05,
        col = "white", 
        xlab="", ylab="", zlab="", 
        xlim=c(-0.2,0.6),
        ylim=c(-0.2,0.6),
        zlim=c(-0.2,0.6),
        axes = FALSE)
    segments3d(box$x, box$y, box$z, col = "black")
    for (i in 1:3) {
        a <- i*0.1
        for (j in 1:3) {
            b <- j*0.1
            for (k in 1:3) {
                c <- k*0.1
                if (i == 2 && j == 1 && k == 1) {
                    c <- c + frame*0.005
                }
                if (i == 2 && j == 1 && k == 2) {
                    c <- c - frame*0.005
                }
                else b <- j*0.1
                if (k %% 2 == 0)
                    #plot3d(a, b, c, type = "s", radius = .05, col = "blue", 
                    plot3d(a, b, c, type = "s", radius = .05, col = "#005199", 
                    add = TRUE, 
                    xlab="", ylab="", zlab="", 
                    axes = FALSE)
                if (k %% 2 != 0)
                    #plot3d(a, b, c, type = "s", radius = .05, col = "red", 
                    plot3d(a, b, c, type = "s", radius = .05, col = "#FF9600", 
                    add = TRUE, 
                    xlab="", ylab="", zlab="", 
                    axes = FALSE)
            }
        }
    }
    newFrame <- totalFrames*2 - frame
    rgl.postscript(filename=paste("exchange/exchange-",sprintf("%03d",frame), ".pdf", sep=""), fmt="pdf")
    system(paste("convert -crop 400x400+200+175 +repage -sharpen 0x1.0 -quality 100 exchange/exchange-",sprintf("%03d", frame), ".pdf exchange/exchange-",sprintf("%03d", frame), ".pdf", sep=""))
    system(paste("convert -sharpen 0x1.0 -quality 100 exchange/exchange-",sprintf("%03d", frame), ".pdf exchange/exchange-",sprintf("%03d", frame), ".png", sep=""))
    if (frame != newFrame) {   
        file.copy(paste("exchange/exchange-",sprintf("%03d", frame),".png", sep=""), paste("exchange/exchange-", sprintf("%03d", newFrame), ".png", sep=""), overwrite = TRUE)
        file.copy(paste("exchange/exchange-",sprintf("%03d", frame),".pdf", sep=""), paste("exchange/exchange-", sprintf("%03d", newFrame), ".pdf", sep=""), overwrite = TRUE)
    }
    clear3d()
}
system(paste("convert -dispose previous -delay 1 exchange/*.png exchange/exchange.gif", sep=""))
