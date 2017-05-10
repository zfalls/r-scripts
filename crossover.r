#!/usr/bin/env Rscript

#LOAD LIbrary
library(rgl)

dir.create("crossover")
par3d(windowRect = c(0, 50, 800, 800),
    userMatrix = rotationMatrix(-30*pi/180, 0, 1, .5) %*% par3d("userMatrix"))
totalFrames <- 20
for (frame in 0:totalFrames) {
    plot3d(0.05, 0.05, 0.05,
        col = "white", 
        xlab="", ylab="", zlab="", 
        xlim=c(-0.2,0.6),
        ylim=c(-0.2,0.6),
        zlim=c(-0.2,0.6),
        axes = FALSE)

    c3d <- cube3d()

    A <- matrix(c(0.2,0,0,0,0.2,0,0,0,0.2),3,3)
    c3d <- transform3d(c3d, A)
    c3d <- translate3d(c3d,0.25,0.25,0.25)

    c3d_trans <- translate3d(c3d,1,0.5,0)
    for (i in 1:6) {
        lines3d(t(c3d_trans$vb)[c3d$ib[,i],])
        lines3d(t(c3d$vb)[c3d$ib[,i],])
    }
    for (i in 1:4) {
        a <- i*0.1
        for (j in 1:4) {
            b <- j*0.1
            for (k in 1:4) {
                c <- k*0.1
                if (i %% 2 == 0 && j %% 2 == 0 && k %% 2 == 0) {
                    if (i > 2) {
                        plot3d(a+frame*0.015, b+frame*0.0125, c-frame*0.05, type = "s", radius = .05, col = "blue", 
                            add = TRUE, 
                            xlab="", ylab="", zlab="", 
                            axes = FALSE)
                        plot3d(a+1, b+0.5, c, type = "s", radius = .05, col = "red", 
                            add = TRUE, 
                            xlab="", ylab="", zlab="", 
                            axes = FALSE)
                    }
                    if (i <= 2) {
                        plot3d(a, b, c, type = "s", radius = .05, col = "blue", 
                            add = TRUE, 
                            xlab="", ylab="", zlab="", 
                            axes = FALSE)
                        plot3d(a+1-frame*0.015, b+0.5-frame*0.0125, c-frame*0.05, type = "s", radius = .05, col = "red", 
                            add = TRUE, 
                            xlab="", ylab="", zlab="", 
                            axes = FALSE)
                    }
                }
                if (i %% 2 != 0 && j %% 2 != 0 && k %% 2 != 0) {
                    if (i > 2) {
                        plot3d(a+frame*0.015, b+frame*0.0125, c-frame*0.05, type = "s", radius = .05, col = "red", 
                            add = TRUE, 
                            xlab="", ylab="", zlab="", 
                            axes = FALSE)
                        plot3d(a+1, b+0.5, c, type = "s", radius = .05, col = "blue", 
                            add = TRUE, 
                            xlab="", ylab="", zlab="", 
                            axes = FALSE)
                    }
                    if (i <= 2) {
                        plot3d(a, b, c, type = "s", radius = .05, col = "red", 
                            add = TRUE, 
                            xlab="", ylab="", zlab="", 
                            axes = FALSE)
                        plot3d(a+1-frame*0.015, b+0.5-frame*0.0125, c-frame*0.05, type = "s", radius = .05, col = "blue", 
                            add = TRUE, 
                            xlab="", ylab="", zlab="", 
                            axes = FALSE)
                    }
                }
            }
        }
    }
    c2 <- cube3d()

    B <- matrix(c(0.1,0,0,0,0.2,0,0,0,0.2),3,3)
    c2 <- transform3d(c2, B)
    c2 <- translate3d(c2,0.35,0.25,0.25)
    c2_new <- translate3d(c2,frame*0.015,0+frame*0.0125,-frame*0.05)

    c2_trans <- translate3d(c2,.8-frame*0.015,0.5-frame*0.0125,-frame*0.05)
    for (i in 1:6) {
        lines3d(t(c2_trans$vb)[c2$ib[,i],])
        lines3d(t(c2_new$vb)[c2_new$ib[,i],])
    }

    newFrame <- totalFrames*2 - frame
    rgl.postscript(filename=paste("crossover/crossover-",sprintf("%03d",frame), ".pdf", sep=""), fmt="pdf")
    system(paste("convert -sharpen 0x1.0 -quality 100 crossover/crossover-",sprintf("%03d", frame), ".pdf crossover/crossover-",sprintf("%03d", frame), ".png", sep=""))
    if (frame != newFrame)
        file.copy(paste("crossover/crossover-",sprintf("%03d", frame),".png", sep=""), paste("crossover/crossover-", sprintf("%03d", newFrame), ".png", sep=""), overwrite = TRUE)
    clear3d()
}
system(paste("convert -delay 1 crossover/*.png crossover/crossover.gif", sep=""))
