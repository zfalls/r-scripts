#!/usr/bin/env Rscript

#LOAD LIbrary
library(rgl)

dir.create("strain")
par3d(windowRect = c(0, 50, 800, 800),
    userMatrix = rotationMatrix(-30*pi/180, 0, 1, 0.5) %*% par3d("userMatrix"))
totalFrames <- 20
for (frame in 1:totalFrames) {
    plot3d(0.05, 0.05, 0.05,
        col = "white", 
        xlab="", ylab="", zlab="", 
        xlim=c(-0.2,0.6),
        ylim=c(-0.2,0.6),
        zlim=c(-0.2,0.6),
        axes = FALSE)
    
    c3d <- cube3d()
    
    A <- matrix(c(0.15,0,0,0,0.15,0,0,0,0.15),3,3)
    c3d <- transform3d(c3d, A)
    
    a <- 1 + frame*0.01
    b <- frame*0.01
    c <- frame*0.01
    
    d <- frame*0.01
    e <- 1 + frame*0.01
    f <- frame*0.01
    
    g <- frame*0.01
    h <- frame*0.01
    i <- 1 + frame*0.01
    
    A <- matrix(c(a,b,c,d,e,f,g,h,i),3,3)
    c3d_trans <- transform3d(c3d, A)
    for (i in 1:6)
        lines3d(t(c3d_trans$vb)[c3d$ib[,i],])
    newFrame <- totalFrames*2 - frame
    rgl.postscript(filename=paste("strain/strain-",sprintf("%03d",frame), ".pdf", sep=""), fmt="pdf")
    system(paste("convert -crop 400x400+75+285 +repage -sharpen 0x1.0 -quality 100 strain/strain-",sprintf("%03d", frame), ".pdf strain/strain-",sprintf("%03d", frame), ".pdf", sep=""))
    system(paste("convert -flatten -background white -sharpen 0x1.0 -quality 100 strain/strain-",sprintf("%03d", frame), ".pdf strain/strain-",sprintf("%03d", frame), ".png", sep=""))
    if (frame != newFrame)
        file.copy(paste("strain/strain-",sprintf("%03d", frame),".png", sep=""), paste("strain/strain-", sprintf("%03d", newFrame), ".png", sep=""), overwrite = TRUE)
    clear3d()
}
system(paste("convert -delay 1 strain/*.png strain/strain.gif", sep=""))
