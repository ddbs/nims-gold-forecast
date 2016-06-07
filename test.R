

#Plot the second time series. The command par(new=T) is handy here. 
# If you just need to plot two timeseries, you could also use the right vertical 
# axis as well. In that case you have to substitute “2” with “4” in the 
# functions axis() and mtext(). Notice that in both functions lines is 
# increased so that the new axis and its label is placed to the left of the 
# first one. You don’t need to increase the value if you use the right vertical axis.
par(new=T)
plot(time, med, axes=F, ylim=c(0,max(med)), xlab=””, ylab=””, 
     type=”l”,lty=2, main=””,xlim=c(7000,3400),lwd=2)
axis(2, ylim=c(0,max(med)),lwd=2,line=3.5)
points(time, med,pch=20)
mtext(2,text=”Median Group Size”,line=5.5)


#Plot the third time series. Again the line parameter are both further increased.


par(new=T)
plot(time, grp, axes=F, ylim=c(0,max(grp)), xlab=””, ylab=””, 
     type=”l”,lty=3, main=””,xlim=c(7000,3400),lwd=2)
axis(2, ylim=c(0,max(grp)),lwd=2,line=7)

points(time, grp,pch=20)
mtext(2,text=”Number of Groups”,line=9)


#We can now draw the X-axis, which is of course shared by all the three time-series.

axis(1,pretty(range(time),10))
mtext(“cal BP”,side=1,col=”black”,line=2)


#And then plot the legend.
legend(x=7000,y=12,legend=c(“Population”,”Median Group Size”,”Number of Groups”),lty=c(1,2,3))