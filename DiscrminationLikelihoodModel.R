library(pracma)
library(ggplot2)
library(ggthemes)

orientations <- seq(-pi/2, pi/2, 0.05)
preferredOrientation <- -90  ## This is equivalent to mean of standard distribution
k <- pi/1  ## 1/k is equivalent to variance of standard distribution

res <- vonMises(orientations, preferredOrientation, k)
#qplot(rad2deg(orientations), res, geom="line")

ggplot(data.frame(x=orientations), aes(x=x)) + 
	theme_few() + 
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_blank()) + 
	theme(axis.line = element_line(colour = "#a9a9a9", size = 0.3)) + 
	theme(axis.ticks.x = element_line(colour = "#a9a9a9", size = 0.3), axis.ticks.y = element_line(colour = "#a9a9a9", size = 0.3)) + 	
	#scale_y_continuous(breaks=c(0, 0.5, 1), labels=c(0, 0.5, 1), expand=c(0,0.4)) + 
	scale_x_continuous(breaks=c(-pi/2, -pi/4, 0, pi/4, pi/2), labels=rad2deg(c(-pi/2, -pi/4, 0, pi/4, pi/2))) + 
	stat_function(fun=vonMises, args=list(preferredOrientation=deg2rad(10), k=k), size=2, color="#FF7F00") + 
	stat_function(fun=vonMises, args=list(preferredOrientation=-pi/4, k=k), color="#E41A1C") +
	stat_function(fun=vonMises, args=list(preferredOrientation=pi/4, k=k), color="#377EB8") + 
	stat_function(fun=vonMises, args=list(preferredOrientation=-pi/2, k=k), color="#4DAF4A") +
	stat_function(fun=vonMises, args=list(preferredOrientation=pi/2, k=k), color="#984EA3") +
	stat_function(fun=vonMises, args=list(preferredOrientation=-3*pi/4, k=k), color="grey50") + ## -135 deg
	stat_function(fun=vonMises, args=list(preferredOrientation=3*pi/4, k=k), color="grey50") 	## 135 deg
	#stat_function(fun=vonMises, args=list(preferredOrientation=-pi, k=k), color="grey50") +
	#stat_function(fun=vonMises, args=list(preferredOrientation=pi, k=k), color="grey50")

###########################################################################
## Tuning function of each neuron. The tuning function is defined using 
## von Mises distribution (called circular normal distribution). 
vonMises <- function(orientations, preferredOrientation, k){
	#circularNormal <- exp(k * (2*cos(orientations-preferredOrientation) - 1))  ## Original formula from Pestilli et al, 2009 paper. 
	circularNormal <- log(exp(k * (2*cos(orientations-preferredOrientation) - 1)))
	
	#circularNormal <- scale(circularNormal)*0.5+0.5
	#circularNormal <- exp(k * (2*cos((orientations-preferredOrientation)) - 1))  
	
	## NOTE: I've subtracted 2 to normalize the tuning curve from 0 to 1. 
	## in Pestilli paper the formula uses -1. Need to figure out if it was done 
	## on purpose in their paper. 
}

