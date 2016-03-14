
library(verisr)
library(ggplot2)
library(dplyr)
library(plyr)
#Phase 1
#Graph 1
#Packet number of reply in vs Destination Ip
phase1 <- read.csv("p1")


#Finding Destination IPs which gave attackers a reply
phase1s<-phase1[which(grepl("reply in", phase1$Info)!= "FALSE"),]

#Graph using Packet number to find the sequence in which each destination was found
f<- ggplot(phase1s, aes(No.,Destination))
f <- f + geom_point()
print(f)

#Phase 2
#Similar to the above
#we find data where sadmind makes a connection and finds a port
phase2 <- read.csv("p2")
phase2s<-phase2[which(grepl("Port:", phase2$Info)!= FALSE),]
f<- ggplot(phase2s, aes(Source,No.,label=Info))
f <- f + geom_text(size=3.5)
print(f)


#Phase3
phase3 <- read.csv("p3")
simple <- getenum(phase3,"Protocol")
simple <- rename(simple, c("enum"="Protocol","x"="Packets"))
simple2 <-getenum(phase3,"Destination")
simple2 <- rename(simple2, c("enum"="Destination IP","freq"="Destination Frequency"))

#Protocol frequency of SADMIND
#graphs adds color to boxplot to show the higher frequnecy/count of packets found within the sadmind attack
#the line itself represents the actual amount of packets
gg <- ggplot(simple, aes(x=Packets,y=Protocol, fill=freq))
gg <- gg + geom_boxplot(ymin=0,ymax=100)
gg <- gg + geom_tile(data=simple)
gg <- gg + scale_fill_gradient(low = "#F0F6FF", high = "#4682B4", guide=F)
print(gg)

#Phase4
#Finding TELNET conversations between source and destination
phase4 <-read.csv("p4")
telnet <-filter(phase4,Protocol=="TELNET")

hh <- ggplot(telnet,aes(x=Source,y=Destination))
hh <- hh + geom_violin(scale=3) + geom_jitter()

print(hh)

install.packages("MASS")

library(MASS)
#parcoord work for parallel coordinates plot
#need a dataframe or matrix with all numbers
#parcoord(x, col=1, lty=1)


#Phase5
phase5 <- read.csv("p5")
#graph with labels.
look_packets <- subset(phase5, phase5$Protocol == "TELNET")

ping_graph <- ggplot(look_packets, aes(x=Source,y=Destination)) + geom_bin2d(color = "#358CA1")
ping_graph <- ping_graph + ggtitle("Looking at Telnet")
ping_graph <- ping_graph + labs(x = "Source IPs", y = "Destination IPs")
ping_graph <- ping_graph + theme_solarized_2(light = FALSE) + scale_color_solarized("blue")
ping_graph <- ping_graph + theme(axis.text.x=element_text(angle=45, hjust=1))
print(ping_graph)

ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50)

