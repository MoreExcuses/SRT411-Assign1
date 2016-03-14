honeypot <- read.csv("kyoto-honepot.csv")
summary(honeypot)


honey2 <- honeypot[which(honeypot$IDS_detect!=""), ]
library(verisr)
h2 <- getenum(honey2, "Label")

head(honey2)

honey3 <- tbl_df(honey2)

library(dplyr)
honey4 <- select(honey3, IDS_detect:dest_port)


h4 <- filter(honey4, Label == "-1" )
honey5<- h4[which(h4$IDS_detect!="0" | h4$Malware_detect!="0" | h4$Ashula_detect!="0" ), ]
h5 <- distinct(honey5,src_ip,dest_ip)
ashula <- getenum(h5, "Ashula_detect")
malware <-getenum(h5, "Malware_detect")
ids <- getenum(h5, "IDS_detect")

ashula <- arrange (ashula,desc(freq))
malware <- arrange (malware,desc(freq))
ids <-arrange(ids,desc(freq))

library(ggplot2)
sampleplot <- function(field) {
  localdf <- field[c(1:7), ]
  localdf$lab <- paste(round(localdf$freq*100, 0), "%", sep="")
  gg <- ggplot(localdf, aes(x=enum, y=freq, label=lab))
  gg <- gg + geom_bar(stat="identity", fill="blue")
  # add in text, adjusted to the end of the bar
  gg <- gg + geom_text(hjust=-0.1, size=3)
  # flip the axes and add in a title
  gg <- gg + coord_flip() + ggtitle(field)
  # remove axes labels and add bw theme
  gg <- gg + xlab("") + ylab("") + theme_bw()
  # fix the y scale to remove padding and fit our label (add 7%)
  gg <- gg + scale_y_continuous(expand=c(0,0),
                                limits=c(0, max(field$freq)*1.1))
  # make it slightly prettier than the default
  gg <- gg + theme(panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks = element_blank())
}

print(sampleplot(top_n(ids,5,freq)))
print(sampleplot(top_n(malware,5,freq)))
print(sampleplot(top_n(ashula,5,freq)))