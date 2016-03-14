install.packages("devtools")
library(devtools)
install_github("verisr", "jayjacobs")

library(verisr)
jsondir <- 'data/json/'

jsondir
vcdb <- json2veris(jsondir)

summary(vcdb)

actors <- getenum(vcdb, "actor")
print(actors)

actors <- getenum(vcdb, "actor", add.n=TRUE, add.freq=TRUE)
print(actors)
# frequncy

#7-10 plots
library(ggplot2)
verisplot <- function(vcdb, field) {
  localdf <- getenum(vcdb, field, add.freq=T)
  localdf <- localdf[c(1:15), ]
  localdf$lab <- paste(round(localdf$freq*100, 0), "%", sep="")
  gg <- ggplot(localdf, aes(x=enum, y=freq, label=lab))
  gg <- gg + geom_bar(stat="identity", fill="red")
  # add in text, adjusted to the end of the bar
  gg <- gg + geom_text(hjust=-0.1, size=3)
  # flip the axes and add in a title
  gg <- gg + coord_flip() + ggtitle(field)
  # remove axes labels and add bw theme
  gg <- gg + xlab("") + ylab("") + theme_bw()
  # fix the y scale to remove padding and fit our label (add 7%)
  gg <- gg + scale_y_continuous(expand=c(0,0),
                                limits=c(0, max(localdf$freq)*1.1))
  # make it slightly prettier than the default
  gg <- gg + theme(panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks = element_blank())
}
print(verisplot(vcdb, "action"))
print(verisplot(vcdb, "actor.external.variety"))
print(verisplot(vcdb, "action.physical.variety"))
print(verisplot(vcdb, "action.hacking.vector"))
print(verisplot(vcdb, "attribute.confidentiality.data.variety"))
print(verisplot(vcdb, "asset.assets"))

#7-11 plots
a2 <- getenum(vcdb, enum="action")
# requires package : verisr, ggplot2
# requires object: vcdb (7-6)
# get a data.frame comparing the actions to the assets
# this will add zero's in missing squares and include a frequency
a2 <- getenum(vcdb, enum="action", primary="asset.assets", add.freq=T)
# trim unknown asset and environment action for space
a2 <- a2[which(a2$enum!="Environmental" & a2$enum!="Unknown"), ]
a2
# so we should create a "slim" version without zeros to color it
slim.a2 <- a2[which(a2$x!=0), ]
# could sort these by converting to factors (we did in Fig 7-6)

slim.a2

# now make a nice plot
gg <- ggplot(a2, aes(x=enum, y=enum1, fill=freq))
gg <- gg + geom_tile(fill="white", color="gray80")
gg <- gg + geom_tile(data=slim.a2, color="gray80")
gg <- gg + scale_fill_gradient(low = "#F0F6FF", high = "#4682B4", guide=F)
gg <- gg + xlab("") + ylab("") + theme_bw()
gg <- gg + scale_x_discrete(expand=c(0,0))
gg <- gg + scale_y_discrete(expand=c(0,0))
gg <- gg + theme(axis.ticks = element_blank())
# and view it
print(gg)