setwd("C:/Users/tfranzem/Desktop/KentStateResearch/CerambycidaeTrapping")
library(readr)
malaise <- read_csv("MalaiseTrapData.csv")
malaise <- as.data.frame(malaise)
str(malaise)#check the dimensions and structure of the 'malaise' object
malaise <- malaise[1:74,]###Subset the dataframe to get rid of a bunch of empty rows
##Now we want to replace 'spider' and 'harvestman' with their taxonomic order names to make 
##those entries consistent with everything else in that column
malaise$Order <- gsub("Spider","Araneae",malaise$Order,ignore.case=TRUE)
malaise$Order <- gsub("Harvestman","Opiliones",malaise$Order,ignore.case=TRUE)
#How many orders are detected in our dataset?
Order <- unique(malaise$Order)

date <- unique(malaise$Date)
site <- unique(malaise$Site)


hist(malaise$Count)
Div_Format <- array(0, dim=c(length(Order),length(site)))
rownames(Div_Format) <- as.character(Order)
colnames(Div_Format) <- as.character(site)

for(i in 1:length(malaise$Order)){
  for(j in 1:length(unique(malaise$Order))){
    for(k in 1:length(unique(malaise$Site))){
      if(malaise$Order[i]==dimnames(Div_Format)[[1]][j] && malaise$Site[i]==dimnames(Div_Format)[[2]][k]){
        Div_Format[j,k] <- Div_Format[j,k] + malaise$Count[i]
      }
    }
  }
}
library(vegan)
Div_Format <- t(Div_Format)
boxplot(Div_Format[,], xlab="Order",ylab="Abundance")

par(mfrow=c(2,2))
barplot(Div_Format[1,], main="Snowville 1",xaxt="n", xlab="Order",ylab="Abundance",ylim=c(0,1200))
# Add custom x-axis
axis(1, at = seq_along(Order), labels = FALSE)
# Rotate and add text labels
text(x = seq_along(Order), y = par("usr")[3] - 50, labels = Order, 
     srt = 45, adj = 1, xpd = TRUE)
barplot(Div_Format[2,], main="Snowville 2",xaxt="n",xlab="Order",ylab="Abundance",ylim=c(0,1200))
# Add custom x-axis
axis(1, at = seq_along(Order), labels = FALSE)
# Rotate and add text labels
text(x = seq_along(Order), y = par("usr")[3] - 50, labels = Order, 
     srt = 45, adj = 1, xpd = TRUE)
barplot(Div_Format[3,], main="Campus 1",xaxt="n",xlab="Order",ylab="Abundance",ylim=c(0,1200))
# Add custom x-axis
axis(1, at = seq_along(Order), labels = FALSE)
# Rotate and add text labels
text(x = seq_along(Order), y = par("usr")[3] - 50, labels = Order, 
     srt = 45, adj = 1, xpd = TRUE)
barplot(Div_Format[4,], main="Campus 2",xaxt="n",xlab="Order",ylab="Abundance",ylim=c(0,1200))
# Add custom x-axis
axis(1, at = seq_along(Order), labels = FALSE)
# Rotate and add text labels
text(x = seq_along(Order), y = par("usr")[3] - 50, labels = Order, 
     srt = 45, adj = 1, xpd = TRUE)


Order_Richness <- specnumber(Div_Format)
plot(Order_Richness)

ord <- metaMDS(Div_Format)
stressplot(ord)
plot(ord)
library(RColorBrewer)
colv=brewer.pal(5,name="Accent")
treat <- unique(malaise$Date)

plot(ord,display="species",type="n",xlim=c(-0.7,0.6),ylim=c(-0.3,0.5))
##ordihull(ord,groups=treat,draw="polygon",col="grey90",label=F)
points(ord, display = "site", cex = 0.8, pch=21, col="red", bg="yellow")
text(ord, display = "site", cex = 0.6,)
text(ord, display = "species", cex=0.3, col="grey")
ordiellipse(ord,groups=treat,display="species",draw="polygon",label=F)
#ordiellipse(ord, treat, col=1:7, draw="polygon")
#ord.fit <- envfit(ord ~ DDQ1+ DDQ2+DDQ3+DDQ4,data=env.var,  perm=999)

legend(x="bottomright", legend=treat_levels, pch=15,col=colv)

