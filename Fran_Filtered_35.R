setwd('~/R_work/Fran')
df.use <- read.csv('filtered_35_NGOM.csv', header=TRUE)
df.meta <- df.use[1:2,1:36]
df.meta <- t(df.meta)
df.meta <- as.data.frame(df.meta[2:nrow(df.meta),])
df.meta[,3] <- rownames(df.meta)
colnames(df.meta) <- c('Season','Location','SampleID')
df.use <- df.use[4:31723,1:36]
df.use[,1] <- as.character(df.use[,1])
for(i in 2:ncol(df.use)){
  df.use[,i] <- as.numeric(as.character(df.use[,i])) 
}
for(i in 1:nrow(df.use)){
  rownames(df.use)[i] <- df.use[i,1]
  #print(i)
}
df.use <- as.data.frame(t(df.use[,2:ncol(df.use)]))

#df.use missing first three transcriptome ids. easier to manually enter than to redo entire operation.

# df.use[,1] <- as.character(df.use[,1])
# 
# colnames(df.use) <- c('Description','Mar2','Mar3','Mar5','Mar6','Mar7','Mar8','Mar9',
#                       'Sep2','Sep5','Sep6','Sep7','Sep8','Sep9','Sep10','Sep11','Sep14',
#                       'rRNA.5.8S','rRNA.5S','SK7.1','SK7.2',colnames(df.use[22:ncol(df.use)]))
# 
# df.meta[which(df.meta$Sex == 1),2] <- 'Male'
# df.meta[which(df.meta$Sex == 2),2] <- 'Female'
# df.meta[which(df.meta$Location == 1),3] <- 'Barataria Bay'
# df.meta[which(df.meta$Location == 2),3] <- 'Chandeleur Sound'
# df.meta[which(df.meta$Location == 3),3] <- 'Mississippi Sound'
# df.meta[which(df.meta$Season == 1),4] <- 'Spring'
# df.meta[which(df.meta$Season == 2),4] <- 'Summer'
# df.meta[which(df.meta$Season == 3),4] <- 'Winter'
# df.meta[21,2] <- 'Female'

#Old dataset
# df.use <- read.csv('dolphin_65_sorted.csv')
# df.use <- df.use[2:ncol(df.use)]

# df <- read.csv('dolphin_FPKM_65.csv')
# df <- df[1:13684,]
# 
# #Make sample names and df.use
# sample.names <- c()
# 
# for(i in 3:ncol(df)){
#   sample.names[i] <- toString(df[3,i])
# }
# 
# sample.names <- sample.names[3:length(sample.names)]
# 
# df.use <- data.frame(sample.names)
# 
# #Build df.use
# n <- 0
# 
# for(i in 3:ncol(df)){
#   x <- as.character(df[7:nrow(df),i])
#   x <- as.numeric(x)
#   n <- n + 1
#   print(n)
#   for(j in 2:(length(x) + 1)){
#     df.use[n,j] <- x[j-1]
#   }
# }
# 
# #13678 columns NOT 18787
# 
# #Make column names
# #Sex
# sex <- c()
# 
# for(i in 3:ncol(df)){
#   sex[i] <- toString(df[1,i])
# }
# 
# sex <- sex[3:length(sex)]
# 
# df.use[13679] <- sex
# 
# #Location
# location <- c()
# 
# for(i in 3:ncol(df)){
#   location[i] <- toString(df[2,i])
# }
# 
# location <- location[3:length(location)]
# 
# df.use[13680] <- location
# 
# #Season type
# season.type <- c()
# 
# for(i in 3:ncol(df)){
#   season.type[i] <- toString(df[4,i])
# }
# 
# season.type <- season.type[3:length(season.type)]
# 
# df.use[13681] <- season.type
# 
# #Season
# season <- c()
# 
# for(i in 3:ncol(df)){
#   season[i] <- toString(df[5,i])
# }
# 
# season <- season[3:length(season)]
# 
# df.use[13682] <- season
# 
# #Gene names
# gene.names <- c()
# 
# for(i in 7:nrow(df)){
#   gene.names[i] <- toString(df[i,2])
# }
# 
# gene.names <- gene.names[7:length(gene.names)] 
# 
# #Make df.use column names
# m <- c('SampleIDs', gene.names[-13678], 'Sex', 'Location', 'SeasonType', 'Season')
# 
# colnames(df.use) <- m
# 
# #x <- as.character(df[7:nrow(df),3])
# #x <- as.numeric(x)
# 
# #for(i in 2:length(x)){
# #  df.use[1,i] <- x[i-1]
# #}
# 
# #Change zeros to 0.001
# #Log transform for PCA
# df.use[df.use == 0] <- 0.001

# df.log <- log(df.use[2:13678])
# 
# metadata <- c(1, 13679:13682)
# 
# df.metadata <- df.use[metadata]
# df.metadata$SampleIDs <- as.character(df.metadata$SampleIDs)
df.meta$Season <- factor(df.meta$Season)
#df.meta$Sex <- factor(df.meta$Sex)
df.meta$Location <- factor(df.meta$Location)
# df.meta$SeasonType <- factor(df.meta$SeasonType)

#TYP052710-01 and TYP052810-02


#Run PCA
#df.use <- df.use[2:ncol(df.use)]
df.pca <- prcomp(df.use, retx=TRUE, center=TRUE, scale=TRUE)

df.pca$sdev
df.pca$rotation
df.pca$center
df.pca$scale
df.pca$x

#ggplot graphics
df.plot <- as.data.frame(df.pca$x)
df.plot$Sex <- df.meta$Sex
df.plot$Season <- df.meta$Season
df.plot$SampleIDs <- df.meta$SampleID
df.plot$Location <- df.meta$Location


#labeling
df.plot$SampleIDs <- as.character(df.plot$SampleIDs)
which(df.meta$SampleID == 'R3100513-02' | 
        df.meta$SampleID == 'R3100512-03' | 
        df.meta$SampleID == 'TYP100918-01' | df.meta$SampleID == 'R3110816-03')
df.plot$SampleIDs[-c(21,23,28,64)] = ''
#ggplot(df.plot, aes(x=PC1, y=PC2, color=Sex, shape=Season, label=SampleIDs)) + geom_point() + geom_text()
#ggplot(df.plot, aes(x=PC1, y=PC2, color=Season, label=SampleIDs)) + geom_point() + geom_text(size=5)

#Season
library(ggplot2)
p <- ggplot(df.plot, aes(x=PC1, y=PC2, color=Season, label=SampleIDs)) + geom_point() + geom_text(size=5)
p + scale_x_continuous(limits=c(-300, 200))

#Manuscript
#Season
p <- ggplot(df.plot, aes(x=PC1, y=PC2, color=Season)) + geom_point()
p <- p + scale_x_continuous(limits=c(-300, 200))
p <- p + theme(panel.background = element_rect(fill = 'white', color = 'black'))
p

#Sex
# p <- ggplot(df.plot, aes(x=PC1, y=PC2, color=Sex)) + geom_point()
# p <- p + scale_x_continuous(limits=c(-300, 200))
# p <- p + theme(panel.background = element_rect(fill = 'white', color = 'black'))
# p

#Location
p <- ggplot(df.plot, aes(x=PC1, y=PC2, color=Location)) + geom_point()
p <- p + scale_x_continuous(limits=c(-300, 200))
p <- p + theme(panel.background = element_rect(fill = 'white', color = 'black'))
p

plot(df.pca, type='line')

summary(df.pca)

# scores <- data.frame(df.metadata, df.pca$x[,1:3])
# pc1.2 <- qplot(x=PC1, y=PC2, data=scores, color=factor(df.metadata)) +
#   theme(legend.position="none")
# pc1.3 <- qplot(x=PC1, y=PC3, data=scores, colour=factor(df.metadata)) +
#   theme(legend.position="none")
# pc2.3 <- qplot(x=PC2, y=PC3, data=scores, colour=factor(df.metadata)) +
#   theme(legend.position="none")

biplot(df.pca)

#Visualize
#Season
COLOR <- c(2:4)
PCH <- c(1:3)
plot(df.pca$x[,1], df.pca$x[,2], col=COLOR[df.metadata$Season], pch=PCH[df.metadata$Season], xlab='PC1', ylab='PC2', main='Season PCA')
legend("topleft", legend=levels(df.metadata$Season), col=COLOR, pch=PCH)


#Sex
COLOR <- c(2,3)
PCH <- c(1,2)
plot(df.pca$x[,1], df.pca$x[,2], col=COLOR[df.metadata$Sex], pch=PCH[df.metadata$Sex], xlab='PC1', ylab='PC2', main='Sex PCA')
legend("topleft", legend=levels(df.metadata$Sex), col=COLOR, pch=PCH)

#Location
COLOR <- c(2:4)
PCH <- c(1:3)
plot(df.pca$x[,1], df.pca$x[,2], col=COLOR[df.metadata$Location], pch=PCH[df.metadata$Location], xlab='PC1', ylab='PC2', main='Location PCA')
legend("topleft", legend=levels(df.metadata$Location), col=COLOR, pch=PCH)

#Season Type
COLOR <- c(2:6)
PCH <- c(1:5)
plot(df.pca$x[,1], df.pca$x[,2], col=COLOR[df.metadata$SeasonType], pch=PCH[df.metadata$SeasonType], xlab='PC1', ylab='PC2', main='Season Type PCA')
legend("topleft", legend=levels(df.metadata$SeasonType), col=COLOR, pch=PCH)




#better graphics
library(reshape2)
library(ggplot2)

melted <- cbind(df.metadata$SampleIDs, melt(df.pca$rotation[,1:3]))

barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value, fill=variable.group), stat="identity") +
  facet_wrap(~Var2)

scores <- data.frame(sample.groups, pca$x[,1:3])
pc1.2 <- qplot(x=PC1, y=PC2, data=scores, colour=factor(sample.groups)) +
  theme(legend.position="none")
pc1.3 <- qplot(x=PC1, y=PC3, data=scores, colour=factor(sample.groups)) +
  theme(legend.position="none")
pc2.3 <- qplot(x=PC2, y=PC3, data=scores, colour=factor(sample.groups)) +
  theme(legend.position="none")
