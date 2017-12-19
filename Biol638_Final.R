getwd()
setwd("/Users/emilyfryer/Documents/SFSU/BIOL638/Final_Project")

#Loading library packages need for analyses
library(ggplot2)
library(QuantPsyc)

#Reading in data
plantGenomes <- read.csv("PlantGenomes.csv")
chloroplastGenomes <- read.csv("ChloroplastGenomes.csv")

#Merging the two .csv files into one
comboDF <- merge(plantGenomes, chloroplastGenomes, by = "Organism")

#Attaching column names
attach(comboDF)

#Creating a dataframe that contains only the data needed for the Genome V Chloroplast plot
genomeVgenome <- comboDF[,c("G.Size..MB.","C.Size.Mb.")]

#Creating a ratio of whole genome size / chloroplast genome size
comboDF$Ratio <- comboDF$G.Size..MB./comboDF$C.Size.Mb.
norm(comboDF$Ratio)
hist(comboDF$Ratio)
norm(log(comboDF$Ratio))
hist(log(comboDF$Ratio))

#Removed outliers skewing the graph. Will include commentary about this in discussion
adjusted_genomeVgenome <- genomeVgenome[c(-375,-374,-376,-299,-34,-154,-120,-121),]
head(adjusted_genomeVgenome)
max(adjusted_genomeVgenome$G.Size..MB.)

#First genome plot shows a huge cluster in one area of the graph due overlap of many data points
genomePlot <- ggplot(adjusted_genomeVgenome, aes(x=G.Size..MB., y=C.Size.Mb.)) + geom_point(shape=21)
genomePlot

#"Jittering the plot to remove some of the noise and enhance visualization
genomePlot + geom_point(position=position_jitter(width=.5, height=0))

#Generating some basic stats (mean, standard deviation, variance) for whole genome size both transformed and un-transformed
#Mean
mean(G.Size..MB.)
#Standard Deviation
sd(G.Size..MB.)
#variance
var(G.Size..MB.)
#Testing for normality of the data
norm(G.Size..MB.)
#Log transforming the data to achieve normality
transG_dat <- log(G.Size..MB.)
#Testing normality of transformed data
norm(transG_dat)
#Generating a histogram of the transformed data
hist(transG_dat)


#Generating some basic stats (mean, standard deviation, variance) for chloroplast genome size both transformed and un-transformed
#Mean
mean(C.Size.Mb.)
#Standard Deviation
sd(C.Size.Mb.)
#Variance
var(C.Size.Mb.)
#Testing normality of the chlorplast genome size data
norm(C.Size.Mb.)
#Transforming the Chloroplast genome size data to achieve normality
transC_dat <- log(C.Size.Mb.)
#Histogram of transformed data
hist(transC_dat)
#Testing normality of transformed chloroplast data
norm(transC_dat)


#Calculating the Pearson's correlation coefficient for the transformed data due to lack of normality in chloroplast data
cor.test(transG_dat,transC_dat,method = "pearson")

#Generating plots for transformed data
transDat <- data.frame(cbind(transG_dat, transC_dat))
class(transDat)
transPlot <- ggplot(transDat, aes(x=transG_dat, y=transC_dat)) + geom_point(shape=21)
transPlot
transPlot + geom_point(position=position_jitter(width=.5, height=0))

#Fitting a linear model to test whether Chloroplast size is dependent on genome size.
genomeLM = lm(transDat$transC_dat~transDat$transG_dat)
summary(genomeLM)
#Generating plots to determine whether the application of the linear model is valid
par(mfrow=c(2,2))
plot(genomeLM)
