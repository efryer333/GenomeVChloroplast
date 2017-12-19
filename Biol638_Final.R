getwd()
setwd("/Users/emilyfryer/Documents/SFSU/BIOL638")

plantGenomes <- read.csv("PlantGenomes.csv")
chloroplastGenomes <- read.csv("ChloroplastGenomes.csv")

comboDF <- merge(plantGenomes, chloroplastGenomes, by = "Organism")
attach(comboDF)

library(ggplot2)

genomeVgenome <- comboDF[,c("G.Size..MB.","C.Size.Mb.")]
genomeVgenome <- genomeVgenome[c(-375,-374,-376,-299,-34,-154,-120,-121),]
head(genomeVgenome)
genomePlot <- ggplot(genomeVgenome, aes(x=G.Size..MB., y=C.Size.Mb.)) + geom_point(shape=21)
genomePlot


genomePlot + geom_point(position=position_jitter(width=.5, height=0))

genomeVgenomePG <- comboDF[,c("G.Size..pg.","C.Size.Mb.")]
ggplot(genomeVgenomePG, aes(x=G.Size..pg., y=C.Size.Mb.)) + geom_point(shape=21)

mean(G.Size..MB.)
sd(G.Size..MB.)
mean(C.Size.Mb.)
sd(C.Size.Mb.)
ggplot()