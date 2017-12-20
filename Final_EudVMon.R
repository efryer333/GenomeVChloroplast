library(ggplot2)
library(QuantPsyc)

#Reading in data
plantGenomes <- read.csv("PlantGenomes.csv")
chloroplastGenomes <- read.csv("ChloroplastGenomes.csv")

#Merging the two .csv files into one
comboDF <- merge(plantGenomes, chloroplastGenomes, by = "Organism")

#Attaching column names
attach(comboDF)

#Subsetting the Eudicots from the data
Eudicots <- subset(comboDF, Angiosperm.Group == "Eudicot", select= c(G.Size..MB.,C.Size.Mb.))
head(Eudicots)
#Generating distinct column names for new subsets
names(Eudicots) <- c("Eud_WhlG", "Eud_ChlrG")

#Subsetting the Monocots from the data
Monocots <- subset(comboDF, Angiosperm.Group == "Monocot", select= c(G.Size..MB.,C.Size.Mb.))
head(Monocots)
#Generating distinct column names for new subsets
names(Monocots) <- c("Mon_WhlG", "Mon_ChlrG")

#Some stats about Eudicot data
mean(Eudicots$Eud_WhlG)
sd(Eudicots$Eud_WhlG)
var(Eudicots$Eud_WhlG)
norm(Eudicots$Eud_WhlG)
transEudWhl <- log(Eudicots$Eud_WhlG)
norm(transEudWhl)
hist(transEudWhl)

mean(Eudicots$Eud_ChlrG)
sd(Eudicots$Eud_ChlrG)
var(Eudicots$Eud_ChlrG)
norm(Eudicots$Eud_ChlrG)
transEudChl <- log(Eudicots$Eud_ChlrG)
norm(transEudChl)
hist(transEudChl)

#Some stats about Monocot data
mean(Monocots$Mon_WhlG)
sd(Monocots$Mon_WhlG)
var(Monocots$Mon_WhlG)
norm(Monocots$Mon_WhlG)
transMonWhl <- log(Monocots$Mon_WhlG)
norm(transMonWhl)
hist(transMonWhl)

mean(Monocots$Mon_ChlrG)
sd(Monocots$Mon_ChlrG)
var(Monocots$Mon_ChlrG)
norm(Monocots$Mon_ChlrG)
transMonChl <- log(Monocots$Mon_ChlrG)
norm(transMonChl)
hist(transMonChl)

