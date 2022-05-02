library(tidyverse)
setwd("~/Desktop/R/Module_Project/Images")
Full_Dataset <- read.csv("~/Desktop/R/Module_Project/Data/Full_Dataset.csv")
Full_Dataset_tb <- as_tibble(Full_Dataset)
head(Full_Dataset_tb)

View(Full_Dataset_tb)

#Deleting unwanted rows and columns
Full <- Full_Dataset_tb[-c(1:7243, 7284:7587, 7589:7706),]
Full <- subset (Full, select = -Code)

View(Full)

#Renaming columns 
library(dplyr)
colnames(Full) <- c("Country", "Year", "BCG", "HepB3", "Hib3", "IPV1", "MCV1",
                    "PCV3", "Pol3", "RCV1", "RotaC", "YFV", "DTP3", "MCV2")
View(Full)


#World Barchart
World <- Full[-c(1:40, 42),]
World <- subset (World, select = -Year)
World <- subset (World, select = -Country)

World <- subset (World, select = -c(13:253))

World <- t(World)
colnames(World)<-c("Percentage")

View(World)
World <- as.data.frame(World)

library(ggplot2)
p <- ggplot(World, aes(rownames(World),Percentage)) + 
            geom_bar(stat="identity", width=0.5,
                     fill=c('orangered2', 'royalblue3', 'seagreen3','maroon2',
                            'lightblue2', 'yellow1', 'mediumpurple2', 'khaki2',
                            'tan2', 'seashell3', 'pink3', 'lavender')) +
            geom_text(aes(label=Percentage), vjust=-0.7, size=3.5,) +
  ggtitle("The Percentage of one-year-olds who have been vaccinated worldwide in 2019",
          subtitle="Plot of Vaccination by Worldwide Percentage(%)") +
  labs(y="Percentage(%)", x="Vaccination") + theme_dark() + scale_fill_manual()+

theme(plot.title = element_text(face="bold.italic", size=12))+coord_flip()

p

ggsave("Figure1.png", plot=p)

#Line graph for UK data
View(Full)

#Deleting unwanted rows and columns
UK <- Full[-c(41),]
UK <- as.data.frame(UK)
UK[UK==0]<- NA

View(UK)

#UK Animated Line Chart
install.packages("gganimate")
install.packages("gifski")
library(ggplot2)
library(gganimate)

pl <- ggplot()+ geom_line(data=UK, mapping=aes(x=Year, y=HepB3 ,color="HepB3"))+
  geom_line(data=UK, mapping=aes(x=Year, y=Hib3, colour="Hib3"))+
  geom_line(data=UK, mapping=aes(x=Year, y=IPV1, colour="IPV1"))+
   geom_line(data=UK, mapping=aes(x=Year, y=PCV3, colour="PCV3"))+
  geom_line(data=UK, mapping=aes(x=Year, y=Pol3, colour="Pol3"))+
 geom_line(data=UK, mapping=aes(x=Year, y=RCV1, colour="RCV1"))+
  geom_line(data=UK, mapping=aes(x=Year, y=RotaC, colour="RotaC"))+
  geom_line(data=UK, mapping=aes(x=Year, y=DTP3, colour="DTP3"))+
 geom_line(data=UK, mapping=aes(x=Year, y=MCV2, colour="MCV2"))+
  geom_line(data=UK, mapping=aes(x=Year, y=MCV1, colour="MCV1"))+
 
labs(x="Year", y="Percentage(%)", title="One-year-olds Vaccinated in the UK from the Year 1980
     to 2019", subtitle=
       "Plot of Percentage by Year")+
  theme(plot.title = element_text(face="bold.italic"))+theme_dark()+
transition_reveal(Year)+ xlim(1980, 2025)+
  
  scale_color_manual(name = "Vaccination", values = c("HepB3" = "royalblue3",
                                                      "Hib3" = "seagreen3",
                                                      "IPV1" = "maroon2",
                                                      "PCV3" = "lightblue2",
                                                      "Pol3" = "yellow1",
                                                      "RCV1" = "mediumpurple2",
                                                      "RotaC" = "khaki2",
                                                      "DTP3" = "seashell3",
                                                      "MCV2" = "pink3",
                                                      "MCV1" = "lavender"))+
  theme(plot.title = element_text(face="bold.italic")) + ease_aes('cubic-in-out')

 pl 

 anim_save("Figure2.gif", pl)
 

