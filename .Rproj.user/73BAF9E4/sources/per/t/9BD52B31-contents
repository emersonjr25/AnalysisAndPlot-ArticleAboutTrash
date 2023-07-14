library("lmtest") 
library("gam")
library("mgcv")
library("ggplot2")

#Loading data to graphics
data <- read.table("data/dataRtwo.txt", h=TRUE)
data
#View(data)

#Graphic for viés abundance
AbundanceTotal <- data$TotalAbundance
Length <- data$Length
plot1 <- plot (Length, AbundanceTotal)
model1 <- lm(AbundanceTotal ~ Length) #linear model to verify if length of space affect abundance of material collected.
abline(model1)
summary(model1)

#Graphic for viés Richness
Richness <- data$Richness
model2 <- lm(Richness ~ Length)
summary(model2)
plot(Length, Richness)
abline(model2)

#Test for normality and homogeneity
shapiro.test(data$Points) #for normality

#GAM1 - Generalized additive model - AbundanceWoodxPoints
DistanceInMeters <- data$Distance
AbundanceWood <- data$AbundanceWood
GAM1 <- gam(AbundanceWood ~ s(DistanceInMeters), method="REML")
summary(GAM1)
ggplot1 <- ggplot(data, aes(DistanceInMeters, AbundanceWood)) + geom_point() + geom_text(aes(label = data$Points), size = 3, hjust = 1, vjust = 1.1) + theme_classic(base_size = 16) + geom_smooth(method="gam", formula= y ~ s(x))
ggplot1 + geom_text (aes (label=ifelse(DistanceInMeters == 180, as.character("Barra"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 2700, as.character("Siribinha"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 7560, as.character("Poças"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust= -1) + annotate("text", label = "c", x = 7500, y = 20, size = 8, colour = "black") +  scale_x_continuous(name ="Distance in meters") + scale_y_continuous(name = "Wood abundance ")

#GAM2 - RichnessxPoints
GAM2<- gam(Richness ~ s(DistanceInMeters), method = "REML")
summary(GAM2)
ggplot2 <- ggplot(data, aes(DistanceInMeters, Richness)) + geom_point() + geom_text(aes(label = data$Points), size = 3, hjust = 1, vjust = 1.1) + theme_classic(base_size = 16) + geom_smooth(method="gam", formula = y ~ s(x))
ggplot2 + geom_text (aes (label=ifelse(DistanceInMeters == 180, as.character("Barra"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 2700, as.character("Siribinha"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 7560, as.character("Poças"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust= 1.5) + annotate("text", label = "b", x = 7500, y = 20, size = 8, colour = "black") + scale_x_continuous(name ="Distance in meters") + scale_y_continuous(name = "Richness")

#GAM3 - TotalAbundancexPoints
GAM3 <- gam(AbundanceTotal ~ s(DistanceInMeters), method="REML")
summary(GAM3)
ggplot3 <- ggplot(data, aes(DistanceInMeters, AbundanceTotal)) + geom_point() + geom_text(aes(label = data$Points), size = 3, hjust = 1, vjust = 1.1) + theme_classic(base_size = 16) + geom_smooth(method="gam", formula = y ~ s(x))
ggplot3 + geom_text (aes (label=ifelse(DistanceInMeters == 180, as.character("Barra"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 2700, as.character("Siribinha"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 7560, as.character("Poças"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust= 2) + annotate("text", label = "a", x = 7500, y = 80, size = 8, colour = "black") + scale_x_continuous(name ="Distance in meters") + scale_y_continuous(name = "Total abundance ")

#GAM4 - AbundanceMetalxPoints
AbundanceMetal <- data$AbundanceMetal
GAM4 <- gam(AbundanceMetal ~ s(DistanceInMeters), method="REML")
summary(GAM4)
ggplot4 <- ggplot(data, aes(DistanceInMeters, AbundanceMetal)) + geom_point () + geom_text(aes(label = data$Points), size = 3, hjust = 1, vjust = 1.1) + theme_classic(base_size = 16) + geom_smooth (method="gam", formula = y ~ s(x))
ggplot4 + geom_text (aes (label=ifelse(DistanceInMeters == 180, as.character("Barra"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 2700, as.character("Siribinha"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 7560, as.character("Poças"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust= 2) + annotate("text", label = "d", x = 7500, y = 8, size = 8, colour = "black") + scale_x_continuous(name ="Distance in meters") + scale_y_continuous(name = "Metal abundance ")

#GAM5 - Plastic x Points
AbundancePlastic <- data$AbudancePlastic
GAM5 <- gam(AbundancePlastic ~ s(DistanceInMeters), method = "REML")
summary(GAM5)
ggplot5 <- ggplot(data, aes(DistanceInMeters, AbundancePlastic)) + geom_point () + geom_text(aes(label = data$Points), size = 3, hjust = 1, vjust = 1.1) + theme_classic(base_size = 16) + geom_smooth (method="gam", formula = y ~ s(x))
ggplot5 + geom_text (aes (label=ifelse(DistanceInMeters == 180, as.character("Barra"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 2700, as.character("Siribinha"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust=-0.5) + geom_text (aes (label=ifelse(DistanceInMeters == 7560, as.character("Poças"), '')), colour = "BLUE", fontface = "bold", size = 4, hjust = 0.5, vjust= 2) + annotate("text", label = "e", x = 7500, y = 70, size = 8, colour = "black") + scale_x_continuous(name ="Distance in meters") + scale_y_continuous(name = "Plastic abundance ")

