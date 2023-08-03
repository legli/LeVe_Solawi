
################### libraries ################### 
#load packages
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(data.table) 


################### data ################### 
## General
CropLabs <- c("Broccoli", "Bush beans", "Fennel", "Potatoes", "Turnips", "Lettuces", "Pumpkins", "Leeks", "Spinach beet", "Carrots", "Beetroot", "Tomatoes", "White cabbages", "Savoy cabbages", "Courgettes", "Onions")
CropTable <- data.frame(Kultur=c("Brokkoli","Buschbohne","Fenchel","Kartoffel", "Kohlrabi","Kopfsalat","Kürbis","Lauch","Mangold","Möhre","Rote Bete","Tomate","Weißkohl","Wirsing","Zucchini","Zwiebel"),
                        Crop=CropLabs)

manual.labs.english <- c(production = "Production", 
                 intermediate = "Distribution", 
                 household = "Consumption",
                 Conventional="Reference") 
                 # CSA="CSA",
                 # Ertrag_kg_qm = "Yield harvested",
                 # deliveredYield ="Collected yield",
                 # netYield = "Net yield",
                 # potYield = "Potential yield")

## CSA
# production
dfProduction_CSA <-  as.data.frame(read_excel("Data/csa_production_flw.xlsx"))
head(dfProduction_CSA)

# distribution
dfDistribution_CSA <- as.data.frame(read_excel("Data/csa_distribution_flw.xlsx"))
head(dfDistribution_CSA)

# consumption
dfConsumption_CSA <- as.data.frame(read_excel("Data/csa_consumption_flw.xlsx"))
head(dfConsumption_CSA)

# merge
dfCSA <- merge(dfProduction_CSA,dfDistribution_CSA,by=c("CSA","Kultur"),all=T)
dfCSA <- merge(dfCSA,dfConsumption_CSA,by="Kultur",all=T)
dfCSA <- merge(dfCSA,CropTable,by="Kultur")
dfCSA[is.na(dfCSA)] <- 0

## Conventional
dfReference <- read_excel("Data/conventional_flw.xlsx")
head(dfReference)
dfReference$Aspect <- "reference"
dfReference$CSA <- dfReference$Reference

################### analyses ################### 

###### Ratio losses all
## CSA
#### losses
## CSA
dfCSAGather <- dfCSA[,c("CSA","Crop","Ratio_production_total","Ratio_distribution_total","Ratio_household_total")]
names(dfCSAGather) <- c("CSA","Crop","production","intermediate","household")
dfCSAGather <- dfCSAGather  %>% gather(Stage, Ratio, "production":"household")
head(dfCSAGather)
dfCSAGather$Aspect <- "CSA"

## reference
# mean where same study assessed same crop multiple times
dfReferenceMean <- aggregate(Ratio~Avoidability+CSA+Stage+Aspect+Crop,dfReference,mean)
# sum of avoidability categories
dfReferenceSum <- aggregate(Ratio~CSA+Stage+Aspect+Crop,dfReferenceMean,sum)
head(dfReferenceSum)


dfCombined <- rbind(dfReferenceSum,dfCSAGather[,names(dfReferenceSum)])
dfCombined$Stage <- factor(dfCombined$Stage,levels=c("production","intermediate","household"))
# mean across CSAs/references
dfCombinedMean <- aggregate(Ratio~Aspect+Crop+Stage,dfCombined,mean)
dfCombinedSD <- aggregate(Ratio~Aspect+Crop+Stage,dfCombined,sd)
names(dfCombinedSD)[ncol(dfCombinedSD)] <- "RatioSD"
dfCombinedFinal <- merge(dfCombinedMean,dfCombinedSD,by=c("Aspect","Crop","Stage"))

# mean across crops
dfCombinedAverageMean <- aggregate(Ratio~Aspect+Stage,dfCombinedMean,mean)
dfCombinedAverageSD <- aggregate(Ratio~Aspect+Stage,dfCombinedMean,sd)
names(dfCombinedAverageSD)[ncol(dfCombinedAverageSD)] <- "RatioSD"
dfCombinedAverage <- merge(dfCombinedAverageMean,dfCombinedAverageSD,by=c("Aspect","Stage"))

1-dfCombinedAverage[which(dfCombinedAverage$Aspect=="CSA"&dfCombinedAverage$Stage=="production"),"Ratio"]/dfCombinedAverage[which(dfCombinedAverage$Aspect=="reference"&dfCombinedAverage$Stage=="production"),"Ratio"]
1-dfCombinedAverage[which(dfCombinedAverage$Aspect=="CSA"&dfCombinedAverage$Stage=="intermediate"),"Ratio"]/dfCombinedAverage[which(dfCombinedAverage$Aspect=="reference"&dfCombinedAverage$Stage=="intermediate"),"Ratio"]
1-dfCombinedAverage[which(dfCombinedAverage$Aspect=="CSA"&dfCombinedAverage$Stage=="household"),"Ratio"]/dfCombinedAverage[which(dfCombinedAverage$Aspect=="reference"&dfCombinedAverage$Stage=="household"),"Ratio"]


fig1 <-   ggplot(dfCombinedAverage, aes(x = Aspect, y = Ratio)) + 
  geom_bar(stat = 'identity', position = 'stack', colour = "black") + 
  facet_grid(.~Stage,labeller = as_labeller(manual.labs.english)) + ylab("Ratio")+xlab("") +
  ylim(0,0.5)+
  theme(axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.text.x = element_text(angle = 45, hjust = 1,size=8),
        axis.text.y = element_text(size=8))+
  geom_errorbar(aes(ymin= Ratio, 
                    ymax= Ratio+RatioSD),
                width=.2,
                position="identity")

# jpeg("fig1.jpeg", width = 16.9,height = 10, units = 'cm', res = 600)
  fig1
# dev.off()

## tables
dfCombinedFinal$Ratio <- paste0(round(dfCombinedFinal$Ratio,2)," (",round(dfCombinedFinal$RatioSD,2),")")
tableS5 <- dcast(setDT(dfCombinedFinal), Crop ~ Aspect+Stage, value.var = c("Ratio"))
# write.xlsx(tableS5,"tableS5.xlsx")


###### Net yield efficiency and potential to compensate yield gaps

## CSA
dfNYEAll <- dfCSA[, c("CSA", "Kultur", "Yield_kg_m2", "Ratio_distribution_total","Ratio_household_total")]
dfNYEAll <- merge(dfNYEAll,CropTable,by="Kultur")
dfReference <- dfCombinedMean[which(dfCombinedMean$Aspect=="reference"),]
dfReference <- dfReference[,c("Crop","Stage","Ratio")]%>% spread(Stage,Ratio)
dfNYEAll <- merge(dfNYEAll,dfReference,by="Crop")
dfNYEAll$netYield <- (dfNYEAll$Yield_kg_m2*(1-dfNYEAll$Ratio_distribution_total))*(1-dfNYEAll$Ratio_household_total) 
dfNYEAll$netYieldReference <- (dfNYEAll$Yield_kg_m2*(1-dfNYEAll$intermediate))*(1-dfNYEAll$household) 

dfNYEAll$gap <- ((dfNYEAll$netYield /dfNYEAll$netYieldReference)*100)
dfNYEAll[which(dfNYEAll$Crop=="Tomatoes"&dfNYEAll$CSA=="csa3"),"gap"] <- 100 # no compensatory potential (100%)
dfGapMean <- aggregate(gap~Crop,dfNYEAll,function(i){mean(i,na.rm=T)})
dfGapMean$gap <- dfGapMean$gap-100
dfGapSD <- aggregate(gap~Crop,dfNYEAll,function(i){sd(i,na.rm=T)})
names(dfGapSD)[2] <- "gapSD"
dfGap <- merge(dfGapMean,dfGapSD)
summary(dfGap$gap)

fig2 <- ggplot(dfGap, aes(x = Crop, y = gap)) + 
  geom_bar(stat = 'identity', position= "dodge", colour = "black") +
  ylab("Yield gap compensation potential (%)") + xlab("") +
  # ylim(0,17500)+
  theme(axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.text.x = element_text(angle = 45, hjust = 1,size=8),
        axis.text.y = element_text(size=8),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8))  +
  geom_errorbar(aes(ymin= gap,
                    ymax= gap+gapSD),
                width=.2,
                position="identity")

# jpeg("fig2.jpeg", width = 16.9,height = 10, units = 'cm', res = 600)
  fig2
# dev.off()

dfYieldSummary <- dfNYEAll[,c("CSA","Crop","Yield_kg_m2","netYield","netYieldReference","gap")]

dfYieldSummaryMean <- aggregate(cbind(Yield_kg_m2,netYield,netYieldReference,gap)~Crop,dfYieldSummary,function(i){mean(i,na.rm=T)})
dfYieldSummaryMean$gap <- dfYieldSummaryMean$gap-100
1-mean(dfYieldSummaryMean$netYield/dfYieldSummaryMean$Yield_kg_m2,na.rm=T)
1-mean(dfYieldSummaryMean$netYieldReference/dfYieldSummaryMean$Yield_kg_m2,na.rm=T)
mean(dfYieldSummaryMean$gap)
dfYieldSummarySD <- aggregate(cbind(Yield_kg_m2,netYield,netYieldReference,gap)~Crop,dfYieldSummary,function(i){sd(i,na.rm=T)})
names(dfYieldSummarySD)[2:5] <- c("YieldSD","netYieldSD","netYieldReferenceSD","gapSD")
tableS6 <- merge(dfYieldSummaryMean,dfYieldSummarySD)
tableS6$Yield <- paste0(round(tableS6$Yield_kg_m2,2)," (",round(tableS6$YieldSD,2),")")
tableS6$netYield <- paste0(round(tableS6$netYield,2)," (",round(tableS6$netYieldSD,2),")")
tableS6$netYieldReference <- paste0(round(tableS6$netYieldReference,2)," (",round(tableS6$netYieldReferenceSD,2),")")
tableS6$yieldCompensation <- paste0(round(tableS6$gap,2)," (",round(tableS6$gapSD,2),")")
tableS6 <- tableS6[,c("Crop","Yield","netYield","netYieldReference","yieldCompensation")]

# write.xlsx(tableS6,"tableS6.xlsx")

rm(list=ls())
