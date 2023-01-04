
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

dfKategorienBeretta <- read_excel("Data/crops_categories_nutrients.xlsx")
dfKategorienBeretta <- merge(dfKategorienBeretta,CropTable,by="Kultur")

manual.labs.english <- c(production = "Production", 
                 intermediate = "Distribution", 
                 household = "Consumption",
                 fresh="Fresh vegetables",
                 storable="Storable vegetables",
                 potato="Potatoes",
                 Conventional="Conventional", 
                 CSA="CSA",
                 Ertrag_kg_qm = "Yield harvested",
                 deliveredYield ="Collected yield",
                 netYield = "Net yield",
                 potYield = "Potential yield")

## CSA
# production
Nachverwertung_Produktion <-  as.data.frame(read_excel("Data/csa_production_flw.xlsx"))
head(Nachverwertung_Produktion)
Nachverwertung_Produktion$potYield <- (Nachverwertung_Produktion$Ertrag_kg_geerntet+Nachverwertung_Produktion$Nachverwertung_kg_total)/Nachverwertung_Produktion$Anbauflaeche

# distribution
Nachverwertung_Verteilstation <- as.data.frame(read_excel("Data/csa_distribution_flw.xlsx"))
head(Nachverwertung_Verteilstation)

# consumption
Nachverwertung_Konsum <- as.data.frame(read_excel("Data/csa_consumption_flw.xlsx"))
head(Nachverwertung_Konsum)
Nachverwertung_Konsum[which(Nachverwertung_Konsum$calculation_basis=="kcal"),"calculation_basis"] <- "Calories"
Nachverwertung_Konsum[which(Nachverwertung_Konsum$calculation_basis=="prot"),"calculation_basis"] <- "Protein"
Nachverwertung_Konsum[which(Nachverwertung_Konsum$calculation_basis=="fat"),"calculation_basis"] <- "Fat"

dfRatioHousehold <- merge(Nachverwertung_Konsum[which(Nachverwertung_Konsum$Kultur=="All_mean"),2:3],Nachverwertung_Konsum[which(Nachverwertung_Konsum$Kultur=="All_sd"),2:3],by="calculation_basis")
names(dfRatioHousehold) <- c("Nutrient","Ratio","RatioSD")
dfRatioHousehold$Aspect <- "CSA"
dfRatioHousehold$Stage <- "household"

# merge
dfAll <- merge(Nachverwertung_Produktion,Nachverwertung_Verteilstation,by=c("Solawi","Kultur"))
dfAll <- merge(dfAll,Nachverwertung_Konsum,by="Kultur",all=T)
dfAll <- merge(dfAll,dfKategorienBeretta,by="Kultur")
# dfAll[is.na(dfAll)] <- 0

# calculate nutrients
dfAllNutrients <- dfAll[,c("Solawi","Kultur","Kategorie_Beretta","Calories (kcal/100g)","Protein (g/100g)","Fat (g/100g)","Ertrag_kg","Ertrag_kg_qm","Ertrag_kg_geerntet_qm","Nachverwertung_kg_total","Liefermenge_kg_VST","Nachverwertung_kg_VST")]
names(dfAllNutrients) <- c("Solawi","Kultur","Kategorie_Beretta","Calories","Protein","Fat","Produktion","Ertrag","Ertrag_geerntet","ProduktionN","Verteilstation","VerteilstationN")
dfCalories <- dfProtein <- dfFat <- dfAllNutrients
dfCalories[7:ncol(dfCalories)] <- dfCalories[7:ncol(dfCalories)]*dfCalories$Calories*10
dfCalories$Nutrient <- "Calories"
dfProtein[7:ncol(dfProtein)] <- dfProtein[7:ncol(dfProtein)]*dfProtein$Protein*10
dfProtein$Nutrient <- "Protein"
dfFat[7:ncol(dfFat)]<- dfFat[7:ncol(dfFat)]*dfFat$Fat*10
dfFat$Nutrient <- "Fat"

dfNutrient <- rbind(dfCalories,dfProtein,dfFat)
dfNutrient <- dfNutrient[,-which(names(dfNutrient) %in% c("Calories","Protein","Fat"))]

## Conventional
dfErtraegeKonventionell <- read_excel("Data/conventional_production.xlsx")
dfErtraegeKonventionell <- merge(dfErtraegeKonventionell,CropTable,by="Kultur")

dfNachverwertungKonventionell <- read_excel("Data/conventional_flw.xlsx")
head(dfNachverwertungKonventionell)
dfNachverwertungKonventionell$Aspect <- "conventional"
dfNachverwertungKonventionell$Solawi <- dfNachverwertungKonventionell$Reference

dfNachverwertungKonventionell <- merge(dfNachverwertungKonventionell,dfKategorienBeretta,by="Crop")

################### analyses ################### 

###### Ratio losses all
## CSA
dfNutrientSum <- aggregate(cbind(Produktion,ProduktionN,Verteilstation,VerteilstationN)~Solawi+Nutrient,dfNutrient,sum)
dfNutrientSum$ratio_production_total <- dfNutrientSum$ProduktionN/(dfNutrientSum$ProduktionN+dfNutrientSum$Produktion)
dfNutrientSum$ratio_intermediate_total <- dfNutrientSum$VerteilstationN/(dfNutrientSum$VerteilstationN+dfNutrientSum$Verteilstation)
dfNutrientSumFinal <- dfNutrientSum[,c("Solawi","Nutrient","ratio_production_total","ratio_intermediate_total")]  %>% gather(Variable, Ratio, "ratio_production_total":"ratio_intermediate_total")
dfNutrientSumFinal$Stage <- sapply(strsplit(dfNutrientSumFinal$Variable,"_"), `[`, 2)
dfNutrientSumFinal$Aspect <- "CSA"
  
## conventional
# sum of avoidability categories
dfNachverwertungKonventionellMean <- aggregate(Ratio~Crop+Kategorie_Beretta+Stage+Avoidability+Solawi+Aspect,dfNachverwertungKonventionell,mean)
dfNachverwertungKonventionellSum <- aggregate(Ratio~Solawi+Stage+Aspect+Crop,dfNachverwertungKonventionellMean,sum)
# mean of crops
dfNachverwertungKonventionellMeanCrops <- aggregate(Ratio~Solawi+Stage+Aspect,dfNachverwertungKonventionellSum,mean)
dfNachverwertungKonventionellMeanCrops$Nutrient <- "None"

## combined
dfAllMean <- rbind(dfNutrientSumFinal[,c("Solawi","Stage","Aspect","Ratio","Nutrient")],dfNachverwertungKonventionellMeanCrops)

# mean and sd over solawi/references
dfRatioFinalSD <- aggregate(Ratio~Aspect+Stage+Nutrient,dfAllMean,sd)
names(dfRatioFinalSD)[ncol(dfRatioFinalSD)] <- "RatioSD"

dfRatioFinalMean <- aggregate(Ratio~Aspect+Stage+Nutrient,dfAllMean,mean)
dfRatioFinal <- merge(dfRatioFinalMean,dfRatioFinalSD)
dfRatioFinal <- rbind(dfRatioFinal,dfRatioHousehold)

dfRatioFinal$Error_Lower <- dfRatioFinal$Ratio-dfRatioFinal$RatioSD
dfRatioFinal$Error_Upper <- dfRatioFinal$Ratio+dfRatioFinal$RatioSD

dfRatioFinal$Stage <- factor(dfRatioFinal$Stage,levels=c("production","intermediate","household"))

## significance tests
funFigRatio <- function(nutrient){
  print(shapiro.test(dfAllMean[which(dfAllMean$Aspect=="CSA"&dfAllMean$Nutrient==nutrient&dfAllMean$Stage=="production"),"Ratio"]))
  print(shapiro.test(dfAllMean[which(dfAllMean$Aspect=="CSA"&dfAllMean$Nutrient==nutrient&dfAllMean$Stage=="intermediate"),"Ratio"]))
  print(shapiro.test(dfAllMean[which(dfAllMean$Aspect=="conventional"&dfAllMean$Stage=="production"),"Ratio"]))
  print(shapiro.test(dfAllMean[which(dfAllMean$Aspect=="conventional"&dfAllMean$Stage=="intermediate"),"Ratio"]))
  print(shapiro.test(dfAllMean[which(dfAllMean$Aspect=="conventional"&dfAllMean$Stage=="household"),"Ratio"]))

  aovRatioProduction <- aov(Ratio ~ Aspect , data = dfAllMean[which(dfAllMean$Nutrient%in%c("None",nutrient)&dfAllMean$Stage=="production"),])
  aovRatioIntermediate <- aov(Ratio ~ Aspect , data = dfAllMean[which(dfAllMean$Nutrient%in%c("None",nutrient)&dfAllMean$Stage=="intermediate"),])
  aovRatioHousehold <- aov(Ratio ~ Aspect , data = rbind(dfAllMean[which(dfAllMean$Nutrient%in%c("None",nutrient)&dfAllMean$Stage=="household"),c("Aspect","Ratio")],dfRatioHousehold[which(dfRatioHousehold$Nutrient==nutrient),c("Aspect","Ratio")]))

  ann_text <- data.frame(Ratio = 0.5,lab = paste0("p = ",c(round(summary(aovRatioProduction)[[1]][1,5],2),round(summary(aovRatioIntermediate)[[1]][1,5],2),round(summary(aovRatioHousehold)[[1]][1,5],2))),Aspect="conventional",
                         Stage  = factor(c("production","intermediate","household"),levels = c("production","intermediate","household")))
  
  ggplot(dfRatioFinal[which(dfRatioFinal$Nutrient%in%c("None",nutrient)),], aes(x = Aspect, y = Ratio)) + 
    geom_bar(stat = 'identity', position = 'stack', colour = "black") + 
    facet_grid(.~Stage,labeller = as_labeller(manual.labs.english)) + ylab("Ratio")+xlab("") +
    scale_fill_brewer(name = "Avoidability", labels = as_labeller(manual.labs.english)) +
    ylim(0,0.5)+
    theme(axis.title.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.text.x = element_text(angle = 45, hjust = 1,size=8),
          axis.text.y = element_text(size=8))+
    geom_errorbar(aes(ymin= Error_Lower, 
                      ymax= Error_Upper),
                  width=.2,
                  position="identity")+
    geom_text(data = ann_text,aes(label=lab))
}

fig1 <- funFigRatio("Calories")
fig1

figS1 <- funFigRatio("Protein")
figS1

figS2 <- funFigRatio("Fat")
figS2

###### Net yield efficiency

#### mean and sd per Crop and Stage
## CSA
dfNYEAll <- dfAll[, c("Solawi", "Kultur", "Ertrag_kg_qm", "Ertrag_kg_geerntet_qm","Loss_Ratio_VST","ratio_gesamt", "potYield")]
dfNYEAll$deliveredYield <- dfNYEAll$Ertrag_kg_qm * (1-dfNYEAll$Loss_Ratio_VST )  
dfNYEAll$netYield <- dfNYEAll$Ertrag_kg_qm * (1-dfNYEAll$Loss_Ratio_VST ) * (1-dfNYEAll$ratio_gesamt) 
dfNYEAll$Ertrag_kg_qm <- dfNYEAll$Ertrag_kg_geerntet_qm # make name consistent
dfNYEAll$Aspect <- "CSA"
dfNYEAll <- merge(dfNYEAll,CropTable,by="Kultur")


## Conventional
dfNachverwertungKonventionellSummary <- as.data.frame(dfNachverwertungKonventionellSum %>%
  # drop_na()  %>%
  group_by(Crop, Stage)  %>%
  dplyr::summarize(mean = mean(Ratio,na.rm=T),
                   sd = sd(Ratio, na.rm=T)))

dfNachverwertungKonventionellSummary
head(dfNachverwertungKonventionellSummary)
dfNachverwertungKonventionellSummary <- merge(dfNachverwertungKonventionellSummary,unique(dfAll[,c("Crop","Calories (kcal/100g)")]),by="Crop")

## sample ratios based on given mean and sd
set.seed(9999)
lsRound <- lapply(1:1000,function(i){
  dfNachverwertungKonventionellSummary$RatioSampled <- rnorm(nrow(dfNachverwertungKonventionellSummary),mean=dfNachverwertungKonventionellSummary[,"mean"],sd=dfNachverwertungKonventionellSummary[,"sd"])
  
  dfNachverwertungKonventionellSummary[which(dfNachverwertungKonventionellSummary$RatioSampled<0),"RatioSampled"] <- 0
  dfNachverwertungKonventionellSummary[which(dfNachverwertungKonventionellSummary$RatioSampled>1),"RatioSampled"] <- 1
  
  
  dfNachverwertungKonventionellSpread<- dfNachverwertungKonventionellSummary[,c("Crop","Stage","RatioSampled")] %>% tidyr::spread(Stage,RatioSampled)
  
  dfNetYieldKonventionell <- merge(dfErtraegeKonventionell[,c("Crop","Ertrag_kg_qm")],dfNachverwertungKonventionellSpread,by="Crop")
  dfNetYieldKonventionell <- na.omit(dfNetYieldKonventionell)
  dfNetYieldKonventionell$potYield <- dfNetYieldKonventionell$Ertrag_kg_qm + (dfNetYieldKonventionell$Ertrag_kg_qm*dfNetYieldKonventionell$production) 
  dfNetYieldKonventionell$deliveredYield <- dfNetYieldKonventionell$Ertrag_kg_qm * (1-dfNetYieldKonventionell$intermediate) 
  
  dfNetYieldKonventionell$netYield <- dfNetYieldKonventionell$Ertrag_kg_qm * (1-dfNetYieldKonventionell$intermediate) * (1-dfNetYieldKonventionell$household) 
  dfNetYieldKonventionell$Solawi <- i
  dfNetYieldKonventionell[,c("Crop","Ertrag_kg_qm","potYield","deliveredYield","netYield","Solawi")]
  
  })
dfRound <- do.call(rbind,lsRound)
dfRound$Aspect <- "conventional"

# combine csa and conventional
dfNYEFinal <- rbind(dfNYEAll[-which(dfNYEAll$Crop%in%c("Fennel","Spinach beet")),c("Crop","Ertrag_kg_qm","potYield","deliveredYield","netYield","Solawi","Aspect")],dfRound)
dfNYEFinal <- dfNYEFinal %>% gather(Variable,Yield,"Ertrag_kg_qm":"netYield")
head(dfNYEFinal)
unique(dfNYEFinal$Variable)
dfNYEFinal <- merge(dfNYEFinal,unique(dfAll[,c("Crop","Calories (kcal/100g)","Protein (g/100g)","Fat (g/100g)")]),by="Crop")
dfNYEFinal$Yield_Calories <- dfNYEFinal$Yield*dfNYEFinal$`Calories (kcal/100g)`*10
dfNYEFinal$Yield_Protein <- dfNYEFinal$Yield*dfNYEFinal$`Protein (g/100g)`*10
dfNYEFinal$Yield_Fat <- dfNYEFinal$Yield*dfNYEFinal$`Fat (g/100g)`*10

# Nutrients
dfNYENutrient <- aggregate(cbind(Yield_Calories,Yield_Protein,Yield_Fat)~Solawi+Aspect+Variable,dfNYEFinal,function(i){sum(i,na.rm=T)})
dfNYENutrientSD <- aggregate(cbind(Yield_Calories,Yield_Protein,Yield_Fat)~Aspect+Variable,dfNYENutrient,function(i){sd(i,na.rm=T)})
names(dfNYENutrientSD)[3:5] <- paste0(names(dfNYENutrientSD)[3:5],"SD")
dfNYENutrientMean <- aggregate(cbind(Yield_Calories,Yield_Protein,Yield_Fat)~Aspect+Variable,dfNYENutrient,function(i){mean(i,na.rm=T)})
dfNYENutrientAgg <- merge(dfNYENutrientMean,dfNYENutrientSD,by=c("Aspect","Variable"))
dfNYENutrientAgg$Error_Upper_Calories <- dfNYENutrientAgg$Yield_Calories+dfNYENutrientAgg$Yield_CaloriesSD
dfNYENutrientAgg$Error_Lower_Calories <- dfNYENutrientAgg$Yield_Calories-dfNYENutrientAgg$Yield_CaloriesSD
dfNYENutrientAgg$Error_Upper_Protein <- dfNYENutrientAgg$Yield_Protein+dfNYENutrientAgg$Yield_ProteinSD
dfNYENutrientAgg$Error_Lower_Protein <- dfNYENutrientAgg$Yield_Protein-dfNYENutrientAgg$Yield_ProteinSD
dfNYENutrientAgg$Error_Upper_Fat <- dfNYENutrientAgg$Yield_Fat+dfNYENutrientAgg$Yield_FatSD
dfNYENutrientAgg$Error_Lower_Fat <- dfNYENutrientAgg$Yield_Fat-dfNYENutrientAgg$Yield_FatSD

dfNYENutrientAgg$Variable <- factor(dfNYENutrientAgg$Variable,levels = c("potYield","Ertrag_kg_qm","deliveredYield","netYield"))

# figure
funFigYield <- function(nutrient,errorLower,errorUpper,yName){
  dfNYENutrient$Yield <- dfNYENutrient[,nutrient]
  ## significance
  print(shapiro.test(dfNYENutrient[which(dfNYENutrient$Aspect=="conventional"&dfNYENutrient$Variable=="potYield"),nutrient]))
  print(shapiro.test(dfNYENutrient[which(dfNYENutrient$Aspect=="CSA"&dfNYENutrient$Variable=="potYield"),nutrient]))
  print(shapiro.test(dfNYENutrient[which(dfNYENutrient$Aspect=="conventional"&dfNYENutrient$Variable=="deliveredYield"),nutrient]))
  print(shapiro.test(dfNYENutrient[which(dfNYENutrient$Aspect=="CSA"&dfNYENutrient$Variable=="deliveredYield"),nutrient]))
  print(shapiro.test(dfNYENutrient[which(dfNYENutrient$Aspect=="conventional"&dfNYENutrient$Variable=="netYield"),nutrient]))
  print(shapiro.test(dfNYENutrient[which(dfNYENutrient$Aspect=="CSA"&dfNYENutrient$Variable=="netYield"),nutrient]))
  
  aovPotYield <- aov(Yield ~ Aspect , data = dfNYENutrient[which(dfNYENutrient$Variable=="potYield"),])
  aovYield <- aov(Yield ~ Aspect , data = dfNYENutrient[which(dfNYENutrient$Variable=="Ertrag_kg_qm"),])
  aovDeliveredYield <- aov(Yield ~ Aspect , data = dfNYENutrient[which(dfNYENutrient$Variable=="deliveredYield"),])
  aovNetYield <- aov(Yield ~ Aspect , data = dfNYENutrient[which(dfNYENutrient$Variable=="netYield"),])
  
  ann_text <- data.frame(Var = 1.1*max(dfNYENutrientAgg[,nutrient]),lab = paste0("p = ",c(format(round(summary(aovPotYield)[[1]][1,5],2), nsmall = 2),"NA",format(round(summary(aovDeliveredYield)[[1]][1,5],2), nsmall = 2),format(round(summary(aovNetYield)[[1]][1,5],2), nsmall = 2))),Aspect="conventional",
                         Variable  = factor(c("potYield","Ertrag_kg_qm","deliveredYield","netYield"),levels = c("potYield","Ertrag_kg_qm","deliveredYield","netYield")))
  
  # ann_text$lab <- list(c(1,2,3,expression("">=200)))
  names(ann_text)[1] <- nutrient

  ggplot(dfNYENutrientAgg, aes(x = Aspect, y = dfNYENutrientAgg[,nutrient])) + 
    geom_bar(stat = 'identity', position= "dodge", colour = "black") +
    facet_grid(.~Variable,labeller = as_labeller(manual.labs.english))+
    ylab(yName) + xlab("") +
    # ylim(0,17500)+
    theme(axis.title.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.text.x = element_text(angle = 45, hjust = 1,size=8),
          axis.text.y = element_text(size=8),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 8))  +
    geom_errorbar(aes(ymin= dfNYENutrientAgg[,errorLower],
                      ymax= dfNYENutrientAgg[,errorUpper]),
                  width=.2,
                  position="identity")+
    geom_text(data = ann_text,aes(y=ann_text[,nutrient],label=lab))
  
}


fig2 <-  funFigYield("Yield_Calories","Error_Lower_Calories","Error_Upper_Calories",expression("Yield ( kcal"~m^-2~")"))
fig2

figS3 <- funFigYield("Yield_Protein","Error_Lower_Protein","Error_Upper_Protein",expression("Yield ( g"~m^-2~")"))
figS3

figS4 <- funFigYield("Yield_Fat","Error_Lower_Fat","Error_Upper_Fat",expression("Yield ( g"~m^-2~")"))
figS4

###### crop tables
#### losses
## CSA
dfCSAMean  <- aggregate(cbind(Nachverwertung_ratio_total,Loss_Ratio_VST ,ratio_gesamt)~Crop,dfAll,mean)
dfCSAsd  <- aggregate(cbind(Nachverwertung_ratio_total,Loss_Ratio_VST ,ratio_gesamt)~Crop,dfAll,sd)
dfCSAMean <- merge(dfCSAMean,dfCSAsd,by="Crop")
dfCSAMean$ratio_production_CSA <- paste0(round(dfCSAMean$Nachverwertung_ratio_total.x,2)," (",round(dfCSAMean$Nachverwertung_ratio_total.y,2),")")
dfCSAMean$ratio_intermediate_CSA <- paste0(round(dfCSAMean$Loss_Ratio_VST.x,2)," (",round(dfCSAMean$Loss_Ratio_VST.y,2),")")
dfCSAMean$ratio_household_CSA <- paste0(round(dfCSAMean$ratio_gesamt.x,2)," (",round(dfCSAMean$ratio_gesamt.y,2),")")

## conventional
head(dfNachverwertungKonventionellMean)
dfKonventionellMean <- aggregate(Ratio~Crop+Stage,dfNachverwertungKonventionellMean,mean)
dfKonventionellSD <- aggregate(Ratio~Crop+Stage,dfNachverwertungKonventionellMean,sd)
dfKonventionellMean <- merge(dfKonventionellMean,dfKonventionellSD,by=c("Crop","Stage"))
dfKonventionellMean$Ratio <- paste0(round(dfKonventionellMean$Ratio.x,2)," (",round(dfKonventionellMean$Ratio.y,2),")")
dfKonventionellMean <- dcast(setDT(dfKonventionellMean), Crop ~ Stage, value.var = c("Ratio"))
names(dfKonventionellMean)[2:4] <- paste0("ratio_",names(dfKonventionellMean)[2:4],"_conventional")

tableS5 <- merge(dfCSAMean[,c("Crop","ratio_production_CSA","ratio_intermediate_CSA","ratio_household_CSA")],dfKonventionellMean[,c("Crop","ratio_production_conventional","ratio_intermediate_conventional","ratio_household_conventional")],by="Crop")
tableS5

#### yields
head(dfNYEFinal)
dfNYETable <- aggregate(Yield~Crop+Aspect+Variable,dfNYEFinal,mean)
dfNYETableSD <- aggregate(Yield~Crop+Aspect+Variable,dfNYEFinal,sd)

dfNYETableAll <- merge(dfNYETable,dfNYETableSD,by=c("Crop","Aspect","Variable"))
dfNYETableAll$Yield <- paste0(round(dfNYETableAll$Yield.x,2)," (",round(dfNYETableAll$Yield.y,2),")")
dfNYETableAll <- dcast(setDT(dfNYETableAll), Crop ~ Aspect+Variable, value.var = c("Yield"))
dfNYETableAll <- dfNYETableAll[,c("Crop","CSA_potYield","CSA_Ertrag_kg_qm","CSA_deliveredYield","CSA_netYield","conventional_potYield","conventional_Ertrag_kg_qm","conventional_deliveredYield","conventional_netYield")]
names(dfNYETableAll) <- c("Crop","potential_yield_CSA_kg_m2","harvested_yield_CSA_kg_m2","delivered_yield_CSA_kg_m2","net_yield_CSA_kg_m2","potential_yield_conventional_kg_m2","harvested_yield_conventional_kg_m2","delivered_yield_conventional_kg_m2","net_yield_conventional_kg_m2")
tableS6 <- dfNYETableAll
tableS6


rm(list=ls())
