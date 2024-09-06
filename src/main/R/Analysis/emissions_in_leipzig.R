library(gridExtra)
library(tidyverse)
library(lubridate)
library(viridis)
library(ggsci)
library(sf)
library(ggplot2)
library(matsim)
library(dplyr)
library(purrr)

### Global declaring ###

Scenario_names <- data.frame(Scenario = c("Base", "99", "95", "90"))
AverageScoresname <- data.frame(Scenario = c("Base", "99", "95", "90"))
my_colors <- c("red", "green", "blue", "yellow")


network <- loadNetwork("./Input/leipzig-flexa-25pct-scaledFleet-base_noDepot.output_network.xml.gz")

links_network <- data.frame(links_Leipzig[2])



## Reading shape files

shape_L <- st_read("./Input/Leipzig_stadt.shp", crs=25832)
shape_99 <-  st_read("./Input/Zonen99_update.shp", crs=25832)
shape_95 <-  st_read("./Input/Zonen95_update.shp", crs=25832)
shape_90 <-  st_read("./Input/Zonen90_update.shp", crs=25832)

#links in Leipzig_Stadt
links_Leipzig <- links_network %>% st_as_sf(coords = c("links.x.from", "links.y.from"), crs = 25832) %>% st_filter(shape_L)
#links in Zonen
links_Zonen99 <- links_network %>% st_as_sf(coords = c("links.x.from", "links.y.from"), crs = 25832) %>% st_filter(shape_99)
links_Zonen95 <- links_network %>% st_as_sf(coords = c("links.x.from", "links.y.from"), crs = 25832) %>% st_filter(shape_95)
links_Zonen90 <- links_network %>% st_as_sf(coords = c("links.x.from", "links.y.from"), crs = 25832) %>% st_filter(shape_90)

length(which(links_Leipzig$links.type == "highway.motorway"))
length(which(links_Leipzig$links.type == "highway.motorway_link"))
length(which(links_Leipzig$links.type == "highway.service"))
length(which(links_Leipzig$links.type == "highway.primary"))
length(which(links_Leipzig$links.type == "highway.primary_link"))
length(which(links_Leipzig$links.type == "highway.secondary"))
length(which(links_Leipzig$links.type == "highway.secondary_link"))
length(which(links_Leipzig$links.type == "highway.tertiary"))
length(which(links_Leipzig$links.type == "highway.residential"))
length(which(links_Leipzig$links.type == "highway.living_street"))
length(which(links_Leipzig$links.type == "highway.trunk"))
length(which(links_Leipzig$links.type == "highway.trunk_link"))
length(which(links_Leipzig$links.type == "highway.unclassified"))

Haupt_Leipzig <- filter(links_Leipzig, links.type == "highway.primary" | links.type =="highway.primary_link" | links.type == "highway.secondary" | links.type =="highway.secondary_link" | links.type =="highway.service" | links.type =="highway.motorway")
Neben_Leipzig <- filter(links_Leipzig, links.type == "highway.residential" | links.type == "highway.tertiary" | links.type == "highway.living_street" | links.type == "highway.unclassified")


# Reading Emission files

emissions_file_base <- read_delim("./Input/leipzig-flexa-25pct-scaledFleet-base_noDepot.emissionsPerLinkPerM.csv")
emissions_file_99 <- read_delim("./Input/leipzig-flexa-25pct-scaledFleet-carfree99pct_noDepot.emissionsPerLinkPerM.csv")
emissions_file_95 <- read_delim("./Input/leipzig-flexa-25pct-scaledFleet-carfree95pct_noDepot.emissionsPerLinkPerM.csv")
emissions_file_90 <- read_delim("./Input/leipzig-flexa-25pct-scaledFleet-carfree90pct_noDepot.emissionsPerLinkPerM.csv")



### Leipzig links analysis ###


## To ensure table formatting consistency, we need to standardize column names across all tables.
## Therefore, we must modify the first column's name in the second table ##

colnames(links_Leipzig)[1] <- "linkId"
colnames(links_Zonen99)[1] <- "linkId"
colnames(links_Zonen95)[1] <- "linkId"
colnames(links_Zonen90)[1] <- "linkId"


## The link data is currently dispersed across two separate tables; in the following lines, the link data will be merged ##

## Link's data merge for base area ##

LinksBase_emission_PolicyBase <- merge(emissions_file_base, links_Leipzig, by = 'linkId', all.x = FALSE)
LinksBase_emission_Policy99 <- merge(emissions_file_99, links_Leipzig, by = 'linkId', all.x = FALSE)
LinksBase_emission_Policy95 <- merge(emissions_file_95, links_Leipzig, by = 'linkId', all.x = FALSE)
LinksBase_emission_Policy90 <- merge(emissions_file_90, links_Leipzig, by = 'linkId', all.x = FALSE)

## Link's data merge for small car free area ##

Links99_emission_PolicyBase <- merge(emissions_file_base, links_Zonen99, by = 'linkId', all.x = FALSE)
Links99_emission_Policy99 <- merge(emissions_file_99, links_Zonen99, by = 'linkId', all.x = FALSE)
Links99_emission_Policy95 <- merge(emissions_file_95, links_Zonen99, by = 'linkId', all.x = FALSE)
Links99_emission_Policy90 <- merge(emissions_file_90, links_Zonen99, by = 'linkId', all.x = FALSE)

## Link's data merge for medium car free area ##

Links95_emission_PolicyBase <- merge(emissions_file_base, links_Zonen95, by = 'linkId', all.x = FALSE)
Links95_emission_Policy99 <- merge(emissions_file_99, links_Zonen95, by = 'linkId', all.x = FALSE)
Links95_emission_Policy95 <- merge(emissions_file_95, links_Zonen95, by = 'linkId', all.x = FALSE)
Links95_emission_Policy90 <- merge(emissions_file_90, links_Zonen95, by = 'linkId', all.x = FALSE)

## Link's data merge for big car free area ##

Links90_emission_PolicyBase <- merge(emissions_file_base, links_Zonen90, by = 'linkId', all.x = FALSE)
Links90_emission_Policy99 <- merge(emissions_file_99, links_Zonen90, by = 'linkId', all.x = FALSE)
Links90_emission_Policy95 <- merge(emissions_file_95, links_Zonen90, by = 'linkId', all.x = FALSE)
Links90_emission_Policy90 <- merge(emissions_file_90, links_Zonen90, by = 'linkId', all.x = FALSE)

## Emission calculations of base area ##

## CO calculation ##

CO_linkBase_base <- sum(LinksBase_emission_PolicyBase$`CO [g/m]`*LinksBase_emission_PolicyBase$links.length)
CO_linkBase_99 <- sum(LinksBase_emission_Policy99$`CO [g/m]`*LinksBase_emission_Policy99$links.length)
CO_linkBase_95 <- sum(LinksBase_emission_Policy95$`CO [g/m]`*LinksBase_emission_Policy95$links.length)
CO_linkBase_90 <- sum(LinksBase_emission_Policy90$`CO [g/m]`*LinksBase_emission_Policy90$links.length)

CO_linkBase <- rbind(CO_linkBase_base,CO_linkBase_99,CO_linkBase_95,CO_linkBase_90)
CO_linkBase <- cbind(Scenario_names,CO_linkBase)

## CO2 Calculation ##

CO2_linkBase_base <- sum(LinksBase_emission_PolicyBase$`CO2_TOTAL [g/m]`*LinksBase_emission_PolicyBase$links.length)
CO2_linkBase_99 <- sum(LinksBase_emission_Policy99$`CO2_TOTAL [g/m]`*LinksBase_emission_Policy99$links.length)
CO2_linkBase_95 <- sum(LinksBase_emission_Policy95$`CO2_TOTAL [g/m]`*LinksBase_emission_Policy95$links.length)
CO2_linkBase_90 <- sum(LinksBase_emission_Policy90$`CO2_TOTAL [g/m]`*LinksBase_emission_Policy90$links.length)

CO2_linkBase <- rbind(CO2_linkBase_base,CO2_linkBase_99,CO2_linkBase_95,CO2_linkBase_90)
CO2_linkBase <- cbind(Scenario_names,CO2_linkBase)

## Emission calculations of small car free area ##

## CO calculation ##

CO_link99_base <- sum(Links99_emission_PolicyBase$`CO [g/m]`*Links99_emission_PolicyBase$links.length)
CO_link99_99 <- sum(Links99_emission_Policy99$`CO [g/m]`*Links99_emission_Policy99$links.length)
CO_link99_95 <- sum(Links99_emission_Policy95$`CO [g/m]`*Links99_emission_Policy95$links.length)
CO_link99_90 <- sum(Links99_emission_Policy90$`CO [g/m]`*Links99_emission_Policy90$links.length)

CO_link99 <- rbind(CO_link99_base,CO_link99_99,CO_link99_95,CO_link99_90)
CO_link99 <- cbind(Scenario_names,CO_link99)

## CO2 Calculation ##

CO2_link99_base <- sum(Links99_emission_PolicyBase$`CO2_TOTAL [g/m]`*Links99_emission_PolicyBase$links.length)
CO2_link99_99 <- sum(Links99_emission_Policy99$`CO2_TOTAL [g/m]`*Links99_emission_Policy99$links.length)
CO2_link99_95 <- sum(Links99_emission_Policy95$`CO2_TOTAL [g/m]`*Links99_emission_Policy95$links.length)
CO2_link99_90 <- sum(Links99_emission_Policy90$`CO2_TOTAL [g/m]`*Links99_emission_Policy90$links.length)

CO2_link99 <- rbind(CO2_link99_base,CO2_link99_99,CO2_link99_95,CO2_link99_90)
CO2_link99 <- cbind(Scenario_names,CO2_link99)


## Emission calculations of medium car free area ##


## CO calculation ##

CO_link95_base <- sum(Links95_emission_PolicyBase$`CO [g/m]`*Links95_emission_PolicyBase$links.length)
CO_link95_99 <- sum(Links95_emission_Policy99$`CO [g/m]`*Links95_emission_Policy99$links.length)
CO_link95_95 <- sum(Links95_emission_Policy95$`CO [g/m]`*Links95_emission_Policy95$links.length)
CO_link95_90 <- sum(Links95_emission_Policy90$`CO [g/m]`*Links95_emission_Policy90$links.length)

CO_link95 <- rbind(CO_link95_base,CO_link95_99,CO_link95_95,CO_link95_90)
CO_link95 <- cbind(Scenario_names,CO_link95)

## CO2 Calculation ##

CO2_link95_base <- sum(Links95_emission_PolicyBase$`CO2_TOTAL [g/m]`*Links95_emission_PolicyBase$links.length)
CO2_link95_99 <- sum(Links95_emission_Policy99$`CO2_TOTAL [g/m]`*Links95_emission_Policy99$links.length)
CO2_link95_95 <- sum(Links95_emission_Policy95$`CO2_TOTAL [g/m]`*Links95_emission_Policy95$links.length)
CO2_link95_90 <- sum(Links95_emission_Policy90$`CO2_TOTAL [g/m]`*Links95_emission_Policy90$links.length)

CO2_link95 <- rbind(CO2_link95_base,CO2_link95_99,CO2_link95_95,CO2_link95_90)
CO2_link95 <- cbind(Scenario_names,CO2_link95)


## Emissions of all kinds are generated within a 90% spatial area ##

## CO calculation ##

CO_link90_base <- sum(Links90_emission_PolicyBase$`CO [g/m]`*Links90_emission_PolicyBase$links.length)
CO_link90_99 <- sum(Links90_emission_Policy99$`CO [g/m]`*Links90_emission_Policy99$links.length)
CO_link90_95 <- sum(Links90_emission_Policy95$`CO [g/m]`*Links90_emission_Policy95$links.length)
CO_link90_90 <- sum(Links90_emission_Policy90$`CO [g/m]`*Links90_emission_Policy90$links.length)

CO_link90 <- rbind(CO_link90_base,CO_link90_99,CO_link90_95,CO_link90_90)
CO_link90 <- cbind(Scenario_names,CO_link90)

## CO2 Calculation ##

CO2_link90_base <- sum(Links90_emission_PolicyBase$`CO2_TOTAL [g/m]`*Links90_emission_PolicyBase$links.length)
CO2_link90_99 <- sum(Links90_emission_Policy99$`CO2_TOTAL [g/m]`*Links90_emission_Policy99$links.length)
CO2_link90_95 <- sum(Links90_emission_Policy95$`CO2_TOTAL [g/m]`*Links90_emission_Policy95$links.length)
CO2_link90_90 <- sum(Links90_emission_Policy90$`CO2_TOTAL [g/m]`*Links90_emission_Policy90$links.length)

CO2_link90 <- rbind(CO2_link90_base,CO2_link90_99,CO2_link90_95,CO2_link90_90)
CO2_link90 <- cbind(Scenario_names,CO2_link90)



### preparing data-files for exporting needed for SimWrapper ###

CO_linkBase_simwrapper <- t(CO_linkBase)
CO_link99_simwrapper <- t(CO_link99)
CO_link95_simwrapper <- t(CO_link95)
CO_link90_simwrapper <- t(CO_link90)

CO2_linkBase_simwrapper <- t(CO2_linkBase)
CO2_link99_simwrapper <- t(CO2_link99)
CO2_link95_simwrapper <- t(CO2_link95)
CO2_link90_simwrapper <- t(CO2_link90)

### Exporting data-files into csv (Needed for SimWrapper) ###


write.table(CO_linkBase_simwrapper, "output\\CO_linkbase.csv", sep = ",", col.names = FALSE, quote = FALSE)
write.table(CO_link99_simwrapper, "output\\CO_link99.csv", sep = ",", col.names = FALSE, quote = FALSE)
write.table(CO_link95_simwrapper, "output\\CO_link95.csv", sep = ",", col.names = FALSE, quote = FALSE)
write.table(CO_link90_simwrapper, "output\\CO_link90.csv", sep = ",", col.names = FALSE, quote = FALSE)


write.table(CO2_linkBase_simwrapper, "output\\CO2_linkBase.csv" , sep = ",", col.names = FALSE,quote = FALSE)
write.table(CO2_link99_simwrapper, "output\\CO2_link99.csv" , sep = ",", col.names = FALSE,quote = FALSE)
write.table(CO2_link95_simwrapper, "output\\CO2_link95.csv" , sep = ",", col.names = FALSE,quote = FALSE)
write.table(CO2_link90_simwrapper, "output\\CO2_link90.csv" , sep = ",", col.names = FALSE,quote = FALSE)






#Emissionen auf Nebenstraßenlinks in Leipzig
emissions_neben_leipzig <- 
  mean()

#Emissionen auf Nebenstraßenlinks innerhalb Zonen
emissions_neben_zonen99 <- 
  mean()
emissions_neben_zonen95 <- 
  mean()
emissions_neben_zonen90 <- 
  mean()

#Verkehr auf Hauptstraßenlinks in Leipzig
traffic_haupt_leipzig <- 
  mean()


#Verkehr auf Nebenstraßenlinks in Leipzig
traffic_neben_leipzig <- 
  mean()


#Verkehr auf Nebenstraßenlinks innerhalb Zonen
traffic_neben_zonen99 <- 
  mean()
traffic_neben_zonen95 <- 
  mean()
traffic_neben_zonen90 <- 
  mean()


