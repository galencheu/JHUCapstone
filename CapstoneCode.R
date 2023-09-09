### JHU Data Analytics and Policy Capstone
### https://jscholarship.library.jhu.edu/handle/1774.2/68156

### Pre-processing was done on Drought Response Actions 
### "Water Shortage Contingency Stage Invoked" column as suppliers coded 1-5
### various ways such as Level 1, Stage 1, Level I, I, Stage 1 - ...
### Some suppliers did not have any value, say None, or have values that do not 
### conform to a 1-5 scale such as Normal, City Conservation, or Alert
### Additionally, 4 suppliers had values duplicate 31 to 63 times which were
### removed during pre-processing for the month of Feb 2022
### Pre-processing for Consumption was to remove duplicate values in some
### suppliers for the month of Feb 2022

### Load the data
library(tidyverse);library(readxl); library(stargazer); library(ggplot2);
library(MASS); library(reshape2); library(reshape); library(sandwich);
library(lmtest); library(plotrix); library(ggrepel); library(forcats);
library(scales); library(tibble); library(geojsonio); library(broom);
library(sp);

actions <- read_xlsx('uw_drought_response020723_DroughtResponseActions.xlsx',
                     sheet = "Machine readable")
consumption <- read_xlsx('uw_supplier_data020723_SupplierMontlhyReports.xlsx',
                      sheet = "Jun14-Dec22 Conservation Data")

#### Prepare the actions data set ####
### Variables of Interest
actions_named <- actions |>
  dplyr::select(c("Reporting Month","Public Water System ID","Hydrologic Region",
           "Climate Zone","Water Shortage Contingency Stage Invoked",
           "DWR Stage","Enacted any measures","Raising rates",
           "Apply drought surcharges","Expanded existing rebate program",
           "Use type restrictions","E-mails","Paper mail", 
           "Notifications via Customer App","Website","Articles/News releases",
           "Youtube","Facebook","Instagram","Other Social Media",
           "Community events","Door hanger","Workshops","Television",
           "Radio","Billboard","Paid Media Advertising","Bus shelter",
           "Assigned a different rate tier","Fine")) 
### Rename Variables
colnames(actions_named) <- c("Date","ID","HydroRegion","ClimateZone",
                             "LocalStage","DWRStage","EnactMeasure",
                             "RaiseRates","Surcharge","Rebate","UseRestrict",
                             "Email","PaperMail","CustApp","Website",
                             "ArticleNews","Youtube","Facebook","Instagram",
                             "OtherSocial","CommunityEvent","DoorHanger",
                             "Workshops","TV","Radio","Billboard","PaidAds",
                             "Bus","Blocks","Fines")
### Check outcome
#head(actions_named)

### Change date to Month/Year
actions_named["MonthYear"] <- format(as.Date(actions_named$Date), "%Y-%m")

### Prepare consumption dataset
### Variables of Interest
consump_named <- consumption |> 
  dplyr::select("Public Water System ID","Reporting Month",
              "Total Population Served", "CALCULATED R-GPCD",
              "CALCULATED Total Potable Water Production Gallons (Ag Excluded)")
### Rename Variables
colnames(consump_named) <- c("ID","Date","Population","RGPCD", "TotalProd")

### Change date to Month/Year
consump_named["MonthYear"] <- format(as.Date(consump_named$Date), "%Y-%m")

### Join Residential per capita use with drought info
merged <- merge(actions_named, consump_named,
      by = c("ID", "MonthYear")) |> dplyr::select(-c("Date.x","Date.y"))

### Check for any duplicates and ensure dimensions are correct
#n_occur <- data.frame(table(merged$MonthID))
#n_occur[n_occur$Freq > 1,]

#### Change the conservation actions to 1 for yes, 0 for NA ####
merged$RaiseRates <- ifelse(merged$RaiseRates == "Y", 1, 0)
merged["RaiseRates"][is.na(merged["RaiseRates"])] <- 0

merged$Surcharge <- ifelse(merged$Surcharge == "Y", 1, 0)
merged["Surcharge"][is.na(merged["Surcharge"])] <- 0

merged$Rebate <- ifelse(merged$Rebate == "Y", 1, 0)
merged["Rebate"][is.na(merged["Rebate"])] <- 0

merged$UseRestrict <- ifelse(merged$UseRestrict == "Y", 1, 0)
merged["UseRestrict"][is.na(merged["UseRestrict"])] <- 0

merged$Email <- ifelse(merged$Email == "Y", 1, 0)
merged["Email"][is.na(merged["Email"])] <- 0

merged$PaperMail <- ifelse(merged$PaperMail == "Y", 1, 0)
merged["PaperMail"][is.na(merged["PaperMail"])] <- 0

merged$CustApp <- ifelse(merged$CustApp == "Y", 1, 0)
merged["CustApp"][is.na(merged["CustApp"])] <- 0

merged$Website <- ifelse(merged$Website == "Y", 1, 0)
merged["Website"][is.na(merged["Website"])] <- 0

merged$ArticleNews <- ifelse(merged$ArticleNews == "Y", 1, 0)
merged["ArticleNews"][is.na(merged["ArticleNews"])] <- 0

merged$Youtube <- ifelse(merged$Youtube == "Y", 1, 0)
merged["Youtube"][is.na(merged["Youtube"])] <- 0

merged$Facebook <- ifelse(merged$Facebook == "Y", 1, 0)
merged["Facebook"][is.na(merged["Facebook"])] <- 0

merged$Instagram <- ifelse(merged$Instagram == "Y", 1, 0)
merged["Instagram"][is.na(merged["Instagram"])] <- 0

merged$OtherSocial <- ifelse(merged$OtherSocial == "Y", 1, 0)
merged["OtherSocial"][is.na(merged["OtherSocial"])] <- 0

merged$CommunityEvent <- ifelse(merged$CommunityEvent == "Y", 1, 0)
merged["CommunityEvent"][is.na(merged["CommunityEvent"])] <- 0

merged$DoorHanger <- ifelse(merged$DoorHanger == "Y", 1, 0)
merged["DoorHanger"][is.na(merged["DoorHanger"])] <- 0

merged$Workshops <- ifelse(merged$Workshops == "Y", 1, 0)
merged["Workshops"][is.na(merged["Workshops"])] <- 0

merged$TV <- ifelse(merged$TV == "Y", 1, 0)
merged["TV"][is.na(merged["TV"])] <- 0

merged$Radio <- ifelse(merged$Radio == "Y", 1, 0)
merged["Radio"][is.na(merged["Radio"])] <- 0

merged$Billboard <- ifelse(merged$Billboard == "Y", 1, 0)
merged["Billboard"][is.na(merged["Billboard"])] <- 0

merged$PaidAds <- ifelse(merged$PaidAds == "Y", 1, 0)
merged["PaidAds"][is.na(merged["PaidAds"])] <- 0

merged$Bus <- ifelse(merged$Bus == "Y", 1, 0)
merged["Bus"][is.na(merged["Bus"])] <- 0

merged$Blocks <- ifelse(merged$Blocks == "Y", 1, 0)
merged["Blocks"][is.na(merged["Blocks"])] <- 0

merged$Fines <- ifelse(merged$Fines == "Y", 1, 0)
merged["Fines"][is.na(merged["Fines"])] <- 0

#### Roll up conservation categories ####
merged <- merged |> 
  mutate(DirectMsg = rowSums(across(c(Email, PaperMail, CustApp, Website))))

merged <- merged |> 
  mutate(eAds = rowSums(across(c(Youtube, Facebook, Instagram,
                                 OtherSocial, TV, Radio, ArticleNews))))

merged <- merged |> 
  mutate(PhysicalAds = rowSums(across(c(CommunityEvent, DoorHanger, Workshops,
                                 Billboard, PaidAds, Bus))))

### Month only as well
merged["Month"] <- substr(merged$MonthYear, 6, 7)
#merged

merged_stage <- merged[merged$LocalStage %in% c("0", "1", "2", "3", "4", "5"),]
merged_stage$LocalStage <- as.integer(merged_stage$LocalStage)


###Subset to select columns
data_subset <- merged_stage |> 
  dplyr::select(ID, MonthYear, Month, HydroRegion, ClimateZone, LocalStage,
                EnactMeasure, RaiseRates, Surcharge, Rebate, UseRestrict,
                Blocks, Fines, DirectMsg, eAds, PhysicalAds, RGPCD)

head(data_subset)

#### Log Linear Models ####
#Model 1
model1 <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + #LocalStage +
               ID, data = data_subset)
#Robust SE for Model 1
model1$rse <- sqrt(diag(vcovHC(model1, type ='HC1')))

#Model 2
model2 <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds +
               Month + ID, data = data_subset)

#Robust SE for Model 2
model2$rse <- sqrt(diag(vcovHC(model2, type ='HC1')))


#### Log Linear Overall Stargazer model ####
stargazer(model1, model2, type = "html", title = "Table I",
      column.labels = c("(1)", "(2)"),
      model.numbers = F,
      dep.var.caption  = "Dependent Variable:",
      dep.var.labels = "Log Transformed RGPCD",
      single.row = TRUE, omit = c("Month", "ID"),
      se = list(model1$rse, model2$rse),
      notes = c("Standard errors are robust standard errors",
      "RGPCD is water use measured as residential gallons per capita per day",
      "Data Source: California State Water Resources Control Board"),
      notes.append = TRUE,
      df = FALSE,
      star.cutoffs = c(0.05, 0.01, 0.001),
      add.lines = list(c("Month Control", "No", "Yes"),
                      c("Supplier Control", "Yes", "Yes")))

#### Log Linear All HydroRegion Stargazer model ####
#Model 3
model3 <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + HydroRegion +
               Month + ID, data = data_subset)

#Robust SE for Model 3
model3$rse <- sqrt(diag(vcovHC(model3, type ='HC1')))

#Model 4
model4 <- lm(RGPCD ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + HydroRegion +
               Month + ID, data = data_subset)

#Robust SE for Model 3
model4$rse <- sqrt(diag(vcovHC(model4, type ='HC1')))

stargazer(model3, model4, type = "text", title = "Table II",
      column.labels = c("(3)", "(4)"),
      model.numbers = F,
      dep.var.caption  = "Dependent Variable:",
      dep.var.labels = c("Log Transformed RGCPD", "RGPCD"),
      single.row = TRUE, omit = c("Month", "ID"),
      se = list(model3$rse, model4$rse),
      notes = c("Standard errors are robust standard errors",
      "RGPCD is water use measured as residential gallons per capita per day",
      "Data Source: California State Water Resources Control Board"),
      notes.append = TRUE,
      df = FALSE,
      star.cutoffs = c(0.05, 0.01, 0.001),
      add.lines = list(c("Month Control", "Yes", "Yes"),
                       c("Supplier Control", "Yes", "Yes")))

#### Log Linear Each HydroRegion Stargazer model ####
####Model Central Coast - Non Metro
modelCC <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + 
               Month + ID, data = data_subset |>
                filter(HydroRegion == "Central Coast"))

#Robust SE for #Model Central Coast
modelCC$rse <- sqrt(diag(vcovHC(modelCC, type ='HC1')))

####Model Colorado River - Non Metro
modelCR <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + 
               Month + ID, data = data_subset |>
                filter(HydroRegion == "Colorado River"))

#Robust SE for Colorado River
modelCR$rse <- sqrt(diag(vcovHC(modelCR, type ='HC1')))

####Model North Coast - Non Metro
modelNC <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + 
               Month + ID, data = data_subset |>
                filter(HydroRegion == "North Coast"))

#Robust SE for North Coast
modelNC$rse <- sqrt(diag(vcovHC(modelNC, type ='HC1')))

####Model North Lahontan - Non Metro
modelNL <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds +
               Month + ID, data = data_subset |>
                filter(HydroRegion == "North Lahontan"))

#Robust SE for North Lahontan
modelNL$rse <- sqrt(diag(vcovHC(modelNL, type ='HC1')))

####Model Sacramento River - Metro
modelSR <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + 
               Month + ID, data = data_subset |>
                filter(HydroRegion == "Sacramento River"))

#Robust SE for Sacramento River
modelSR$rse <- sqrt(diag(vcovHC(modelSR, type ='HC1')))

####Model San Francisco Bay - Metro
modelSFB <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + 
               Month + ID, data = data_subset |>
                 filter(HydroRegion == "San Francisco Bay"))

#Robust SE for San Francisco Bay
modelSFB$rse <- sqrt(diag(vcovHC(modelSFB, type ='HC1')))

####Model San Joaquin River - Metro
modelSJR <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds + 
               Month + ID, data = data_subset |>
                 filter(HydroRegion == "San Joaquin River"))

#Robust SE for San Joaquin River
modelSJR$rse <- sqrt(diag(vcovHC(modelSJR, type ='HC1')))

####Model South Coast - Metro
modelSC <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds +
               Month + ID, data = data_subset |>
                filter(HydroRegion == "South Coast"))

#Robust SE for South Coast
modelSC$rse <- sqrt(diag(vcovHC(modelSC, type ='HC1')))

####Model South Lahontan - Non Metro
modelSL <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
               Rebate + UseRestrict + Blocks + Fines + DirectMsg +
               eAds + PhysicalAds +
               Month + ID, data = data_subset |>
                filter(HydroRegion == "South Lahontan"))

####Robust SE for South Lahontan
modelSL$rse <- sqrt(diag(vcovHC(modelSL, type ='HC1')))

####Model Tulare Lake - Metro
modelTL <- lm(log(RGPCD) ~ RaiseRates + Surcharge +
                Rebate + UseRestrict + Blocks + Fines + DirectMsg +
                eAds + PhysicalAds +
                Month + ID, data = data_subset |>
                filter(HydroRegion == "Tulare Lake"))

####Robust SE for Tulare Lake
modelTL$rse <- sqrt(diag(vcovHC(modelTL, type ='HC1')))

####Metros
stargazer(modelSC, modelSFB, modelSJR, modelSR, modelTL,
    type = "text", title = "Table III",
    model.numbers = T,
    dep.var.caption  = "Dependent Variable:",
    dep.var.labels = "Log Transformed RGPCD",
    single.row = TRUE, omit = c("Month", "ID"),
    se = list(modelSC$rse, modelSFB$rse, modelSJR$rse, modelSR$rse, modelTL$rse),
    column.labels = c("South Coast", "San Francisco Bay", "San Joaquin River",
                      "Sacramento River", "Tulare Lake"),
    notes = c("Standard errors are robust standard errors",
    "RGPCD is water use measured as residential gallons per capita per day",
    "Data Source: California State Water Resources Control Board"),
    df = FALSE,
    notes.append = TRUE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    add.lines = list(c("Month Control", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("Supplier Control", "Yes", "Yes", "Yes", "Yes", "Yes")))


####NonMetros
stargazer(modelCC, modelCR, modelNC, modelNL, modelSL,
    model.numbers = F,
    type = "text", title = "Table IV",
    dep.var.caption  = "Dependent Variable:",
    dep.var.labels = "Log Transformed RGPCD",
    single.row = TRUE, omit = c("Month", "ID"),
    se = list(modelCC$rse, modelCR$rse, modelNC$rse, modelNL$rse ,modelSL$rse),
    column.labels = c("Central Coast", "Colorado River", "North Coast", 
                      "North Lahontan", "South Lahontan"),
    notes = c("Standard errors are robust standard errors",
    "RGPCD is water use measured as residential gallons per capita per day",
    "Data Source: California State Water Resources Control Board"),
    notes.append = TRUE,
    df = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    add.lines = list(c("Month Control", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("Supplier Control", "Yes", "Yes", "Yes", "Yes", "Yes")))

######### Visuals Data Prep #########
### Create helper counter to get number of Supplier Months per category
data_subset["counter"] <- 1

hydroCounts <- table(data_subset$HydroRegion, data_subset$counter)
hydroCountsDF <- as.data.frame.matrix(hydroCounts)
colnames(hydroCountsDF)[1] = "SupplierMonths"

#### E-Ads Proportions Data Frame ####
eadsTable <- table(data_subset$HydroRegion, data_subset$eAds)
eadsDF <- as.data.frame.matrix(eadsTable)
eadspropDF <- cbind(eadsDF, hydroCountsDF)
eadspropDF$"0" <- eadspropDF$"0"/eadspropDF$SupplierMonths
eadspropDF$"1" <- eadspropDF$"1"/eadspropDF$SupplierMonths
eadspropDF$"2" <- eadspropDF$"2"/eadspropDF$SupplierMonths
eadspropDF$"3" <- eadspropDF$"3"/eadspropDF$SupplierMonths
eadspropDF$"4" <- eadspropDF$"4"/eadspropDF$SupplierMonths
eadspropDF$"5" <- eadspropDF$"5"/eadspropDF$SupplierMonths
eadspropDF$"6" <- eadspropDF$"6"/eadspropDF$SupplierMonths
eadspropDF$"7" <- eadspropDF$"7"/eadspropDF$SupplierMonths
eadspropDF <- subset(eadspropDF, select = -c(SupplierMonths))
eadspropDF <- eadspropDF |> mutate_if(is.numeric, round, digits = 2)
eadspropDF

#### Physical Ads Proportions Data Frame ####
physAdsTable <- table(data_subset$HydroRegion, data_subset$PhysicalAds)
physDF <- as.data.frame.matrix(physAdsTable)
physDF <- cbind(physDF, hydroCountsDF)
physDF$"0" <- physDF$"0"/physDF$SupplierMonths
physDF$"1" <- physDF$"1"/physDF$SupplierMonths
physDF$"2" <- physDF$"2"/physDF$SupplierMonths
physDF$"3" <- physDF$"3"/physDF$SupplierMonths
physDF$"4" <- physDF$"4"/physDF$SupplierMonths
physDF$"5" <- physDF$"5"/physDF$SupplierMonths
physDF$"6" <- physDF$"6"/physDF$SupplierMonths
physDF <- subset(physDF, select = -c(SupplierMonths))
physDF <- physDF |> mutate_if(is.numeric, round, digits = 2)
physDF

#### Direct Messaging Proportions Data Frame ####
directmsgTable <- table(data_subset$HydroRegion, data_subset$DirectMsg)
directDF <- as.data.frame.matrix(directmsgTable)
directDF <- cbind(directDF, hydroCountsDF)
directDF$"0" <- directDF$"0"/directDF$SupplierMonths
directDF$"1" <- directDF$"1"/directDF$SupplierMonths
directDF$"2" <- directDF$"2"/directDF$SupplierMonths
directDF$"3" <- directDF$"3"/directDF$SupplierMonths
directDF$"4" <- directDF$"4"/directDF$SupplierMonths
directDF <- subset(directDF, select = -c(SupplierMonths))
directDF <- directDF |> mutate_if(is.numeric, round, digits = 2)
directDF

########## Bar Plots ########### 
# Bar plot of Electronic Ads
eadspropDF["HydroRegion"] <- rownames(eadspropDF)
eadspropDFmelt <- melt(eadspropDF, na.rm = FALSE,
                       value.name = "Proportions",
                        id = 'HydroRegion')
colnames(eadspropDFmelt) <- c("Hydrologic Region",
                              "Number of Categories",
                              "Proportion of Months")
ggplot(eadspropDFmelt, aes(x = `Hydrologic Region`,
                           y = `Proportion of Months`,
                           fill = `Number of Categories`)) +
  geom_bar(position = position_dodge(),
           stat = "identity")  +
  coord_flip() + 
  labs(fill='Number of
Categories') +
  ggtitle("Figure II: Proportion of Months and Number of Electronic Ad 
          Categories Used by Hydrologic Region")

# Bar plot of Physical Ads
physDF["HydroRegion"] <- rownames(physDF)
physDFmelt <- melt(physDF, na.rm = FALSE,
                       value.name = "Proportions",
                       id = 'HydroRegion')
colnames(physDFmelt) <- c("Hydrologic Region",
                              "Number of Categories",
                              "Proportion of Months")
ggplot(physDFmelt, aes(x = `Hydrologic Region`,
                           y = `Proportion of Months`,
                           fill = `Number of Categories`)) +
  geom_bar(position = position_dodge(),
           stat = "identity")  +
  coord_flip() + 
  labs(fill='Number of
Categories') +
  ggtitle("Figure III: Proportion of Months and Number of Physical Ad 
          Categories Used by Hydrologic Region")

# Bar plot of Direct Messaging
directDF["HydroRegion"] <- rownames(directDF)
directDFmelt <- melt(physDF, na.rm = FALSE,
                   value.name = "Proportions",
                   id = 'HydroRegion')
colnames(directDFmelt) <- c("Hydrologic Region",
                          "Number of Categories",
                          "Proportion of Months")
ggplot(directDFmelt, aes(x = `Hydrologic Region`,
                       y = `Proportion of Months`,
                       fill = `Number of Categories`)) +
  geom_bar(position = position_dodge(),
           stat = "identity")  +
  coord_flip() + 
  labs(fill='Number of
Categories') +
  ggtitle("Figure IV: Proportion of Months and Number of Direct Messaging Categories Used
by Hydrologic Region")

#### Pie Chart by HydroRegion ####

prodByHydro <- merged |> 
  group_by(HydroRegion) |> 
  summarize(HydroProdMM = sum(TotalProd)/1000000) |> 
  arrange(desc(HydroProdMM))

# Pie Chart with Percentages
slices <- prodByHydro$HydroProdMM
lbls <- prodByHydro$HydroRegion
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls, ",") # add percents to labels
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie(slices,labels = lbls, col=topo.colors(length(lbls)),
    main="Figure I: Total Water Use by Hydrologic Region - Residential and Commercial",
    radius = 1, cex = 0.8,
    init.angle = 150)

########## HydroRegions Map ########### 
spdf <- geojson_read("i03_Hydrologic_Regions.geojson",  what = "sp")
#plot(spdf)

#GGPlot
spdf_tidy <- broom::tidy(spdf)
hydrocoords <- data.frame(lat = c(41.7021, #NC
                                  39.8285, #SR
                                  40.5563, #NL
                                  37.4393, #SQR
                                  35.2828, #CC
                                  37.9749, #SFB
                                  36.3302, #TL
                                  35.2225, #SL
                                  34.0522, #SC
                                  34.1347), #CR
                          long = c(-123.0637, #NC
                                   -121.4375, #SR
                                   -119.8530, #NL
                                   -120.1970, #SQR
                                   -120.8596, #CC
                                   -123.3194, #SFB
                                   -119.2921, #TL
                                   -116.9709, #SL
                                   -118.2437, #SC
                                   -115.5131), #CR
                          region = c("North Coast",
                                     "Sacramento River",
                                     "North Lahontan",
                                     "San Joaquin River",
                                     "Central Coast",
                                     "San Francisco Bay",
                                     "Tulare Lake",
                                     "South Lahontan",
                                     "South Coast",
                                     "Colorado River"))

ggplot()+
  geom_polygon(data = spdf_tidy, aes(x = long,
                                     y = lat,
                                     group = group),
               fill = "darkgrey",
               color = "white") +
  theme_void() +
  coord_map() +
  geom_text(aes(label = hydrocoords$region,
                x = hydrocoords$long, y = hydrocoords$lat)) +
  labs(title = "Figure V: California Hydrologic Regions",
       caption = "Data source: California State Geoportal")

#### Number of Months in each Local Stage
# stage_table <- table(data_subset$LocalStage, data_subset$counter)
# stagedf <- as.data.frame.matrix(stage_table)
# stagedf <- cbind(stagedf, c("0", "1", "2", "3", "4", "5"))
# colnames(stagedf) <- c("Count","Stage")
# stagedf$prop <- stagedf$Count/sum(stagedf$Count)
# stagedf <- stagedf |> mutate_if(is.numeric, round, digits = 2)
# stagedf
# 
# ggplot(stagedf, aes(x = Stage,
#                          y = Count)) +
#   geom_bar(position = position_dodge(),
#            stat = "identity")  +
#   xlab("Drought Stage") +
#   theme_bw() +
#   ggtitle("Figure #: Distribution of Drought Stages in the Data Set") 


#### Visual of top 3 intervention efforts and impacts by region  ####
Region <- c("South Coast", "South Coast", "South Coast",
            "San Francisco Bay", "San Francisco Bay","San Francisco Bay",
            "San Joaquin River", "San Joaquin River","San Joaquin River",
            "Sacramento River", "Sacramento River", "Sacramento River",
            "Tulare Lake", "Tulare Lake", "Tulare Lake",
            "Central Coast", "Central Coast", "Central Coast",
            "Colorado River", "Colorado River", "Colorado River",
            "North Coast", "North Coast", "North Coast",
            "North Lahontan", "North Lahontan", "North Lahontan",
            "South Lahontan", "South Lahontan", "South Lahontan")
Response <- c(-4.4, 13.3, -1.1,
            -2, 0, 0,
            -6.7, -2.4, 0,
            0, 0, 0,
            9.7, 0, 0,
            -11.5, -8.5, 0,
            -4.8, 4.2, 1.8,
            0, 0, 0,
            0, 0, 0,
            -2.7, 0, 0)
Action <- c("Use Restriction", "Blocks", "Direct Messaging",
            "Direct Messaging", NA, NA,
            "Use Restriction", "Physical Advertising", NA,
            NA, NA, NA,
            "Use Restriction", NA, NA,
            "Surcharge", "Fines", NA,
            "Direct Messaging", "Fines", "Electronic Advertising",
            NA, NA, NA,
            NA, NA, NA,
            "Physical Advertising", NA, NA)

effectiveness_df <- data.frame(Region, Response, Action)


ggplot(effectiveness_df, aes(x = Response,
                             y = Region,
                             fill = Action)) +
  geom_bar(position = position_dodge(),
           stat = "identity") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme_bw() + 
  scale_fill_manual( breaks = c("Blocks",
                                "Direct Messaging",
                                "Electronic Advertising",
                                "Fines",
                                "Physical Advertising",
                                "Surcharge",
                                "Use Restriction"),
                     values = scales::hue_pal()(7) ) +
  labs(title = 
"Figure IV: Top Three Most Impactful Conservation Actions by Hydrologic Region",
       caption = 
"Note: Some regions had less than three significant conservation actions
Negative percentages indicate conservation, positive indicate additional use
Data source: California State Water Resources Control Board")

# #### Actions found to increase water use ####
# 
# Region2 <- c("South Coast", "South Coast", "South Coast",
#             "San Francisco Bay", "San Francisco Bay","San Francisco Bay",
#             "San Joaquin River", "San Joaquin River","San Joaquin River",
#             "Sacramento River", "Sacramento River", "Sacramento River",
#             "Tulare Lake", "Tulare Lake", "Tulare Lake",
#             "Central Coast", "Central Coast", "Central Coast",
#             "Colorado River", "Colorado River", "Colorado River",
#             "North Coast", "North Coast", "North Coast",
#             "North Lahontan", "North Lahontan", "North Lahontan",
#             "South Lahontan", "South Lahontan", "South Lahontan")
# Response2 <- c(0, 13.3, 0,
#               0, 0, 0,
#               0, 0, 0,
#               0, 0, 0,
#               9.7, 0, 0,
#               0, 0, 0,
#               0, 4.2, 1.8,
#               0, 0, 0,
#               0, 0, 0,
#               0, 0, 0)
# Action2 <- c(NA, "Blocks", NA,
#             NA, NA, NA,
#             NA, NA, NA,
#             NA, NA, NA,
#             "Use Restriction", NA, NA,
#             NA, NA, NA,
#             NA, "Fines", "Electronic Advertising",
#             NA, NA, NA,
#             NA, NA, NA,
#             NA, NA, NA)
# 
# 
# effectiveness2_df <- data.frame(Region, Response, Action)
# 
# ggplot(effectiveness2_df, aes(x = Response2,
#                              y = Region2,
#                              fill = Action2)) +
#   geom_bar(position = position_dodge(),
#            stat = "identity") +
#   scale_x_continuous(labels = scales::percent_format(scale = 1)) +
#   theme_bw() + 
#   labs(title = 
# "Actions found to Increase Water Use by Hydrologic Region",
#        caption = 
# "Note: Some regions had less than three significant conservation actions
# Negative percentages indicate conservation, positive indicate additional use
# Data source: California State Water Resources Control Board")
# 
# 
# #### Actions that decrease water use and region  ####
# Region3 <- c("South Coast", "South Coast", "South Coast",
#             "San Francisco Bay", "San Francisco Bay","San Francisco Bay",
#             "San Joaquin River", "San Joaquin River","San Joaquin River",
#             "Sacramento River", "Sacramento River", "Sacramento River",
#             "Tulare Lake", "Tulare Lake", "Tulare Lake",
#             "Central Coast", "Central Coast", "Central Coast",
#             "Colorado River", "Colorado River", "Colorado River",
#             "North Coast", "North Coast", "North Coast",
#             "North Lahontan", "North Lahontan", "North Lahontan",
#             "South Lahontan", "South Lahontan", "South Lahontan")
# Response3 <- c(-4.4, 0, -1.1,
#               -2, 0, 0,
#               -6.7, -2.4, 0,
#               0, 0, 0,
#               0, 0, 0,
#               -11.5, -8.5, 0,
#               -4.8, 0, 0,
#               0, 0, 0,
#               0, 0, 0,
#               -2.7, 0, 0)
# Action3 <- c("Use Restriction", NA, "Direct Messaging",
#             "Direct Messaging", NA, NA,
#             "Use Restriction", "Physical Advertising", NA,
#             NA, NA, NA,
#             NA, NA, NA,
#             "Surcharge", "Fines", NA,
#             "Direct Messaging", NA, NA,
#             NA, NA, NA,
#             NA, NA, NA,
#             "Physical Advertising", NA, NA)
# 
# effectiveness3_df <- data.frame(Region3, Response3, Action3)
# 
# ggplot(effectiveness2_df, aes(x = Response3,
#                               y = Region3,
#                               fill = Action3)) +
#   geom_bar(position = position_dodge(),
#            stat = "identity") +
#   scale_x_continuous(labels = scales::percent_format(scale = 1)) +
#   theme_bw() + 
#   labs(title = 
#          "Actions found to Decrease Water Use by Hydrologic Region",
#        caption = 
#          "Note: Some regions had less than three significant conservation actions
# Negative percentages indicate conservation, positive indicate additional use
# Data source: California State Water Resources Control Board")

#### Count of suppliers for Abstract ####
merged |> 
  filter(MonthYear == "2022-12") |> 
  dplyr::select(ID, MonthYear) |> 
  summarize(n = n())
  
