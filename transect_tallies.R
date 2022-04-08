#make a dataframe that provides total counts from transects by pivoting data

#open required libraries
library(dplyr)
library(ggplot2)
library(pivottabler)

#read in csv file(s) for HONOKOHAU
hono <- read.csv("honokohau_3.csv")

#convert all data columns to factors
hono$Site <- as.factor(as.character(hono$Site))
hono$Transect.Number <- as.factor(hono$Transect.Number)
hono$Date <- as.factor(hono$Date)
hono$Time <- as.factor(hono$Time)
hono$Meter <- as.factor(as.character(hono$Meter))
hono$Point <- as.factor(as.character(hono$Point))
hono$Substrate <- as.factor(hono$Substrate)
hono$Living.cover <- as.factor(hono$Living.cover)
hono$Genus <- as.factor(hono$Genus)
hono$Species <- as.factor(hono$Species)

#make a column that puts Genus and species together
hono$Genus_species <- as.factor(paste(hono$Genus, hono$Species, sep = "_"))

#make a column that puts site and transect number together
hono$site_transect <- as.factor(paste(hono$Site, hono$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(hono)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(hono)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(hono)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(hono)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

# Site 4 QLC _____________________________________________________________________________

#read in csv file(s) for QLC
qlc <- read.csv("qlc_4.csv")

#convert all data columns to factors
qlc$Site <- as.factor(as.character(qlc$Site))
qlc$Transect.Number <- as.factor(qlc$Transect.Number)
qlc$Date <- as.factor(qlc$Date)
qlc$Time <- as.factor(qlc$Time)
qlc$Meter <- as.factor(as.character(qlc$Meter))
qlc$Point <- as.factor(as.character(qlc$Point))
qlc$Substrate <- as.factor(qlc$Substrate)
qlc$Living.cover <- as.factor(qlc$Living.cover)
qlc$Genus <- as.factor(qlc$Genus)
qlc$Species <- as.factor(qlc$Species)

#make a column that puts Genus and species together
qlc$Genus_species <- as.factor(paste(qlc$Genus, qlc$Species, sep = "_"))

#make a column that puts site and transect number together
qlc$site_transect <- as.factor(paste(qlc$Site, qlc$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(qlc)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(qlc)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(qlc)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(qlc)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

# Site 0 Mahaiula _____________________________________________________________________________

#read in csv file(s) for QLC
maha0 <- read.csv("mahaiula_0.csv")

#convert all data columns to factors
maha0$Site <- as.factor(as.character(maha0$Site))
maha0$Transect.Number <- as.factor(maha0$Transect.Number)
maha0$Date <- as.factor(maha0$Date)
maha0$Time <- as.factor(maha0$Time)
maha0$Meter <- as.factor(as.character(maha0$Meter))
maha0$Point <- as.factor(as.character(maha0$Point))
maha0$Substrate <- as.factor(maha0$Substrate)
maha0$Living.cover <- as.factor(maha0$Living.cover)
maha0$Genus <- as.factor(maha0$Genus)
maha0$Species <- as.factor(maha0$Species)

#make a column that puts Genus and species together
maha0$Genus_species <- as.factor(paste(maha0$Genus, maha0$Species, sep = "_"))

#make a column that puts site and transect number together
maha0$site_transect <- as.factor(paste(maha0$Site, maha0$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(maha0)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(maha0)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(maha0)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(maha0)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

# Site 1 Mahaiula _____________________________________________________________________________

#read in csv file(s) for QLC
maha1 <- read.csv("mahaiula_1.csv")

#convert all data columns to factors
maha1$Site <- as.factor(as.character(maha1$Site))
maha1$Transect.Number <- as.factor(maha1$Transect.Number)
maha1$Date <- as.factor(maha1$Date)
maha1$Time <- as.factor(maha1$Time)
maha1$Meter <- as.factor(as.character(maha1$Meter))
maha1$Point <- as.factor(as.character(maha1$Point))
maha1$Substrate <- as.factor(maha1$Substrate)
maha1$Living.cover <- as.factor(maha1$Living.cover)
maha1$Genus <- as.factor(maha1$Genus)
maha1$Species <- as.factor(maha1$Species)

#make a column that puts Genus and species together
maha1$Genus_species <- as.factor(paste(maha1$Genus, maha1$Species, sep = "_"))

#make a column that puts site and transect number together
maha1$site_transect <- as.factor(paste(maha1$Site, maha1$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(maha1)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(maha1)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(maha1)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(maha1)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

# Site 5 Kona Town _____________________________________________________________________________

#read in csv file(s) for Kona Town
town <- read.csv("konatown_5.csv")

#convert all data columns to factors
town$Site <- as.factor(as.character(town$Site))
town$Transect.Number <- as.factor(town$Transect.Number)
town$Date <- as.factor(town$Date)
town$Time <- as.factor(town$Time)
town$Meter <- as.factor(as.character(town$Meter))
town$Point <- as.factor(as.character(town$Point))
town$Substrate <- as.factor(town$Substrate)
town$Living.cover <- as.factor(town$Living.cover)
town$Genus <- as.factor(town$Genus)
town$Species <- as.factor(town$Species)

#make a column that puts Genus and species together
town$Genus_species <- as.factor(paste(town$Genus, town$Species, sep = "_"))

#make a column that puts site and transect number together
town$site_transect <- as.factor(paste(town$Site, town$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(town)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(town)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(town)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(town)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

# ___________Site 2 NELHA___________
# Read in csv file(s) for NELHA
nelha <- read.csv("nelha_2.csv")

#convert all data columns to factors
nelha$Site <- as.factor(as.character(nelha$Site))
nelha$Transect.Number <- as.factor(nelha$Transect.Number)
nelha$Date <- as.factor(nelha$Date)
nelha$Time <- as.factor(nelha$Time)
nelha$Meter <- as.factor(as.character(nelha$Meter))
nelha$Point <- as.factor(as.character(nelha$Point))
nelha$Substrate <- as.factor(nelha$Substrate)
nelha$Living.cover <- as.factor(nelha$Living.cover)
nelha$Genus <- as.factor(nelha$Genus)
nelha$Species <- as.factor(nelha$Species)

#make a column that puts Genus and species together
nelha$Genus_species <- as.factor(paste(nelha$Genus, nelha$Species, sep = "_"))

#make a column that puts site and transect number together
nelha$site_transect <- as.factor(paste(nelha$Site, nelha$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(nelha)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(nelha)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(nelha)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(nelha)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

# ___________Site 7 Lymans___________
# Read in csv file(s) for Lymans
lymans <- read.csv("lymans_7.csv")

#convert all data columns to factors
lymans$Site <- as.factor(as.character(lymans$Site))
lymans$Transect.Number <- as.factor(lymans$Transect.Number)
lymans$Date <- as.factor(lymans$Date)
lymans$Time <- as.factor(lymans$Time)
lymans$Meter <- as.factor(as.character(lymans$Meter))
lymans$Point <- as.factor(as.character(lymans$Point))
lymans$Substrate <- as.factor(lymans$Substrate)
lymans$Living.cover <- as.factor(lymans$Living.cover)
lymans$Genus <- as.factor(lymans$Genus)
lymans$Species <- as.factor(lymans$Species)

#make a column that puts Genus and species together
lymans$Genus_species <- as.factor(paste(lymans$Genus, lymans$Species, sep = "_"))

#make a column that puts site and transect number together
lymans$site_transect <- as.factor(paste(lymans$Site, lymans$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(lymans)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(lymans)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(lymans)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(lymans)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

# ___________Site 8 Kahalu‘u___________
# Read in csv file(s) for Kahalu‘u
kaha <- read.csv("kahaluu_8.csv")

#convert all data columns to factors
kaha$Site <- as.factor(as.character(kaha$Site))
kaha$Transect.Number <- as.factor(kaha$Transect.Number)
kaha$Date <- as.factor(kaha$Date)
kaha$Time <- as.factor(kaha$Time)
kaha$Meter <- as.factor(as.character(kaha$Meter))
kaha$Point <- as.factor(as.character(kaha$Point))
kaha$Substrate <- as.factor(kaha$Substrate)
kaha$Living.cover <- as.factor(kaha$Living.cover)
kaha$Genus <- as.factor(kaha$Genus)
kaha$Species <- as.factor(kaha$Species)

#make a column that puts Genus and species together
kaha$Genus_species <- as.factor(paste(kaha$Genus, kaha$Species, sep = "_"))

#make a column that puts site and transect number together
kaha$site_transect <- as.factor(paste(kaha$Site, kaha$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(kaha)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(kaha)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(kaha)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(kaha)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

# ___________Site 9 Keauhou___________
# Read in csv file(s) for Keauhou
keau <- read.csv("keauhou_9.csv")

#convert all data columns to factors
keau$Site <- as.factor(as.character(keau$Site))
keau$Transect.Number <- as.factor(keau$Transect.Number)
keau$Date <- as.factor(keau$Date)
keau$Time <- as.factor(keau$Time)
keau$Meter <- as.factor(as.character(keau$Meter))
keau$Point <- as.factor(as.character(keau$Point))
keau$Substrate <- as.factor(keau$Substrate)
keau$Living.cover <- as.factor(keau$Living.cover)
keau$Genus <- as.factor(keau$Genus)
keau$Species <- as.factor(keau$Species)

#make a column that puts Genus and species together
keau$Genus_species <- as.factor(paste(keau$Genus, keau$Species, sep = "_"))

#make a column that puts site and transect number together
keau$site_transect <- as.factor(paste(keau$Site, keau$Transect.Number, sep = "_"))

#pivot data to provide total counts for each row (transect) and column (Genus_species)
pt <- PivotTable$new()
pt$addData(keau)
pt$addColumnDataGroups("Genus_species")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Genus)
# This will provide totals for Ulva (all)
pt <- PivotTable$new()
pt$addData(keau)
pt$addColumnDataGroups("Genus")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Substrate)
pt <- PivotTable$new()
pt$addData(keau)
pt$addColumnDataGroups("Substrate")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()

#pivot data to provide total counts for each row (transect) and column (Live cover)
pt <- PivotTable$new()
pt$addData(keau)
pt$addColumnDataGroups("Living.cover")
pt$addRowDataGroups("site_transect")
pt$defineCalculation(calculationName = "site_transectCount", summariseExpression = "n()",
                     caption = "Count", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="site_transect")
pt$defineCalculation(calculationName="site_transectTotal", filters=filterOverrides, 
                     summariseExpression="n()", caption="site_transect Total", visible=FALSE)
pt$defineCalculation(calculationName="PercentageOfsite_transect", type="calculation", 
                     basedOn=c("site_transectCount", "site_transectTotal"),
                     calculationExpression="values$site_transectCount/values$site_transectTotal*100", 
                     format="%.1f %%", caption="% Total")
pt$renderPivot()