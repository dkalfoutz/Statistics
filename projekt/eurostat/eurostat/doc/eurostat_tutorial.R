## ----setup, include=FALSE------------------------------------------------
# Global options
library(knitr)
opts_chunk$set(fig.path="fig/")

## ----install, eval=FALSE-------------------------------------------------
#  install.packages("eurostat")

## ----install2, eval=FALSE------------------------------------------------
#  library(devtools)
#  install_github("ropengov/eurostat")

## ---- echo=FALSE---------------------------------------------------------
library(eurostat)

## ---- echo=FALSE,comment=NA----------------------------------------------
cat(paste0(library(help = "eurostat")$info[[2]], collapse = "\n"))

## ----get_eurostat_toc, warning=FALSE, message=FALSE----------------------
# Load the package
library(eurostat)
library(rvest)

# Get Eurostat data listing
toc <- get_eurostat_toc()

# Check the first items
library(knitr)
kable(head(toc))

## ----search_eurostat, warning=FALSE, message=FALSE-----------------------
# info about passengers
kable(head(search_eurostat("passenger transport")))

## ----get_id, warning=FALSE, message=FALSE, results='asis'----------------
# For the original data, see
# http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&plugin=1&language=en&pcode=tsdtr210
id <- search_eurostat("Modal split of passenger transport", 
        	             type = "table")$code[1]
print(id)

## ----get_eurostat, warning=FALSE, message=FALSE, results='asis'----------
dat <- get_eurostat(id, time_format = "num")

## ----str_dat, warning=FALSE, message=FALSE-------------------------------
str(dat)

## ----head_dat, warning=FALSE, message=FALSE, results='asis'--------------
kable(head(dat))

## ----get_eurostat_json, warning=FALSE, message=FALSE, results='asis', eval=FALSE----
#  dat2 <- get_eurostat(id, filters = list(geo = c("EU28", "FI"), lastTimePeriod=1), time_format = "num")
#  kable(dat2)

## ----json_labels, warning=FALSE, message=FALSE, results='asis', eval=FALSE----
#  datl2 <- get_eurostat(id, filters = list(geo = c("EU28", "FI"),
#                                           lastTimePeriod = 1),
#                        type = "label", time_format = "num")
#  kable(head(datl2))

## ----labels, warning=FALSE, message=FALSE, results='asis'----------------
datl <- label_eurostat(dat)
kable(head(datl))

## ----name_labels, eval = FALSE-------------------------------------------
#  label_eurostat_vars(names(datl))

## ----vehicle_levels, eval = FALSE----------------------------------------
#  levels(datl$vehicle)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
data(efta_countries)
kable(efta_countries)

## ----eu_12---------------------------------------------------------------
dat_eu12 <- subset(datl, geo == "European Union (28 countries)" & time == 2012)
kable(dat_eu12, row.names = FALSE)

## ----eu_vehicles_table---------------------------------------------------
library("tidyr")
dat_eu_0012 <- subset(dat, geo == "EU28" & time %in% 2000:2012)
dat_eu_0012_wide <- spread(dat_eu_0012, vehicle, values)
kable(subset(dat_eu_0012_wide, select = -geo), row.names = FALSE)

## ----trains_table--------------------------------------------------------
dat_trains <- subset(datl, geo %in% c("Austria", "Belgium", "Finland", "Sweden")
                     & time %in% 2000:2012 
                     & vehicle == "Trains")

dat_trains_wide <- spread(dat_trains, geo, values) 
kable(subset(dat_trains_wide, select = -vehicle), row.names = FALSE)

## ----trains_plot, fig.width=6, fig.height=3------------------------------
library(ggplot2)
p <- ggplot(dat_trains, aes(x = time, y = values, colour = geo)) 
p <- p + geom_line()
print(p)

## ----plotGallery, warning=FALSE, message=FALSE, fig.width=6, fig.height=6----
library(tidyr)
library(plotrix)
library(eurostat)
library(dplyr)
library(tidyr)

# All sources of renewable energy are to be grouped into three sets
 dict <- c("Solid biofuels (excluding charcoal)" = "Biofuels",
 "Biogasoline" = "Biofuels",
 "Other liquid biofuels" = "Biofuels",
 "Biodiesels" = "Biofuels",
 "Biogas" = "Biofuels",
 "Hydro power" = "Hydro power",
 "Tide, Wave and Ocean" = "Hydro power",
 "Solar thermal" = "Wind, solar, waste and Other",
 "Geothermal Energy" = "Wind, solar, waste and Other",
 "Solar photovoltaic" = "Wind, solar, waste and Other",
 "Municipal waste (renewable)" = "Wind, solar, waste and Other",
 "Wind power" = "Wind, solar, waste and Other",
 "Bio jet kerosene" = "Wind, solar, waste and Other")
# Some cleaning of the data is required
 energy3 <- get_eurostat("ten00081") %>%
 label_eurostat(dat) %>%
 filter(time == "2013-01-01",
 product != "Renewable energies") %>%
 mutate(nproduct = dict[as.character(product)], # just three categories
 geo = gsub(geo, pattern=" \\(.*", replacement="")) %>%
 select(nproduct, geo, values) %>%
 group_by(nproduct, geo) %>%
 summarise(svalue = sum(values)) %>%
 group_by(geo) %>%
 mutate(tvalue = sum(svalue),
 svalue = svalue/sum(svalue)) %>%
 filter(tvalue > 1000) %>% # only large countries
 spread(nproduct, svalue)
 
# Triangle plot
 par(cex=0.75, mar=c(0,0,0,0))
 positions <- plotrix::triax.plot(as.matrix(energy3[, c(3,5,4)]),
                     show.grid = TRUE,
                     label.points= FALSE, point.labels = energy3$geo,
                     col.axis="gray50", col.grid="gray90",
                     pch = 19, cex.axis=0.8, cex.ticks=0.7, col="grey50")

 # Larger labels
 ind <- which(energy3$geo %in%  c("Norway", "Iceland","Denmark","Estonia", "Turkey", "Italy", "Finland"))
 df <- data.frame(positions$xypos, geo = energy3$geo)
 points(df$x[ind], df$y[ind], cex=2, col="red", pch=19)
 text(df$x[ind], df$y[ind], df$geo[ind], adj = c(0.5,-1), cex=1.5)

## ----maps1, eval=TRUE, fig.width=8, fig.height=8-------------------------
library(eurostat)
library(dplyr)
library(ggplot2)
# Data from Eurostat
eurostat::get_eurostat("tgs00026", time_format = "raw") %>% 
  # subset to have only a single row per geo
  dplyr::filter(time == 2010, nchar(as.character(geo)) == 4) %>% 
  # categorise
  dplyr::mutate(cat = cut_to_classes(values, n = 5)) %>% 
  # merge with geodata
  merge_eurostat_geodata(data=.,geocolumn="geo",resolution = "60", output_class = "df", all_regions = TRUE) %>% 
  # plot map
  ggplot(data=., aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=cat),color="white", size=.1) +
  scale_fill_brewer(palette ="Oranges")


## ----maps2, fig.width=8, fig.height=8------------------------------------
library(eurostat)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
# Downloading and manipulating the tabular data
df <- get_eurostat("tgs00026", time_format = "raw") %>% 
  # subsetting to year 2005 and NUTS-3 level
  dplyr::filter(time == 2005, nchar(as.character(geo)) == 4, grepl("PL",geo)) %>% 
  # label the single geo column
  mutate(label = label_eurostat(.)[["geo"]],
         cat = cut_to_classes(values)) %>% 
  # merge with geodata
  merge_eurostat_geodata(data=.,geocolumn="geo",resolution = "01", all_regions = FALSE, output_class="df")

# plot map
p <- ggplot(data=df, aes(long,lat,group=group))
p <- p + geom_polygon(aes(fill = cat),colour="white",size=.8)
p <- p + scale_fill_manual(values=brewer.pal(n = 5, name = "Oranges"))

p <- p + geom_label(data=df %>% group_by(label,values,cat) %>% summarise(long = mean(long),
                                                         lat = mean(lat)), 
                    aes(long, lat, label = paste(label,"\n",values,"€"), group=label,fill=cat), 
                    size=3.5, color="white", fontface="bold", lineheight=.8, show.legend=FALSE)
p <- p + labs(title = paste0("Disposable household incomes in 2005"))
p <- p + guides(fill = guide_legend(title = "EUR per Year",title.position = "top", title.hjust=0))
p

## ----maps3, fig.width=8, fig.height=8, dev='CairoPNG'--------------------
library(sp)
library(eurostat)
library(dplyr)
dat <- get_eurostat("tgs00026", time_format = "raw") %>% 
  # subsetting to year 2005 and NUTS-3 level
  dplyr::filter(time == 2005, nchar(as.character(geo)) == 4) %>% 
  # classifying the values the variable
  dplyr::mutate(cat = cut_to_classes(values)) %>% 
  # merge Eurostat data with geodata from Cisco
  merge_eurostat_geodata(data=.,geocolumn="geo",resolution = "10", output_class ="spdf", all_regions=FALSE) 

# plot map
sp::spplot(obj = dat, "cat", main = "Disposable household income",
	   xlim=c(-22,34), ylim=c(35,70), 
           col.regions = c("dim grey", brewer.pal(n = 5, name = "Oranges")),
	   col = "white", usePolypath = FALSE)

## ----rsdmx, fig.width=8, fig.height=8, dev='CairoPNG'--------------------
library(rsdmx)

# Data set URL
url <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/cdh_e_fos/..PC.FOS1.BE/?startperiod=2005&endPeriod=2011 "

# Read the data from eurostat
d <- readSDMX(url)

# Convert to data frame and show the first entries
df <- as.data.frame(d)

kable(head(df))

## ----citation, message=FALSE, eval=TRUE, echo=TRUE-----------------------
citation("eurostat")

## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()

