
##-- Packages
## @knitr settingmaps
library(rgdal)
library(raster)
library(dismo)

## @knitr tospatialobject
df_site <- readRDS("rdata/df_site.rds")[,c(
  "SITE", "NDECDEG", "EDECDEG")] %>% unique
#
sp_site <- SpatialPointsDataFrame(
    df_site[,c("EDECDEG", "NDECDEG")],
    df_site[c("SITE")],
    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs
     +ellps=WGS84 +towgs84=0,0,0")
    )


## @knitr quickmap
## background map
bg_map <- gmap('Europe', type="satellite",
  zoom=3, exp=1.1, scale=2, add=T)
## map
par(mar=c(1,1,1,1))
plot(c(-258698, 4351808), c(4350881, 11745460), asp=1, ann=F, axes=F, type="n")
plot(bg_map, add=TRUE)
plot(spTransform(sp_site, CRS("+init=epsg:3857")), add=TRUE,
  col="grey25", bg="grey75", cex=1, pch=21)

## @knitr get
climate <- getData('worldclim', var='bio', res=2.5)
clim_site <- extract(climate,sp_site,df=TRUE)

## @knitr merge
clim_site <- data.frame(SITE=sp_site@data$SITE,clim_site[,-1])
str(clim_site)


## @knitr visit
tmp <- readRDS("rdata/df_site.rds")
par(las=1, cex.axis=.8, mgp=c(2, .4, 0), tcl=-0.2)
cool <- tmp %>% `[`(,c("EDECDEG", "NDECDEG", "YEAR_OF_COLL")) %>%
  unique %>% `[`(,c("EDECDEG", "NDECDEG")) %>%
  apply(1,paste, collapse="/") %>% table %>% table %>%
  graphics::barplot(border=NA, col="grey25")
mtext(1, text= "Number of times visited", line=1.25)
mtext(3, at=-1,  text= "Number of sites", line=.8, adj=0)
