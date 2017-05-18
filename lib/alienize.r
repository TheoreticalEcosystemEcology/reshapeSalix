## packages needed
library(magrittr)
library(reshape2)
library(stringi)
devtools::install_github("TheoreticalEcosystemEcology/alien")
devtools::install_github("letiR/letiRmisc")

## Use formated data
source("lib/format4R.r")
get_formatData("./data/Salix_webs.csv")

df_galler <- readRDS("rdata/df_galler.rds")
df_parasit <- readRDS("rdata/df_parasit.rds")
df_salix <- readRDS("rdata/df_salix.rds")
df_intract <- readRDS("rdata/df_interact.rds")
df_site <- readRDS("rdata/df_site.rds")
df_salix_galler <- readRDS("rdata/df_salix_galler.rds")



####------ idObs

df_site2 <- df_site[, c('SITE', 'NDECDEG', 'EDECDEG', 'COUNTRY', 'REGION')] %>% unique
df_site2$ID <- paste0("site", 1:nrow(df_site2))
## assigning an unique id to sites
df_site$ID <- ""
for (i in 1:nrow(df_site)){
  tmp <- (df_site$SITE[i] == df_site2$SITE) +
    (df_site$NDECDEG[i] == df_site2$NDECDEG) +
    (df_site$EDECDEG[i] == df_site2$EDECDEG)
  id <- which(tmp==3)
  df_site$ID[i] <- df_site2$ID[id]
}
## assigning an unique id to sites in df_interact
df_interact$ID_site <- ""
for (i in 1:nrow(df_interact)){
  id <- which(as.character(df_site$REARING_NUMBER) == as.character(df_interact$REARING_NUMBER[i]))
  df_interact$ID_site[i] <- df_site$ID[id]
}

tmp <- data.frame(
    IdSite = rep(df_interact$ID_site, 3),
    IDSp = c(as.character(df_interact$RSAL),
      as.character(df_interact$RGALLER),
      df_interact$RPAR),
    stringsAsFactors = FALSE
    )
id <- which(tmp$IDSp == "none")

idObs <- tmp[-id, ] %>% unique
## TODO Mettre les individus pour Salix... :)




####----- interactPair

# interactPair
# df_interact (2 times!)
rsal <- df_interact$RSAL %>% as.character
rgal <- df_interact$RGALLER %>% as.character
rpar <- df_interact$RPAR %>% as.character
interactPair <- data.frame(
  idFrom = c(rsal, rgal),
  idTo = c(rgal, rpar),
  stringsAsFactors = FALSE
  )
##-- Add frequence as value
conc <- paste0(interactPair$idFrom, interactPair$idTo)
tbint <- table(conc)
# Unique pair of interactip
interactPair <- unique(interactPair)
interactPair$value <- NA
for (i in 1:nrow(interactPair)){
  tmpconc <- paste0(interactPair$idFrom[i],interactPair$idTo[i])
  id <- which(names(tbint) == tmpconc)
  interactPair$value[i] <- tbint[id]
}
interactPair <- interactPair[-which(interactPair$idTo == "none"),]



####----- INFO SITES
sitetmp <- df_site2[,c(6,2:5,1)]
## removing non-ascii character
sitetmp$SITE <- stri_trans_general(sitetmp$SITE, "latin-ascii")
sitetmp$REGION <- stri_trans_general(sitetmp$REGION, "latin-ascii")
siteEnv <- melt(sitetmp, id.vars = names(sitetmp)[1])
# names(siteEn)



####----- INFO SPECIES
traiSP <- data.frame(
  idSp = c(df_salix$RSAL %>% as.character,
     df_galler$RGAL %>% as.character,
     df_parasit$RPAR %>% as.character),
  categ = c(rep("Salix", nrow(df_salix)),
    rep("Galler", nrow(df_galler)),
    rep("Parasitoid", nrow(df_parasit))),
  genus = c(rep("Salix", nrow(df_salix)),
    df_galler$GENUS,
    df_parasit$GENUS),
  species = c(df_salix$SPECIES %>% stri_trans_general("latin-ascii"),
    df_galler$SPECIES %>% stri_trans_general("latin-ascii"),
    df_parasit$FULL_NAME %>% keepWords(2) %>% unlist %>% stri_trans_general("latin-ascii")),
  stringsAsFactors = FALSE
  )




####----- SAVING THA DATA
kopelke <- alien::as.alienData(idObs, interactPair, siteEnv = siteEnv, traitSp = traiSP)
save(kopelke, file='kopelke.rda', compress='xz', ascii=TRUE)
# devtools::use_data(sal, salix)
# save(salix, file="salix.Rda")
# save(bartomeus, file='bartomeus.rda', compress='xz')
