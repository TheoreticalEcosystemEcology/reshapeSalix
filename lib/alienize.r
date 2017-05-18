## packages needed
library(magrittr)
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


df_interact$ID_site <- ""
for (i in 1:nrow(df_interact)){
  id <- which(as.character(df_site$REARING_NUMBER) == as.character(df_interact$REARING_NUMBER[i]))
  df_interact$ID_site[i] <- df_site$ID[id]
}


tmp <- data.frame(
    IdSite = rep(df_interact$ID_site, 3),
    IDSp = c(as.character(df_interact$RSAL),
      as.character(df_interact$RGALLER),
      df_interact$RPAR)
    )
id <- which(tmp$IDSp == "none")
##
idObs <- tmp[-id, ]
## Mettre les individus... :)
# siteEnv <-




####----- InteractPair

# interactPair
# df_interact



####----- INFO SITES



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
  species = c(df_salix$SPECIES,
    df_galler$SPECIES,
    df_parasit$FULL_NAME %>% keepWords(2) %>% unlist)
  )




#@param siteEnv A matrix or a data frame where each column is a descriptor of the sites. TODO: siteEnv should cover the possibility that environmental variables could be taken at several times - link to idTime in idObs?.
#@param traitSp A matrix or a data frame where each column is a trait characterizing all species. The first column is a unique identifier of the species documented in \code{idObs} data.frame.
#@param traitInd



####----- SAVING THA DATA

# as.alienData(idObs, )
# save(x, file="x.Rda")
