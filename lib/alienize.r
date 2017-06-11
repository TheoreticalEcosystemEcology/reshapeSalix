## packages needed
library(magrittr)
library(letiRmisc)
library(reshape2)
library(stringi)
library(taxize)
# devtools::install_github("TheoreticalEcosystemEcology/alien")
# devtools::install_github("letiR/letiRmisc")

## Use formated data
source("lib/format4R.r")
get_formatData("./data/Salix_webs.csv")

df_galler <- readRDS("rdata/df_galler.rds")
df_parasit <- readRDS("rdata/df_parasit.rds")
df_salix <- readRDS("rdata/df_salix.rds")
df_intract <- readRDS("rdata/df_interact.rds")
df_site <- readRDS("rdata/df_site.rds")
df_salix_galler <- readRDS("rdata/df_salix_galler.rds")




######################### BUILDING dfNodes

## add species
df_parasit$SPECIES <- keepWords(df_parasit$FULL_NAME, 2, punct.rm=F, collapse=" ")
df_parasit$SPECIES[119L] <- "insignipennis"

## Issues ?
# "Theroscopus hemipteron (Riche 1791) insignipennis (Schmiedeknecht 1905)"
# "Saotis pygidiator pygidiator Kasparyan & Kopelke 2009"
# Theroscopus insignipennis Schmiedeknecht 1905
# "Kasparyan & Kopelke 2009"

## add Authors
df_salix$AUTHOR <- NA_character_
df_parasit$AUTHOR <- keepWords(df_parasit$FULL_NAME, 3:8, punct.rm=F,
  collapse=" ") %>% unlist %>% gsub(pat=" NA|\\(|\\)", rep="")
df_parasit$AUTHOR[113L] <- "Kasparyan & Kopelke 2009"
df_parasit$AUTHOR[119L] <- "Schmiedeknecht 1905"
df_galler$AUTHOR %<>% gsub(pat="\\(|\\)", rep="")

## Add Order and Genus
##
df_salix$ORDER <- "Malpighiales"
df_salix$FAMILY <- "Salicaceae"
df_salix$GENUS <- "Salix"
##
df_galler$ORDER <- "Hymenoptera"
df_galler$FAMILY <- "Tenthredinidae"

## add a Type
df_salix$TYPE <- "Salix"
df_galler$TYPE <- "Galler"
df_parasit$TYPE <- "Parasitoid"


## Hybrids
df_salix$HYBRIDS <- df_parasit$HYBRIDS <- df_galler$HYBRIDS <- FALSE
df_salix$HYBRIDS[grepl(df_salix$SPECIES, pat=" x ")] <- TRUE


## RESOLVED
df_salix$RESOLVED <- df_parasit$RESOLVED <- df_galler$RESOLVED <- "Species"
df_salix$RESOLVED[grepl(df_salix$SPECIES, pat="?", fixed=TRUE)] <- "Genus"
##
df_parasit$RESOLVED[grepl(df_salix$SPECIES, pat="?", fixed=TRUE)] <- "Genus"
##
df_parasit$RESOLVED[grepl(df_parasit$SPECIES, pat="sp[\\.1-9\\?]{1,2}")] <- "Genus"
idb <- grepl(df_parasit$GENUS, pat="(blank)", fixed=TRUE)
df_parasit$RESOLVED[idb] <- "Family"
df_parasit$GENUS[idb] <- NA_character_
# NB: id_row = 38 => superfamily

##
df_parasit$RESOLVED[grepl(df_galler$SPECIES, pat="sp\\.")] <- "Genus"
df_galler$RESOLVED[grepl(df_galler$SPECIES, pat="\\?")] <- "Genus"

##
names(df_parasit)[1] <- "idNodes"
names(df_galler)[1] <- "idNodes"
names(df_salix)[1] <- "idNodes"
names(df_galler)[7] <- "CODE_GALLTYPE_SIMPLE"
names(df_parasit)[7:10] <- c("PAR_INQ", "ENDO_ECTO", "KOINO_IDIO", "TARGETED_LIFESTAGE")

df_parasit$CODE_GALLTYPE_SIMPLE <- df_salix$CODE_GALLTYPE_SIMPLE <- NA_character_
df_salix$PAR_INQ <- df_galler$PAR_INQ <- NA_character_
df_salix$ENDO_ECTO <- df_galler$ENDO_ECTO <- NA_character_
df_salix$KOINO_IDIO <- df_galler$KOINO_IDIO <- NA_character_
df_salix$TARGETED_LIFESTAGE <- df_galler$TARGETED_LIFESTAGE <- NA_character_

idnm <- c("idNodes", "ORDER", "FAMILY", "GENUS", "SPECIES", "RESOLVED", "AUTHOR",
          "TYPE", "HYBRIDS", "CODE_GALLTYPE_SIMPLE", "PAR_INQ", "ENDO_ECTO", "KOINO_IDIO", "TARGETED_LIFESTAGE")

####
dfNodes <- rbind(
  df_salix[idnm],
  df_galler[idnm],
  df_parasit[idnm]
  )



######################### Building dfSite
dfSite <- df_site[, c('SITE', 'NDECDEG', 'EDECDEG', 'COUNTRY', 'REGION')] %>% unique
dfSite$idSite <- paste0("site", sprintf("%03d", 1:nrow(df_site2)))
rownames(dfSite) <- NULL
dfSite <- dfSite[, c(6,1:3,5,4)]
#### add idSite to df_site

df_site$idSite <- df_intract$idSite <- ""
for (i in 1:nrow(df_site)) {
  sco <- (dfSite[, 'SITE'] == df_site[i, 'SITE']) +
    (dfSite[, 'NDECDEG'] == df_site[i, 'NDECDEG']) +
    (dfSite[, 'EDECDEG'] == df_site[i, 'EDECDEG'])
  # print(which(sco==3))
  df_site$idSite[i] <- dfSite$idSite[which(sco==3)]
}
##
for (i in 1:nrow(df_intract)) {
  id <- which(df_site$REARING_NUMBER == df_intract$REARING_NUMBER[i])
  df_intract$idSite[i] <- df_site$idSite[id]
}



######################### Building dfEdges

aggregate(NB_GALLS_PAR ~ REARING_NUMBER*RSAL*RGALLER, data = df_intract, sum)


int_gal <- df_intract[, c("RSAL", "RGALLER", "N_GALLS", "idSite")] %>% unique
int_par <- df_intract[, c("RGALLER", "RPAR", "NB_GALLS_PAR", "idSite")] %>% unique

int_par <- int_par[ -which(int_par$RPAR=="none"),]

names(int_gal)[1:3] <- c("idFrom", "idTo", "value")
names(int_par)[1:3] <- c("idFrom", "idTo", "value")

dfEdges <- rbind(int_gal, int_par)



######################### SAVING THA DATA
dfSite$SITE %<>% stri_trans_general("latin-ascii")
dfSite$REGION %<>% stri_trans_general("latin-ascii")
kopelke <- alienData(dfNodes, dfEdges, trait = 7:13, taxo = c(2:6), dfSite = dfSite)
save(kopelke, file='kopelke.rda', compress='xz', ascii=TRUE)
# # devtools::use_data(sal, salix)
# # save(salix, file="salix.Rda")
# # save(bartomeus, file='bartomeus.rda', compress='xz')
