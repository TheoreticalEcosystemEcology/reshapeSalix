#######################################
#### SETTINGS

##-- Packages

## @knitr settingweb
library(igraph)
library(bipartite)
df_interact <- readRDS("rdata/df_interact.Rds")



## @knitr bipartite1
bip_salgal <- df_interact[,c("RSAL","RGALLER")] %>% table
bip_galpar <- df_interact[,c("RGALLER","RPAR")] %>% table

## @knitr bipartite2
C.score(bip_galpar)

## @knitr bipartite3
visweb(bip_salgal)


## @knitr metaweb1
mweb_salgal <- df_interact[,c("RSAL","RGALLER")] %>% unique
igr_salgal <- data.frame(
    from = mweb_salgal$RSAL,
    to = mweb_salgal$RGAL
  ) %>% graph_from_data_frame(directed=TRUE)
#
id <- df_interact$RPAR!="none"
mweb_galpar <- df_interact[id,c("RPAR","RGALLER")] %>% unique
igr_salpar <- data.frame(
    from = mweb_galpar$RGAL,
    to = mweb_galpar$RPAR
  ) %>% graph_from_data_frame(directed=TRUE)

## @knitr metaweb3
metweb <- igraph::union(igr_salgal, igr_salpar)

## @knitr metaweb4
igraph::degree(metweb)[1:20]
