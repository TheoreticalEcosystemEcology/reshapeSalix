
#### SETTINGS
######################################

## Create output folders
mymkdir <-  . %>% {dir.create(. , showWarnings = FALSE)}
tonumeric <- . %>% sub(pat = ",", rep = ".") %>% as.numeric
mysave <- function(obj, file) {
  saveRDS(obj, paste0("./rdata/", file, ".rds"))
  write.csv2(obj, paste0("./csv/", file, ".csv"))
}

## Creating output folders
flds <- c("rdata", "csv") %T>% sapply(mymkdir)

## Cleaning existing file folders
file.remove(list.files(path="./rdata", full.names=T, pattern="*.rds"))
file.remove(list.files(path="./csv", full.names=T, pattern="*.csv"))

#### Main function
######################################

# get_formatData. Please read
# The only argument "filename" is the dataset (Salix_webs.csv available in the data folder).

get_formatData <- function(filename) {
    ####################################### IMPORTING DATA
    data_salix <- read.csv(filename, header = FALSE, stringsAsFactors = FALSE)
    nr <- nrow(data_salix)
    nc <- ncol(data_salix)

    ##-- Bloc sites / gallers / salix
    df_sal_gal <- data_salix[12:nr, 1:24] %>%
      as.data.frame(stringsAsFactors = FALSE)
    df_sal_gal[, c(1,7:9, 19:24)] %<>% apply(2, tonumeric)

    names(df_sal_gal) <- data_salix[11, 1:24] %>% toupper %>%
      sub(pat = " ", rep = "_")
    #
    mat_par_gal <- data_salix[12:nr, 25:nc]
    mat_par_gal[mat_par_gal == ""] <- "0"
    mat_par_gal %<>% apply(2, as.numeric)


    ####################################### SPECIES

    ## NB: We do not have the same amount of information for all level. I therefore
    ## used three different dataframes fot the three level of the web.


    ##-- A- Parasitoids
    parasit <- data_salix[1:11, 24:nc] %>% t
    df_parasit <- parasit[-1, ] %>% as.data.frame(stringsAsFactors = FALSE)
    #
    names(df_parasit) <- parasit[1, ]
    names(df_parasit)[ncol(df_parasit)] <- "FULL_NAME"
    rownames(df_parasit) <- NULL
    df_parasit <- df_parasit[, c(10, 1:9, 11)]


    ##-- B- Salix Species
    df_salix <- df_sal_gal[, 10:11] %>% unique
    #
    names(df_salix) <- c("SPECIES", "AUTHOR")
    rownames(df_salix) <- NULL
    #
    id <- paste0("Sal", 1:nrow(df_salix))
    df_salix %<>% cbind(RSAL = id, ., stringsAsFactors = FALSE)

    ## C- Gallers
    df_galler <- df_sal_gal[, c(12:18)] %>% unique %>%
      as.data.frame(stringsAsFactors = TRUE)
    rownames(df_galler) <- NULL
    df_galler <- df_galler[, c(4, 1:3, 5:7)]


    ####################################### SALIX INDIVIDUALS Unique site x visit
    df_site <- df_sal_gal[, c(2, 1, 3:9)] %>% unique
    rownames(df_site) <- NULL


    ####################################### INTERACTIONS
    ##-- A- SALIX-GALLER

    sal_gal <- df_sal_gal[, c(2:3, 19:24)]
    #
    id_salix <- sapply(df_sal_gal$SALIX, . %>% equals(df_salix$SPECIES, .) %>% which) %>%
        unlist
    id_galler <- sapply(df_sal_gal$GENUS_SPECIES, . %>% equals(df_galler$GENUS_SPECIES,
        .) %>% which) %>% unlist
    #
    df_salix_galler <- cbind(RSAL = df_salix$RSAL[id_salix],
       RGALLER = df_galler$RGALLER[id_galler], sal_gal)

    ##-- B- SALIX-GALLER-PARASITOID
    df_tmp <- df_salix_galler[, 1:5]
    df_tmp$RPAR <- ""
    df_tmp$NB_GALLS_PAR <- 0

    k <- 0
    ls_inter <- list()
    for (i in 1:nrow(df_tmp)) {
        id <- which(mat_par_gal[i, ] != 0)
        if (length(id) > 0) {
            for (j in id) {
                k <- k + 1
                tmp <- df_tmp[i, c(3:4, 1:2, 6, 5, 7)]
                tmp$NB_GALLS_PAR <- mat_par_gal[i, j]
                tmp$RPAR <- df_parasit$RPAR[j]
                ls_inter[[k]] <- tmp
            }
        }
        else {
          k <- k + 1
          tmp <- df_tmp[i, c(3:4, 1:2, 6, 5, 7)]
          tmp$NB_GALLS_PAR <- 0
          tmp$RPAR <- "none"
          ls_inter[[k]] <- tmp
        }
    }
    df_interact <- do.call("rbind", ls_inter)
    df_interact <- df_interact[,-c(which(colnames(df_interact)=="LEG"))]


  ####################################### EXPORTING FILES

  df_parasit$RPAR <- as.factor(df_parasit$RPAR)
  df_salix$RSAL <- as.factor(df_salix$RSAL)
  df_galler$RGALLER <- as.factor(df_galler$RGALLER)
  df_site$REARING_NUMBER <- as.factor(df_site$REARING_NUMBER)
  df_interact$REARING_NUMBER <- as.factor(df_interact$REARING_NUMBER)
  df_site$LEG <- as.Date(df_site$LEG, format="%d.%m.%y")
  colnames(df_site)[which(colnames(df_site)=="YEAR_OF COLL")] <- "YEAR_OF_COLL"

  mysave(df_parasit, "df_parasit")
  mysave(df_salix, "df_salix")
  mysave(df_galler, "df_galler")
  mysave(df_site, "df_site")
  mysave(df_salix_galler, "df_salix_galler")
  mysave(df_interact, "df_interact")

}
