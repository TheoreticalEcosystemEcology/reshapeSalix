netCoocData <- function(dfNodes, dfEdges, trait = NULL, phylo = NULL, taxo = NULL,
    dfSites = NULL, siteEnv = NULL, dfOcc = NULL, directed = FALSE, verbose = TRUE) {

    ##############################
    osaf <- options()
    options(stringsAsFactors = FALSE)
    on.exit(options(osaf))

    ############################## dfNodes
    if (is.vector(dfNodes)) {
        dfNodes <- data.frame(ID = as.character(dfNodes))
    }
    ##
    dfNodes %<>% as.data.frame
    dfEdges %<>% as.data.frame
    ##
    stopifnot("idNodes" %in% names(dfNodes))
    stopifnot("idFrom" %in% names(dfEdges))
    stopifnot("idTo" %in% names(dfEdges))
    ##
    dfNodes$idNodes %<>% as.character
    dfEdges$idFrom %<>% as.character
    dfEdges$idTo %<>% as.character
    ##
    stopifnot(!any(table(dfNodes$idNodes) > 1))
    if (verbose)
        message("==> Nodes information detected")

    ##
    sc <- 0
    if (is.null(trait)) {
        nmTrait <- NULL
        if (verbose)
            message("==> No traits detected")
    } else {
        nmTrait <- names(dfNodes[, trait, drop = FALSE])
        sc <- 1
        if (verbose)
            message(paste0("==> Traits detected: ", paste(nmTrait, collapse = ", ")))
    }
    ##
    if (is.null(phylo)) {
        nmPhylo <- NULL
        if (verbose)
            message("==> No phylo detected")
    } else {
        nmPhylo <- names(dfNodes[, phylo, drop = FALSE])
        sc <- 1
        if (verbose)
            message(paste0("==> Phylo detected: ", paste(nmPhylo, collapse = ", ")))
    }
    ##
    if (is.null(taxo)) {
        nmTaxo <- NULL
        if (verbose)
            message("==> No taxon detected")
    } else {
        nmTaxo <- names(dfNodes[, taxo, drop = FALSE])
        sc <- 1
        if (verbose)
            message(paste0("==> Taxo detected: ", paste(nmTaxo, collapse = ", ")))
    }

    ############################## dfEdges
    stopifnot(all(dfEdges$idFrom %in% dfNodes$idNodes))
    stopifnot(all(dfEdges$idTo %in% dfNodes$idNodes))
    idn <- which(!dfNodes$idNodes %in% c(dfEdges$idFrom, dfEdges$idTo))
    if (length(idn))
        warning(paste0("Unlinked nodes: ", paste(dfNodes$idNodes[idn], collapse = ", ")))

    if (!"value" %in% names(dfEdges)) {
        if (verbose)
            message("==> No Edges' value detected, values are set to 1")
        dfEdges$value <- 1
    } else if (verbose)
        message("==> Edges' values detected")


    ############################## dfSites
    nmSite <- NULL
    if (is.null(dfSites)) {
        if ("idSite" %in% names(dfEdges)) {
            if (verbose)
                message("==> Sites' ID are provided by dfEdges")
            dfSites <- data.frame(idSite = unique(dfEdges$idSite))
            nbSites <- nrow(dfSites)
        } else {
            if (verbose)
                message("==> No site info detected")
            nbSites <- NULL
        }

    } else {
        stopifnot("idSite" %in% names(dfSites))
        stopifnot(all(table(dfSites$idSite) == 1))
        ##
        if (!is.null(siteEnv)) {
            ##
            dfSites <- cbind(idSite = dfSites$idSite, dfSites[, siteEnv, drop = FALSE])
            dfSites <- dfSites[, unique(names(dfSites))]
            ##
        }
        if (ncol(dfSites) > 1) {
            nmSite <- names(dfSites)[-1L]
            if (verbose)
                message(paste0("==> Site info detected: ", paste(nmSite, collapse = ", ")))
        } else if (verbose)
            message("==> No site info detected")
        ##
        dfSites$idSite %<>% as.character
        if ("idSite" %in% names(dfEdges)) {
            if (!all(dfSites$idSite %in% dfEdges$idSite)) {
                warnings("Sites without interaction records")
            }
        }
        nbSites <- nrow(dfSites)
    }

    ############################## dfOcc
    occ <- FALSE
    if ("idSite" %in% names(dfEdges) & is.null(dfOcc)) {
        stopifnot(all(dfEdges$idSite %in% dfSites$idSite))
        dfEdges$idSite %<>% as.character
        if (verbose)
            message("==> Getting occurrence information from 'dfEdges'...")
        ##
        dfOcc <- data.frame(id = c(dfEdges$idTo, dfEdges$idFrom), idSite = rep(dfEdges$idSite,
            2)) %>% unique
        ##
        names(dfOcc)[1L] <- "idNodes"
    }


    if (!is.null(dfOcc)) {
        stopifnot("idSite" %in% names(dfOcc))
        dfOcc$idSite %<>% as.character
        stopifnot("idNodes" %in% names(dfOcc))
        ##
        stopifnot(all(dfOcc$idSite %in% dfSites$idSite))
        stopifnot(all(dfOcc$idNodes %in% dfNodes$idNodes))
        occ <- TRUE
        if (!all(dfNodes$idNodes %in% dfOcc$idNodes))
            warning("Nodes without any occurrence record.")
        if (verbose)
            message("==> Occurrence information detected")
        dfOcc$idNodes %<>% as.character
        ##
        nbOcc <- nrow(dfOcc)
    } else {
        if (!is.null(nbSites))
            warning("Site information provided without any occurrence")
        nbOcc <- NULL
    }

    ## output
    res <- list(dfNodes = dfNodes, dfEdges = dfEdges, dfSites = dfSites, dfOcc = dfOcc,
        info = list(nbNodes = nrow(dfNodes), nbEdges = nrow(dfEdges), directed = directed,
            nbSites = nbSites, nbOcc = nbOcc, nmTrait = nmTrait, nmPhylo = nmPhylo,
            nmTaxo = nmTaxo, nmSite = nmSite))
    class(res) <- "netCooc"
    res
}
