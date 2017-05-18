# Format original data from Kolpelke et al. 2017 with R

This document explains how to reshape the original dataset from Kolpelke et al. 2017 ([DOI: 10.1002/ecy.1832](http://doi.wiley.com/10.1002/ecy.1832)), and then explore and manipulate the new data structure. This document is not intended to be a description of the dataset, we rather provide lines of code to import and handle the dataset in the statistical language R. Notably, we split the data set into different tables for which primary keys are assigned (i.e. unique identifier) allowing us to easily retrieve pieces of information from each tables. We exemplify the handling of the data and we further show how to obtain a quick map and how to use the data for network analyses. For further details, please have a look at the document entitled `summary.pdf`.

**Important -** A maintained version of this original content can be found on Github plateform at this address: https://github.com/TheoreticalEcosystemEcology/reshapeSalix

# Setting up

## Compile `summary.Rmd`

```r
setwd("path/to/the/Salix/folder")
install.packages("rmarkdown")
install.packages("magrittr")
library(magrittr)
rmarkdown::render("summary.Rmd", output_format = "all")
```

## Run reshape script `get_formatData()`

```r
setwd("path/to/the/Salix/folder")
install.packages("magrittr")
library(magrittr)
source("./lib/format4R.r")
get_formatData("./data/Salix_webs.csv")
```
