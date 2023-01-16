
library(dplyr)
library(knitr)
library(kableExtra)

if (!require(taxize, quietly=TRUE)) {
  install.packages('taxize')
  library(taxize)
}

getTaxo <- function(Species) {
  taxa <- try(taxize::classification(Species, 'worms', rows=1), silent=TRUE)
  if (class(taxa)=="try-error") {
    taxa <-  taxize::classification(Species, 'col', rows=1)
  }
  taxa
}

# Make the Table
alldirs <- list.dirs("OM_case_studies", recursive = FALSE)

# loop over case studies
CSList <- list()
for (i in seq_along(alldirs)) {
  CSname <- basename(alldirs[i])
  message(CSname)
  source.dir <- file.path(alldirs[i])
  fls <- list.files(source.dir)

  if(!file.exists(file.path(source.dir, 'OM.rdata'))) {
    warning("OM.rdata not found for ", CSname, ". Skipping...", call. = FALSE)
    break
  } else {
    OM <- readRDS(file.path(source.dir, 'OM.rdata'))
  }

  # Meta-data
  Species <- OM@Species
  Agency <- OM@Agency
  Sponsor <- OM@Sponsor
  Region <- OM@Region
  if (length(Species)<1) Species = ''
  if (length(Agency)<1) Agency = ''
  if (length(Sponsor)<1) Sponsor = ''
  if (length(Region)<1) Region = ''

  hasDatarep <- FALSE
  hasOMrep <- FALSE
  hasOMxl <- FALSE

  if (file.exists(file.path(source.dir,paste0(CSname, '.html')))) hasOMrep <- TRUE
  if (file.exists(file.path(source.dir,paste0(CSname, '.pdf')))) hasOMrep <- TRUE
  if (file.exists(file.path(source.dir,paste0(CSname, '.xlsx')))) hasOMxl <- TRUE

  # get taxonomic info
  taxo <- try(getTaxo(OM@Species), silent=TRUE)
  if (class(taxo)=="try-error" | is.na(OM@Species)) {
    phylum <- "not found"
    class <- "not found"
    order <- "not found"
    family <- "not found"
    genus <- "not found"
    species <- OM@Species
    subspecies <- "not found"
  } else {
    phylum <- taxo[[1]]$name[tolower(taxo[[1]]$rank) == "phylum"]
    class <- taxo[[1]]$name[tolower(taxo[[1]]$rank) == "class"]
    order <- taxo[[1]]$name[tolower(taxo[[1]]$rank) == "order"]
    family <- taxo[[1]]$name[tolower(taxo[[1]]$rank) == "family"]
    genus <- taxo[[1]]$name[tolower(taxo[[1]]$rank) == "genus"]
    species <- taxo[[1]]$name[tolower(taxo[[1]]$rank) == "species"]
    subspecies <- taxo[[1]]$name[taxo[[1]]$rank == "subspecies"]
    ckhSub <- taxo[[1]]$name[tolower(taxo[[1]]$rank) == "infraspecies"]
    if (length(ckhSub)>0 && nchar(ckhSub) >0) species <- ckhSub
  }

  # source link
  base.url <- file.path("https://github.com/Blue-Matter/MSEextra/raw/master/OM_case_studies", CSname)
  base.url2 <- file.path('https://github.com/Blue-Matter/MSEextra/blob/master/OM_case_studies', CSname)

  pdfs <- list.files(source.dir, pattern = '.pdf')
  if (hasOMrep) {
    OM.pdf <- pdfs[grepl(CSname, pdfs)]
    if (length(OM.pdf)>0) {
      OMrep <- OM.pdf
    } else {
      OMrep <- paste0(CSname, '.html')
    }
  } else {
    OMrep <- ''
  }


  Data.pdf <- pdfs[grepl('Data-Report', pdfs)]

  hasDatarep <- FALSE
  if (length(Data.pdf)>0) {
    Datarep <- Data.pdf
    hasDatarep <- TRUE
  } else {
    Datarep <- ''
  }

  if (hasOMxl) {
    OMxl <- paste0(CSname, '.xlsx')
  } else {
    OMxl <- ''
  }

  OM.path <- file.path(paste0('https://htmlpreview.github.io/?', base.url2), OMrep)
  if (grepl('pdf', OMrep)) {
    OM.path <- file.path(paste0('https://docs.google.com/viewer?url=', base.url), OMrep)
  }

  Data.path <- file.path(paste0('https://htmlpreview.github.io/?', base.url2), Datarep)
  if (grepl('pdf', Datarep)) {
    Data.path <- file.path(paste0('https://docs.google.com/viewer?url=', base.url), Datarep)
  }

  df <- data.frame(Species, Region, Agency, Sponsor,
                   OM.Report=OM.path,
                   OM.XL=file.path(base.url, OMxl),
                   Data.Report=Data.path,
                   hasOMrep=hasOMrep,
                   hasOMxl=hasOMxl,
                   hasDatarep=hasDatarep,
                   OMpathout=file.path(base.url, 'OM.rdata'),
                   Class=class, Family=family,
                   Name=OM@Common_Name)
  CSList[[i]] <- df
}

DF <- do.call('rbind', CSList)

saveRDS(DF, 'build_package/DF.rda')
