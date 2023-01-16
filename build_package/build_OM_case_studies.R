library(openMSE)
library(dplyr)
library(usethis)

# setwd("C:/Users/Adrian/Documents/GitHub/MSEextra")
setwd('C:/Users/User/Documents/GitHub/MSEextra')

pkg.dir <- getwd()
if (!grepl('MSEextra', pkg.dir))
  stop('Current directory must be MSEextra project')
OM.path <- 'G:/Shared drives/BM shared/4. Resources/3_OM_case_studies'

OM.dirs <- list.dirs(OM.path, recursive = FALSE)
OM.dirs <- OM.dirs[!grepl('Z - INCOMPLETE', OM.dirs)]

OMs <- basename(OM.dirs) # names of the OMs
OMdf <- data.frame(OMs=OMs, path=OM.dirs, build=TRUE, success=TRUE, error=NA)

# set build=FALSE to skip re-building (e.g. importing from SS)


runs <- c(2,3,4,9,10,11,13,21,27,28) #  which(grepl('CDFW', OMdf$OMs))
# ---- Build the OMs and Documentation for each OM in OMdf (where build=TRUE) ----
for (XX in 1:nrow(OMdf)) {
# for (XX in runs) {
  OMname <- as.character(OMdf$OMs[XX])
  message(OMname)
  fp <- OMdf$path[XX]

  # Build the OM and documentation
  if (OMdf$build[XX]) {
    unlink(file.path(fp,'build'), recursive=TRUE, force=TRUE) # delete old build files
    setwd(fp)
    tt <- try(source('Build.r'))
    if (class(tt) == 'try-error') {
      OMdf$success[XX] <- FALSE
      OMdf$error[XX] <- tt[1]
    }
    setwd(pkg.dir)
  }
}

if (any(!OMdf$success))
  stop('Some OMs did not build successfully!')




# ---- Copy over files to MSEextra ----
MSEextra.path <- 'OM_case_studies'

RoxygenFile <- "Roxy_DataObjects.r" # name of R script with roxygen
file.remove(file.path(pkg.dir, 'R/', RoxygenFile)) # delete
file.create(file.path(pkg.dir, 'R/', RoxygenFile)) # make empty file

cat("# This file is automatically built by build_package/build_OM_case_studies.R \n",
    "# Don't edit by hand!\n",
    "# \n\n", sep="", append=TRUE,
    file=file.path(pkg.dir, 'R/', RoxygenFile))

cat("#' @name MSEextra-OM",
    "\n#' @aliases MSEextra MSEextra-package",
    "\n#' @docType data",
    "\n#' @title Operating models and MSE output for openMSE",
    "\n#' @description A suite of operating models (class OM)",
    "\n#' @references \\url{}",
    "\n#' @examples",
    "\n#' library(openMSE)",
    "\n#' library(MSEextra)",
    "\n#' myMSE <- runMSE(Shortspine_Thornyhead_BC_DFO)",
    "\n#' plot(Shortspine_Thornyhead_BC_DFO)",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path(pkg.dir, 'R/', RoxygenFile))


OMdf <- OMdf %>% dplyr::filter(success==TRUE)
for (XX in 1:nrow(OMdf)) {
    OMname <- as.character(OMdf$OMs[XX])
    message(OMname)

    src.path <- file.path(OM.path, OMname)
    out.path <- file.path(MSEextra.path,OMname)

    # create directory
    if (!dir.exists(out.path)) dir.create(out.path)

    # copy docs folder
    if (dir.exists(file.path(src.path, 'docs'))) {
      if (!dir.exists(file.path(out.path, 'docs'))) dir.create(file.path(out.path, 'docs'))
      file.copy(file.path(src.path, 'docs'), out.path,
                overwrite = TRUE, recursive = TRUE)
    }

    # copy OM.rdata
    if (file.exists(file.path(src.path, "OM.rdata"))) {
      file.copy(file.path(src.path, "OM.rdata"), file.path(out.path, "OM.rdata"),
                overwrite = TRUE)

      # add to MSEextra data
      OM <- readRDS(file.path(out.path, "OM.rdata"))
      assign(OMname, OM)
      do.call("use_data", list(as.name(OMname), overwrite = TRUE))

      # Write roxygen
      cat("#' @rdname MSEextra-OM", '\n"', OMname, '"\n\n\n', sep="", append=TRUE,
          file=file.path(pkg.dir, 'R/', RoxygenFile))

    }

    # copy Excel
    fls <- list.files(src.path, pattern='.xlsx')
    if (length(fls) >1) fls <- fls[1]
    if (length(fls) <1) {
      OMxl <- ''
    } else {
      OMxl <- fls
    }
    if (nchar(OMxl) >0) {
      tt <- file.copy(file.path(src.path,OMxl), file.path(out.path, OMxl))
    }

    # copy html
    fls <- list.files(src.path, pattern='.html')
    if (length(fls) >1) fls <- fls[fls == paste0(OMname, ".html")]

    if (length(fls) <1) {
      OMrep <- ''
    } else {
      OMrep <- fls
    }
    if (nchar(OMrep) >0) {
      tt <- file.copy(file.path(src.path, OMrep), file.path(out.path, OMrep))
    }

    # copy pdf
    fls <- list.files(src.path, pattern='.pdf')

    if (length(fls) <1) {
      OMrep <- ''
    } else {
      OMrep <- fls
    }
    if (nchar(OMrep) >0) {
      tt <- file.copy(file.path(src.path, OMrep), file.path(out.path, OMrep))
    }

}


