#' Stochastic production in continuous time (spict)
#'
#' A state-space surplus production model that uses a time-series of catches and a relative abundance index
#' and coded in TMB (Pedersen and Berg 2017). This is a wrapper assessment function for the \code{SAMtool} package, which
#' calls the model from the \code{spict} package. There are a multitude of possible configurations for spict. Here, the default is to use
#' \code{n = 2} (symmetric yield curve), \code{dep = 1} (unfished stock in year 1 of model), and \code{alpha, beta = 1} (equal observation
#' and process standard deviations). These can be changed in the function arguments below.
#'
#' @aliases SPiCT
#' @param x An index for the objects in \code{Data} when running in \link[DLMtool]{runMSE}.
#' Otherwise, equals to 1 when running an assessment interactively.
#' @param Data An object of class Data.
#' @param start Optional list of starting values. See details.
#' @param fix_dep Logical, whether to fix the initial depletion (ratio of biomass to carrying capacity in the
#' first year of the model). If \code{TRUE}, uses the value in \code{start}, otherwise equal to 1
#' (assumes unfished conditions).
#' @param fix_n Logical, whether to fix the exponent of the production function. If \code{TRUE},
#' uses the value in \code{start}, otherwise equal to \code{n = 2}, where the biomass at MSY
#' is half of carrying capacity.
#' @param fix_sigma Logical, whether the standard deviation of the index is fixed. If \code{TRUE},
#' sigma is fixed to value provided in \code{start} (if provided), otherwise, value based on \code{Data@@CV_Ind}.
#' @param fix_omega Logical, whether the standard deviation of the catch is fixed. If \code{TRUE},
#' omega is fixed to value provided in \code{start} (if provided), otherwise, value based on \code{Data@@CV_Cat}.
#' @param fix_alpha Logical, whether the ratio of index and biomass standard deviations is fixed. If \code{TRUE},
#' alpha is fixed to value provided in \code{start} (if provided), otherwise, equal to 1.
#' @param fix_beta Logical, whether the ratio of catch and F standard deviations is fixed. If \code{TRUE},
#' tau is fixed to value provided in \code{start} (if provided), otherwise, equal to 1.
#' @param n_seas Integer, the number of sub-annual time steps in the model, i.e. the inverse of the Euler time step. Defaults to 4 (seasonal time steps).
#' @param ... Additional arguments (not currently used).
#' @details
#' To provide starting values, a named list can be provided for \code{MSY}, \code{K}, \code{n}, \code{omega}, and
#' \code{sigma} via the \code{start} argument. Otherwise, the defaults from the spict package are used.
#' @return An object of \code{\linkS4class{Assessment}} containing assessment output.
#' @note
#' This is a wrapper function intended for a DLMtool \linkS4class{Data} object. The \code{spict} package can be
#' downloaded from Github with \code{devtools::install_github("mawp/spict/spict")}.
#'
#' The full spict model also accommodates time-varying reference points and seasonal data among other
#' things, but these features are currently not used here. Additional diagnostics and plotting functions are also available in the spict package.
#' @author Q. Huynh
#' @references
#' Pedersen, M. W. and Berg, C. W. 2017. A stochastic surplus production model in continuous time. Fish and Fisheries. 18:226-243.
#'
#' @section Required Data:
#' \itemize{
#' \item \code{spict}: Cat, Ind
#' }
#' @section Optional Data:
#' \code{spict}: CV_Cat, CV_Ind
#' @examples
#' \donttest{
#' library(openMSE)
#' data(swordfish)
#'
#' res <- spict(Data = swordfish)
#' plot(res)
#' summary(res)
#'
#' ## Use additional functions spict functions
#' spict_output <- res@@info
#' library(spict)
#' summary(spict_output)
#' plot(spict_output)
#' spict_retro <- retro(spict_output)
#' plotspict.retro(spict_retro)
#' }
#' @import SAMtool
#' @importClassesFrom SAMtool Assessment
#' @importMethodsFrom SAMtool summary plot
#' @seealso \link[SAMtool]{SP_production} \link[SAMtool]{plot.Assessment} \link[SAMtool]{summary.Assessment} \link[SAMtool]{make_MP}
#' @export spict SPiCT
spict <- SPiCT <- function(x = 1, Data, start = NULL, fix_dep = TRUE, fix_n = TRUE, fix_sigma = FALSE,
                           fix_omega = FALSE, fix_alpha = TRUE, fix_beta = TRUE, n_seas = 4L, ...) {
  check_dependencies("spict", "spict")
  dependencies = "Data@Cat, Data@Ind"
  dots <- list(...)
  start <- lapply(start, eval, envir = environment())
  if(any(names(dots) == "yind")) {
    yind <- eval(dots$yind)
  } else {
    ystart <- which(!is.na(Data@Cat[x, ]))[1]
    yind <- ystart:length(Data@Cat[x, ])
  }
  Year <- Data@Year[yind]
  C_hist <- Data@Cat[x, yind]
  #if(any(is.na(C_hist))) stop('Model is conditioned on complete catch time series, but there is missing catch.')
  I_hist <- Data@Ind[x, yind]
  I_hist[I_hist < 0] <- NA
  ny <- length(C_hist)

  inp <- spict::check.inp(list(obsC = C_hist, timeC = Year, obsI = I_hist[!is.na(I_hist)], timeI = Year[!is.na(I_hist)],
                               dteuler = 1/n_seas))
  inp$getReportCovariance <- inp$getJointPrecision <- FALSE

  if(!is.null(start$MSY) && is.numeric(start$MSY)) inp$ini$logm <- log(start$MSY[1])
  if(!is.null(start$K) && is.numeric(start$K)) inp$ini$logK <- log(start$K[1])
  if(!is.null(start$n) && is.numeric(start$n)) inp$ini$log_n <- log(start$n[1])
  if(!is.null(start$sigma) && is.numeric(start$sigma)) inp$ini$logsdi <- log(start$sigma[1])
  if(!is.null(start$omega) && is.numeric(start$omega)) inp$ini$logsdc <- log(start$omega[1])

  if(fix_dep) {
    val <- ifelse(!is.null(start$dep), start$dep[1], 1)
    inp$priors$logbkfrac <- c(log(val), 1e-2, 1)
  }
  if(fix_n) {
    val <- ifelse(!is.null(start$n), start$n[1], 2)
    inp$priors$logn <- c(log(val), 1e-2, 1)
  } else {
    inp$priors$logn[3] <- 0
  }
  if(fix_sigma) {
    val <- ifelse(!is.null(start$sigma), start$sigma[1], sdconv(1, Data@CV_Ind[x]))
    val <- max(c(0.01, val))
    inp$priors$logsdi <- list(c(log(val), 1e-2, 1))
  }
  if(fix_omega) {
    val <- ifelse(!is.null(start$omega), start$omega[1], sdconv(1, Data@CV_Cat[x]))
    val <- max(c(0.01, val))
    inp$priors$logsdc <- c(log(val), 1e-2, 1)
  }
  if(fix_alpha) {
    val <- ifelse(!is.null(start$alpha), start$alpha[1], 1)
    inp$priors$logalpha <- c(log(val), 1e-2, 1)
  }
  if(fix_beta) {
    val <- ifelse(!is.null(start$beta), start$beta[1], 1)
    inp$priors$logbeta <- c(log(val), 1e-2, 1)
  }

  res <- try(spict::fit.spict(inp), silent = TRUE)
  if(is.character(res)) {
    return(new("Assessment", Model = "spict", conv = FALSE))
  }

  obj <- res$obj
  opt <- res$opt
  if(is.null(res$sderr)) SD <- structure(res[1:9], class = "sdreport")
  report <- obj$report(obj$env$last.par.best)

  season_1_ind <- which((1:(ny * n_seas)-1) %% n_seas == 0)
  Yearplusone <- c(Year, max(Year) + 1)
  plus_one_ind <- c(season_1_ind, ny * n_seas + 1)

  K <- exp(res$par.fixed[names(res$par.fixed) == "logK"])
  B1 <- exp(SD$par.random[names(SD$par.random)=='logB'][season_1_ind])
  B2 <- exp(SD$par.random[names(SD$par.random)=='logB'][plus_one_ind])
  Ipred <- exp(res$par.fixed[names(res$par.fixed) == "logq"]) * B1

  FMort <- exp(SD$par.random[names(SD$par.random)=='logF'][season_1_ind])

  Assessment <- new("Assessment", Model = "spict", Name = Data@Name, conv = is.null(res$sderr) && SD$pdHess,
                    FMSY = report$Fmsy, MSY = report$MSY, BMSY = report$Bmsy, VBMSY = report$Bmsy, SSBMSY = report$Bmsy,
                    B0 = K, VB0 = K, SSB0 = K,
                    FMort = structure(FMort, names = Year),
                    F_FMSY = structure(FMort/report$Fmsy, names = Year),
                    B = structure(B2, names = Yearplusone),
                    B_BMSY = structure(B2/report$Bmsy, names = Yearplusone),
                    B_B0 = structure(B2/K, names = Yearplusone),
                    VB = structure(B2, names = Yearplusone),
                    VB_VBMSY = structure(B2/report$Bmsy, names = Yearplusone),
                    VB_VB0 = structure(B2/K, names = Yearplusone),
                    SSB = structure(B2, names = Yearplusone),
                    SSB_SSBMSY = structure(B2/report$Bmsy, names = Yearplusone),
                    SSB_SSB0 = structure(B2/K, names = Yearplusone),
                    Obs_Catch = structure(C_hist, names = Year), Obs_Index = structure(I_hist, names = Year),
                    Index = structure(Ipred, names = Year),
                    NLL = ifelse(is.list(opt), opt$objective, NA),
                    info = res, obj = obj, opt = opt, TMB_report = report,
                    dependencies = dependencies)

  if(Assessment@conv) {

    if(Assessment@MSY < 0 || Assessment@FMSY < 0 || Assessment@BMSY < 0) {
      Assessment@MSY <- SD$value["MSYd"]
      Assessment@FMSY <- SD$value["Fmsyd"]
      Assessment@BMSY <- SD$value["Bmsyd"]

      Assessment@F_FMSY <- Assessment@FMort/Assessment@F_FMSY
      Assessment@B_BMSY <- Assessment@VB_VBMSY <- Assessment@SSB_SSBMSY <- Assessment@B/Assessment@BMSY
    }

    Assessment@SE_FMSY <- SD$sd[names(SD$value) == "Fmsy"]
    Assessment@SE_MSY <- SD$sd[names(SD$value) == "MSY"]

    # Assessment@SE_F_FMSY_final <- delta_log(SD$value[names(SD$value) == "logFFmsy"][ny*n_seas-n_seas+1],
                                            # SD$sd[names(SD$value) == "logFFmsy"][ny*n_seas-n_seas+1])
    # Assessment@SE_B_BMSY_final <- Assessment@SE_VB_VBMSY_final <-
      # delta_log(SD$value[names(SD$value) == "logBBmsy"][ny*n_seas+1], SD$sd[names(SD$value) == "logBBmsy"][ny*n_seas+1])
  }
  if(is.null(res$sderr)) {
    Assessment@SD <- SD
    Cpred <- spict::get.par("logCpred", res, exp = TRUE)[1:ny, 2] # NULL if errored
    Assessment@Catch <- structure(Cpred, names = Year)
  }

  return(Assessment)
}
class(spict) <- class(SPiCT) <- "Assess"

delta_log <- function(mu, std) exp(mu) * std
