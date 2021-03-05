#' function to plot a variogram in the correct color of athe sample
#' @export
plot_svfs_single <- function(variogram, colors = clr_set_base){
  color <- colors[[attributes(variogram)$info$identity]]
  level <- c(0.95) # 50% and 95% CIs
  plot(variogram, fraction = .5, level = level, col = color)
}

#' function to create a variogram plot pair (initial close up and total data range)
#' @export
plot_svfs <- function(variogram, colors = clr_set_base){
  color <- colors[[attributes(variogram)$info$identity]]
  level <- c(0.5, 0.95) # 50% and 95% CIs
  xlim <- c(0, 36 %#% "hour") # 0-12 hour window
  plot(variogram, xlim = xlim, level = level, col = color)
  plot(variogram, fraction = 1, level = level, col = color)
}

#' function to compare variogram with a model fit (initial close up and total data range)
#' @export
plot_svf_vs_model <- function(variogram, mod,  colors = clr_set_base, ...){
  color <- colors[[attributes(variogram)$info$identity]]
  xlim <- c(0, 12 %#% "hour") # 0-12 hour window
  plot(variogram, CTMM = mod , col = color, col.CTMM = "black", xlim = xlim)
  plot(variogram, CTMM = mod , col = color, col.CTMM = "black", fraction = 1)
}

#' manipulate ctmm model tau entry
#' @export
update_tau <- function(mod, tauH, tauF, ...){
  mod$tau[[1]] <- tauH
  mod$tau[[2]] <- tauF
  mod
}

#' manipulate ctmm model and output new model alongside original for comparison plot
#' @export
update_model <- function(mod, update_fun, ...){
  new_mod <- update_fun(mod = mod, ...)
  list(mod, new_mod)
}

#' plot two model (original guess and modified) in context of variogram (in right colors)
#' @export
plot_ctm <- function(mod, svf, ...){
  ctmm:::plot.variogram(x = svf, CTMM = mod, col = clr_set_base[attributes(svf)$info$identity], ...)
}

#' function to compare variogram with all ML model fits (initial close up and total data range)
#' @export
compare_all_models <- function(id, svfs, fitted_mods, ...){
  id_idx <- which(fitted_mods$sample_id == id)

  n_mod <-  length(fitted_mods$all_models[[id_idx]])
  if(n_mod > 2){
    clr_mod <- RColorBrewer::brewer.pal(n = n_mod, name = "Greys") %>%
      clr_alpha(alpha = .7)} else {
        clr_mod <- c("black", "darkgray")
      }

  plot(svfs[[id]], fitted_mods$all_models[[id_idx]] %>% rev(),
       col = clr_set_base[[id]], col.CTMM = clr_mod,  fraction = .02)
  plot(svfs[[id]], fitted_mods$all_models[[id_idx]] %>% rev(),
       col = clr_set_base[[id]], col.CTMM = clr_mod, fraction =1)
}

#' function to compare variogram with the n best ML model fits (initial close up and total data range)
#' @export
compare_n_best_models <- function(id, svfs, n_mod_max = 5,  fitted_mods, ...){
  id_idx <- which(fitted_mods$sample_id == id)

  n_mod <-  min(length(fitted_mods$all_models[[id_idx]]),n_mod_max)
  if(n_mod > 2){
    clr_mod <- RColorBrewer::brewer.pal(n = n_mod, name = "Greys") %>%
      clr_alpha(alpha = .7)} else {
        clr_mod <- c("black", "darkgray")
      }

  plot(svfs[[id]], fitted_mods$all_models[[id_idx]][1:n_mod] %>% rev(),
       col = clr_set_base[[id]], col.CTMM = clr_mod,  fraction = .02)
  plot(svfs[[id]], fitted_mods$all_models[[id_idx]][1:n_mod] %>% rev(),
       col = clr_set_base[[id]], col.CTMM = clr_mod, fraction =1)
}

# /// --------------------------------------------------------------

#' functions to parse estimated model parameters (mu & tau) from ctmm
#'
#'  get the estimate units and round estimate to two digits
#' @export
parse_cis <- function(mod, ci_type){
  ci_type_idx <- c(low = 1, est = 2, high = 3)[[ci_type]]
  cis <- summary(mod)$CI[, ci_type_idx]
  cis_units <- str_remove_all(pattern = ".*\\(|\\)", names(cis)) %>%
    str_replace(., "square kilometers", "km^2") %>%
    str_remove_all(., "ours|inutes|ilo|eters|ays|ay|ect|res|ili|econds")

  cis %>% set_names(cis_units)
}

#' universal function (works for both OU & OUF)
#' @export
get_ci_univ <- function(mod, ci_idx, ci_type = "est",  ...){
  cis <- parse_cis(mod, ci_type)
  str_c(cis[[ci_idx]] %>% round(.,2),"_", names(cis)[[ci_idx]])
}

#' function for OUF specific parameters (tau2)
#' @export
get_ci_ouf <- function(mod, ci_idx, ci_type = "est", ...){
  mod_type <- summary(mod)$name
  if(mod_type == "OUF anisotropic"){
    cis <- parse_cis(mod, ci_type)
    str_c(cis[[ci_idx]] %>% round(.,2),"_", names(cis)[[ci_idx]])
  } else { NA_character_ }
}

#' wrapper to get sigma, tauH, tauF, and speed estimates of a model
#' @export
mod_tab <- function(mod){
  tibble( est_sig = get_ci_univ(mod = mod, ci_idx = 1),
          est_tau_pos = get_ci_univ(mod = mod, ci_idx = 2),
          est_tau_vel = get_ci_ouf(mod = mod, ci_idx = 3),
          est_speed = get_ci_ouf(mod = mod, ci_idx = 4))
}

#' separate  ctmm model estimates from units
#' @export
separate_mod_tab <- function(tab){
  tab %>%
    separate(est_sig, into = c("est_σ", "σ_unit"), sep = "_", convert = TRUE) %>%
    separate(est_tau_pos, into = c("est_τ_pos", "τp_unit"), sep = "_", convert = TRUE) %>%
    separate(est_tau_vel, into = c("est_τ_vel", "τv_unit"), sep = "_", convert = TRUE) %>%
    separate(est_speed, into = c("est_speed", "speed_unit"), sep = "_", convert = TRUE)
}

# / ----------------------------------------------------------------

#' function to create and fit a Ornstein-Uhlenbeck-F model to a semi-variance function
#' @export
fit_ouf <- function(svf, sigma, tauH, tauF){
  m.ouf <- ctmm(sigma = sigma %#% "ha",tau = c(tauH %#% "hour", tauF %#% "min"))
  plot_svf_vs_model(svf, m.ouf)
  m.ouf
}

#' function for automatizing autocorrelated kernel density estimation
#' @export
get_akde <- function(data, fitted_mls, id, ...){
  cat(paste0(id,"\n"))
  akde(data[[id]], fitted_mls$best_model[fitted_mls$sample_id == id])
}

#' convert SI units (m2 -> ha)
#' @export
m2_to_ha <- function(x){x / (100*100)}


#' convert SI units (s -> day)
#' @export
s_to_day <- function(x){x / (60*60*24)}

#' convert a svf to a tibble
#' @export
svf_to_tibble <- function(svf, ci_level = .05){
  svf %>%
    as_tibble() %>%
    mutate(id =  attributes(svf)$info$identity,
           ci_lower = ctmm:::CI.lower(DOF, Alpha = ci_level) * SVF,
           ci_upper = ctmm:::CI.upper(DOF, Alpha = ci_level) * SVF)
}

#' modified plot.variogram from {ctmm}
#'
#' The function was altered from ctmm:::plot.variogram()
#'
#' It now returns the internally computed SVF (by using plot_svf - the altered version of ctmm:::plot.svf())
#'
#' @export
plot_variogram <- function(x, CTMM = NULL, level = 0.95, units = TRUE, fraction = 0.5,
                           col = "black", col.CTMM = "red", xlim = NULL, ylim = NULL, ext = NULL, ...)
{
  alpha <- 1-level

  if(!is.null(ext))
  {
    xlim <- ext$x
    ylim <- ext$y
  }

  # empirical variograms
  x <- ctmm:::listify(x)
  n <- length(x)
  # theoretical models
  CTMM <- ctmm:::listify(CTMM)
  m <- length(CTMM)

  # default single comparison model
  # if(is.null(CTMM) && n==1 && !is.null(attr(x[[1]],"info")$CTMM)) { CTMM <- attr(x[[1]],"info")$CTMM }
  ACF <- !is.null(attr(x[[1]],"info")$ACF)

  # don't plot if DOF<1
  x <- lapply(x,function(y){ y[y$DOF>=1,] })

  # subset the data if xlim or fraction provided
  if(!is.null(xlim))
  {
    fraction <- 1 # xlim overrides fraction
    x <- lapply(x,function(y){ y[xlim[1]<=y$lag & y$lag<=xlim[2],] })
  }
  else
  {
    # maximum lag in data
    max.lag <- sapply(x, function(v){ last(v$lag) } )
    max.lag <- max(max.lag,xlim)
    # subset fraction of data
    max.lag <- fraction*max.lag

    # subset all data to fraction of total period
    if(fraction<1) { x <- lapply(x, function(y) { y[y$lag<=max.lag,] }) }

    xlim <- c(0,max.lag)
  }

  # clamp ACF estimates before calculating extent
  if(ACF) { x <- lapply(x,function(y) { y$SVF <- clamp(y$SVF,-1,1) ; y }) }

  # calculate ylimits from all variograms !!!
  if(is.null(ylim)) { ylim <- extent(x,level=max(level))$y }

  if(!ACF) # SVF plot
  {
    # choose SVF units
    SVF.scale <- ctmm:::unit(ylim,"area",SI=!units)
    SVF.name <- SVF.scale$name
    SVF.scale <- SVF.scale$scale

    SVF.name <- c(SVF.name,ctmm:::unit(ylim,"area",concise=TRUE,SI=!units)$name)
    SVF.name[3] <- SVF.name[2]

    ylab <- "Semi-variance"
    ylab <- c(ylab,ylab,"SVF")

    # range of possible ylabs with decreasing size
    ylab <- paste(ylab, " (", SVF.name, ")", sep="")

  }
  else # ACF plot
  {
    SVF.scale <- 1
    ylab <- "Autocorrelation"
    ylab <- c(ylab,ylab,"ACF")
  }

  # choose lag units
  lag.scale <- ctmm:::unit(xlim,"time",2,SI=!units)
  lag.name <- lag.scale$name
  lag.scale <- lag.scale$scale

  lag.name <- c(lag.name,ctmm:::unit(xlim,"time",thresh=2,concise=TRUE,SI=!units)$name)
  lag.name[3] <- lag.name[2]

  xlab <- "Time-lag"
  xlab <- c(xlab,xlab,"Lag")

  xlab <- paste(xlab, " (", lag.name, ")", sep="")

  # choose appropriately sized axis labels for base plot
  lab <- rbind(xlab,ylab)

  # string width max
  max.cex.w <- lab # copy dimensions and preserve below
  max.cex.w[] <- graphics::par('pin')/graphics::strwidth(lab,'inches')
  # string height max
  max.cex.h <- lab
  max.cex.h[] <- (graphics::par('mai')[1:2]/graphics::par('mar')[1:2])/graphics::strheight(lab,'inches')

  # min of x & y
  max.cex.w <- pmin(max.cex.w[1,],max.cex.w[2,])
  max.cex.h <- pmin(max.cex.h[1,],max.cex.h[2,])
  # min of width and height
  max.cex <- pmin(max.cex.w,max.cex.h)

  lab <- 1
  if(max.cex[lab]<1) { lab <- lab + 1 }
  if(max.cex[lab]<1) { lab <- lab + 1 }

  # unit convert scales if supplied
  xlim <- xlim/lag.scale
  ylim <- ylim/SVF.scale

  # fix base plot layer
  # plot(xlim,ylim, xlim=xlim, ylim=ylim, xlab=xlab[lab], ylab=ylab[lab], col=grDevices::rgb(1,1,1,0), ...)

  # color array for plots
  col <- array(col,n)
  # color array for plots
  col.CTMM <- array(col.CTMM,m)

  # units conversions
  x <- lapply(x,function(X){ctmm:::unit.variogram(X,time=lag.scale,area=SVF.scale)})
  CTMM <- lapply(CTMM,function(M){ctmm:::unit.ctmm(M,length=sqrt(SVF.scale),time=lag.scale)})

  for(i in 1:n)
  {
    SVF <- x[[i]]$SVF
    if(!ACF)
    {
      if("MSE" %in% names(x[[i]])) # calibrated errors
      { MSE <- x[[i]]$MSE }
      else # uncalibrated errors - needs fitted error in model
      { MSE <- x[[i]]$MSDOP }
    }
    lag <- x[[i]]$lag
    DOF <- x[[i]]$DOF

    # make sure plot looks nice and appropriate for data resolution
    if(length(lag) < 100) { type <- "p" } else { type <- "l" }

    # graphics::points(lag, SVF, type=type, col=col[[i]])

    for(j in 1:length(alpha))
    {
      # chi-square CIs for semi-variance
      if(!ACF)
      {
        LOW <- ctmm:::CI.lower(DOF,alpha[j])
        HIGH <- ctmm:::CI.upper(DOF,alpha[j])

        SVF.lower <- SVF * LOW
        SVF.upper <- SVF * HIGH
      }
      else # Fisher CIs for autocorrelation
      {
        # subset relevant data for Fisher transformation
        STUFF <- data.frame(lag=lag,SVF=SVF,DOF=DOF)
        STUFF <- STUFF[DOF>3,]
        lag <- STUFF$lag
        SVF <- STUFF$SVF
        DOF <- STUFF$DOF
        # Fisher transformation
        FISH <- atanh(SVF)
        SD <- 1/sqrt(max(DOF-3,0))
        SVF.lower <- tanh(stats::qnorm(alpha[j]/2,mean=FISH,sd=SD,lower.tail=TRUE))
        SVF.upper <- tanh(stats::qnorm(alpha[j]/2,mean=FISH,sd=SD,lower.tail=FALSE))

        # graphics::abline(h=c(-1,0,1)/sqrt(DOF[1])*stats::qnorm(1-alpha[j]/2),col="red",lty=c(2,1,2))
      }

      # graphics::polygon(c(lag,rev(lag)),c(SVF.lower,rev(SVF.upper)),col=ctmm:::malpha(col[[i]],alpha=0.1),border=NA)
    }

    # PLOT CORRESPONDING MODEL
    if(i<=m) { ret_svf <- plot_svf(lag,CTMM[[i]],error=MSE,alpha=alpha,type=type,col="blue") }
  }
  # PLOT LEFTOVER MODELS USING THE LAST DATA
  if(n<m) { for(i in n:m) { ctmm:::plot.svf(lag,CTMM[[i]],error=MSE,alpha=alpha,type=type,col=col.CTMM[[i]]) } }

  return(ret_svf)
}

# ==============================================================================

#' modified plot.svf from {ctmm}
#'
#' The function was altered from ctmm:::plot.svf()
#'
#' It now returns the internally computed SVF
#'
#' @export
plot_svf <- function(lag,CTMM,error=NULL,alpha=0.05,col="red",type="l",...)
{
  # changed from max lag to all lags
  # changed from error=0 or number/logical to error=NULL or array

  # number of pixels across diagonal of display
  PX <- ceiling(sqrt(sum(grDevices::dev.size("px")^2)))

  # are we including errors?
  ERROR <- !is.null(error) && CTMM$error
  e0 <- 0
  # can we plot a smooth curve?
  if(!ERROR)
  {
    lag <- seq(0,last(lag),length.out=PX)
    error <- 0 -> e0
  }
  else if(all(diff(error[-1])==0))
  { error <- error[2] -> e0 } # can still plot curve because all errors it the same

  SVF <- ctmm:::svf.func(CTMM,moment=TRUE)
  svf <- SVF$svf
  DOF <- SVF$DOF

  # point estimate plot
  SVF <- Vectorize(function(t,error=e0) { svf(t,error=error) })

  lag[1] <- lag[2]/1000 # almost go to origin, but not quite to avoid nugget
  #
  # if(length(error)==1) # can plot curve
  # { graphics::curve(SVF,from=0,to=last(lag),n=PX,add=TRUE,col=col,...) }
  # else
  # { graphics::points(lag,SVF(lag,error),col=col,type=type,...) }
  #
  return(SVF)

}

#' Extract a fitted parameter from a ctmm object
#' @export
get_var_fit_raw <- function(vario, model = "ou"){
  var_f <- variogram.fit(variogram = vario,interactive = FALSE)

  switch(model,
         "ou" = ctmm(sigma = var_f$sigma[[1]] %#% "m^2",tau = var_f$tau[[1]] %#% "sec"),
         "ouf" = ctmm(sigma = var_f$sigma[[1]] %#% "m^2",tau = c(var_f$tau[[1]] %#% "sec", var_f$tau[[2]] %#% "sec"))
  )
}

#' Extract a OU fitted parameter from a ctmm object
#' @export
get_var_fit_ou <- function(vario, xlim = c(0, 50), n = 200){
  var_f <- variogram.fit(variogram = vario,interactive = FALSE)
  m.ou <- ctmm(sigma = var_f$sigma[[1]] %#% "m^2",tau = var_f$tau[[1]] %#% "sec")
  var_fun <- plot_variogram(vario, CTMM=m.ou, col.CTMM="red")

  tibble(x = seq(xlim[[1]],xlim[[2]],length.out = n), y = var_fun(x)) %>%
    mutate(id = attributes(vario)$info$identity,
           sigma = var_f$sigma[[1]] ,
           tau = var_f$tau[[1]])
}

#' Extract a OUF fitted parameter from a ctmm object
#' @export
get_var_fit_ouf <- function(vario, xlim = c(0, 50), n = 200){
  var_f <- variogram.fit(variogram = vario,interactive = FALSE)
  m.ou <- ctmm(sigma = var_f$sigma[[1]] %#% "m^2",tau = c(var_f$tau[[1]] %#% "sec", var_f$tau[[2]] %#% "sec"))
  var_fun <- plot_variogram(vario, CTMM=m.ou, col.CTMM="red")

  tibble(x = seq(xlim[[1]],xlim[[2]],length.out = n), y = var_fun(x)) %>%
    mutate(id = attributes(vario)$info$identity,
           sigma = var_f$sigma[[1]] ,
           tau = var_f$tau[[1]])
}

#' Get the parameter estimates from a ctmm fitted model
#' @export
get_param_estimate <- function(mod){
  summary(mod)$CI %>%
    as.data.frame() %>%
    mutate(param = row.names(.)) %>%
    select(param, est)
}

#' Load all ctmm fitted models
#' @export
read_all_ml_fits <- function(files){
  files %>%
    purrr::map_dfr(readRDS) %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(all_models = list(fitted_ml),
                     model_summary = list(summary(fitted_ml)),
                     best_model = list(fitted_ml[[1]]),
                     best_model_name = summary(fitted_ml[[1]])$name,
                     best_bodel_summary = list(get_param_estimate(fitted_ml[[1]]) %>% mutate(sample_id = sample_id[[1]])))
}

#' Load ctmm based AKDEs by Sample ID
#' @export
get_akde_maual <- function(data, fitted_mls, id, ...){
  cat(paste0(id, "\n"))

  tibble(sample_id = id,
         akde = list(akde(data[[id]],
                          fitted_mls$best_model[fitted_mls$sample_id == id])))
}

#' Load ctmm based AKDEs grouped by field season
#' @export
get_akde_aligned <- function(data, fitted_mls, year, ...){
  cat(paste0(year, "\n"))

  ctmms <- fitted_mls$best_model %>% set_names(nm = fitted_mls$sample_id)
  sample_id_year <- fitted_mls$sample_id[fitted_mls$sample_id %in% meta$id[meta$year == year]]

  tibble(year = year,
         akde = akde(data[sample_id_year], ctmms[sample_id_year]))
}

#' Load ctmm based kriged occurrences by Sample ID
#' @export
get_kriged_occurrence_maual <- function (data, fitted_mls, id, ...) {
  cat(paste0(id, "\n"))
  tibble(sample_id = id,
         occurrence = list(occurrence(data = data[[id]],
                                      CTMM = fitted_mls$best_model[fitted_mls$sample_id == id][[1]])))
}
