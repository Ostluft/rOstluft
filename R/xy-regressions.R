#' extract linear equation parameters from model
#'
#' @keywords internal
get_coef <- function(model) {
  if ("MCResultResampling" %in% class(model)) {
    slope <- model@para[2,1]
    intercept <- model@para[1,1]
  } else {
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
  }
  return(list(slope = slope, intercept = intercept))
}


#' make an equation from model
#'
#' @keywords internal
eqn <- function(linearmodel = NULL, expr = TRUE, digits = 2){
  cf <- get_coef(linearmodel)
  if (expr) {
    eq <- substitute(italic(y) == a + b %.% italic(x),#*","~~italic(r)^2~"="~r2,
                     list(a = format(round(cf$intercept,digits), digits = digits), b = format(round(cf$slope,digits), digits = digits)))#, r2 = format(summary(linearmodel)$r.squared, digits = 3)))
  } else {
    eq <- paste0("y = ", format(round(cf$slope,digits), nsmall = digits),"x ", ifelse(cf$intercept < 0, "- ", "+ "), format(abs(round(cf$intercept,digits)), nsmall = digits))
  }
  return(as.character(as.expression(eq)))
}


#' perform linear regression
#'
#' @keywords internal
Rregr <- function(df, fmla = "y ~ x", model = "rlm", group = NA, ...) { # model can be: lm, rlm, deming, weighted deming, bablock
  # code noch verbessern (dplyr etc) ...
  df2 <- as.data.frame(df)
  df2[,as.character(as.formula(fmla))[3]] <- ifelse(as.data.frame(df)[,as.character(as.formula(fmla))[3]] < 0, NA, as.data.frame(df)[,as.character(as.formula(fmla))[3]])
  df2[,as.character(as.formula(fmla))[2]] <- ifelse(as.data.frame(df)[,as.character(as.formula(fmla))[2]] < 0, NA, as.data.frame(df)[,as.character(as.formula(fmla))[2]])
  if (!is.na(group) & group %in% names(df)) {
    m <- lapply(unique(pull(df, group)), function(x) {
      if (model %in% c("Deming", "PaBa", "PaBaLarge")) {
        mod <- mcreg(x = as.data.frame(df)[df[,group] == x, as.character(as.formula(fmla))[3]], y = as.data.frame(df)[df[,group] == x,as.character(as.formula(fmla))[2]], method.reg = model, na.rm = TRUE)
      }
      if (model == "WDeming") {
        mod <- mcreg(x = df2[df[,group] == x,as.character(as.formula(fmla))[3]], y = df2[df[,group] == x,as.character(as.formula(fmla))[2]], method.reg = model, na.rm = TRUE)
      }
      if (model %in% c("lm","glm","gam","loess","rlm")) {
        mod <- do.call(model, args = list(formula = as.formula(fmla), data = df[df[,group] == x,], ...))
      }
      return(mod)
    })
  } else {
    if (model %in% c("Deming", "PaBa")) {
      m <- mcreg(x = as.data.frame(df)[,as.character(as.formula(fmla))[3]], y = as.data.frame(df)[,as.character(as.formula(fmla))[2]], method.reg = model, na.rm = TRUE)
    }
    if (model == "WDeming") {
      m <- mcreg(x = df2[,as.character(as.formula(fmla))[3]], y = df2[,as.character(as.formula(fmla))[2]], method.reg = model, na.rm = TRUE)
    }
    if (model %in% c("lm","glm","gam","loess","rlm")) {
      m <- do.call(model, args = list(formula = as.formula(fmla), data = df, ...))
    }
    m <- list(m)
  }
  return(m)
}


#' derives confidence intervals for regression models
#'
#' @keywords internal
reg_conf_interval <- function(model, alpha = 0.95, xn = 100) {
  if ("MCResultResampling" %in% class(model)) {
    rx <- range(model@data[,"x"], na.rm = TRUE)
  } else {
    rx <- range(model$model[,2], na.rm = TRUE)
  }
  xd <- seq(rx[1], rx[2], length.out = xn)
  if ("MCResultResampling" %in% class(model)) {
    ci <- calcResponse(model, alpha = 1-alpha, x.levels = xd)
    lci <- ci[,"Y.LCI"]
    uci <- ci[,"Y.UCI"]
    x <- xd#ci[,"X"]
  } else {
    nd <- data.frame(x = xd); names(nd) <- names(model$model)[2]
    ci <- predict(model, newdata = nd, interval = "confidence", level = alpha)
    lci <- ci[,"lwr"]
    uci <- ci[,"upr"]
    x <- xd
  }
  return(data.frame(x = x, lci = lci, uci = uci))
}


#' derives prognosis intervals for regression models
#'
#' @keywords internal
reg_prog_interval <- function(model, xn = 100, alpha = 0.975) {
  if ("MCResultResampling" %in% class(model)) {
    rx <- range(model@data[,"x"], na.rm = TRUE)
  } else {
    rx <- range(model$model[,2], na.rm = TRUE)
  }
  xd <- seq(rx[1], rx[2], length.out = xn)
  if ("MCResultResampling" %in% class(model)) {
    SSx <- sum((model@data$x[!is.na(model@data$x) & !is.na(model@data$y)] - model@xmean)^2)
    R <- model@data$y[!is.na(model@data$x) & !is.na(model@data$y)] - calcResponse(model, alpha = 1-alpha, x.levels = model@data$x[!is.na(model@data$x) & !is.na(model@data$y)])[,"Y"]
    sigma.hut <- sqrt((1/(length(model@data$x[!is.na(model@data$x) & !is.na(model@data$y)])-2))*sum(R^2))
    a <- qt(alpha, length(model@data$x[!is.na(model@data$x) & !is.na(model@data$y)])-2)
    # sqrt(1/length(model@data$x[!is.na(model@data$x)]) + ((xd - model@xmean)^2/SSx))
    b <- sqrt(1 + (1/length(model@data$x[!is.na(model@data$x) & !is.na(model@data$y)])) + ((xd - model@xmean)^2/SSx))
    pi <- calcResponse(model, alpha = 1-alpha, x.levels = xd)
    lpi <- pi[,"Y"] - sigma.hut * a * b #pi[,"Y.LCI"]
    upi <- pi[,"Y"] + sigma.hut * a * b #pi[,"Y.UCI"]
    x <- xd
  } else {
    nd <- data.frame(x = xd); names(nd) <- names(model$model)[2]
    pi <- predict(model, newdata = nd, interval = "prediction")
    lpi <- pi[,"lwr"]
    upi <- pi[,"upr"]
    x <- xd
  }
  return(data.frame(x = x, lpi = lpi, upi = upi))
}


#' perform various types of regressions between two parameters
#'
#' Regressions are based on packages 'stats', 'MASS', 'mcr' and include the following methods:
#' 'stats::lm','stats::glm','gam::gam','stats::loess','MASS:rlm', mcr::mcreg(..., method.reg %in% c('Deming', 'WDeming', 'PaBa')).
#' For the chosen method, a ggplot2 object as well as the regression model, equations, confidence and prognosis intervals are returned.
#'
#' ... code has to be enhanced, eventually
#'
#' @param df input data.frame
#' @param fmla string; supplied to as.formula(), e.g. 'y ~ x'
#' @param model string; specifies regression model, one of c('lm', 'glm', 'gam', 'loess', 'rlm', 'Deming', 'WDeming', 'PaBa')
#' @param group string, optional => can be NA; name for a grouping column
#' @param eqx numeric, optional => can be NA; explicit x-axis position of regression equation
#' @param eqy numeric, optional => can be NA; explicit y-axis position of regression equation
#' @param geom string; one of c('point', 'hex'): possiblity to plot y vs. x as ggplot2::geom_point() or ggplot2::geom_hex()
#' @param ptsize plotting style parameters ...
#' @param ptfill ...
#' @param ptcol ...
#' @param ptshp ...
#' @param lsize ...
#' @param nbins ...
#' @param txtsize ...
#' @param asp ...
#' @param facetscales scales argument for facet_wrap()
#' @param digits for equation display
#' @param plot.confint TRUE, FALSE
#' @param plot.progint TRUE, FALSE
#' @param ...; further parameters as input for geom_hex(...)
#'
#' @return named list; list(model, df, dfeq, plot, confint, progint)
#'
#' @keywords statistics, plotting
do_regr <- function(df, fmla = "y ~ x", model = "rlm", group = NA, eqx = NA, eqy = NA, geom = "point", ptsize = 1.5, ptfill = "lightblue",
                    ptcol = "blue", ptshp = 21, lsize = 0.75, nbins = NULL, txtsize = 4, asp = 1, facetscales = "free", digits = 2, plot.confint = FALSE, plot.progint = FALSE, ...) {
  m <- Rregr(df, fmla, model, group, ...)
  eq <- lapply(m, function(x) eqn(x, expr = ifelse(is.na(group), TRUE, FALSE), digits = digits))
  cf <- lapply(m, function(x) get_coef(x))
  ci <- lapply(m, function(x) reg_conf_interval(x))
  pi <- lapply(m, function(x) reg_prog_interval(x))
  if (!is.na(group) & group %in% names(df)) {
    dfeq <- data.frame(group = unique(df[,group]), equation = unlist(eq))
    names(dfeq)[1] <- group
    dfcf <- data.frame(group = unique(df[,group]), slope = sapply(cf, function(x) x$slope), intercept = sapply(cf, function(x) x$intercept))
    names(dfcf)[1] <- group
    dfci <- data.frame(group = unlist(lapply(unique(df[,group]), function(x) rep(x,nrow(ci[[1]])))), x = unlist(lapply(ci, function(x) x$x)), lci = unlist(lapply(ci, function(x) x$lci)), uci = unlist(lapply(ci, function(x) x$uci)))
    names(dfci)[1] <- group
    dfpi <- data.frame(group = unlist(lapply(unique(df[,group]), function(x) rep(x,nrow(pi[[1]])))), x = unlist(lapply(pi, function(x) x$x)), lpi = unlist(lapply(pi, function(x) x$lpi)), upi = unlist(lapply(pi, function(x) x$upi)))
    names(dfpi)[1] <- group
  } else {
    dfeq <- data.frame(group = NA, equation = unlist(eq))
    dfcf <- data.frame(group = NA, slope = unlist(cf)[1], intercept = unlist(cf)[2])
    dfci <- data.frame(group = NA, x = ci[[1]][,"x"], lci = ci[[1]][,"lci"], uci = ci[[1]][,"uci"])
    dfpi <- data.frame(group = NA, x = pi[[1]][,"x"], lpi = pi[[1]][,"lpi"], upi = pi[[1]][,"upi"])
  }
  x <- as.character(as.formula(fmla))[3]
  y <- as.character(as.formula(fmla))[2]
  p <- ggplot(df, aes_string(x = x, y = y))
  if (geom == "point") {p <- p + geom_point(col = ptcol, fill = ptfill, shape = ptshp, size = ptsize)}
  if (geom == "hex") {p <- p + geom_hex(...)}
  # p <- p + geom_text(data = dfeq, aes(x = ifelse(is.na(eqx), pretty(as.data.frame(df)[,x])[1], eqx), y = ifelse(is.na(eqy), pretty(as.data.frame(df)[,y])[6], eqy), label = equation), parse = TRUE, hjust = "inward", size = txtsize)
  if (!is.na(group) & group %in% names(df)) {p <- p + facet_wrap(group, labeller = as_labeller(setNames(paste0(dfeq[,1],"\n",dfeq$equation), dfeq[,1])), scales = facetscales)}
  if (plot.progint) {p <- p + geom_ribbon(data = dfpi, aes(x = x, ymin = lpi, ymax = upi), inherit.aes = FALSE, fill = "lightgray", alpha = 0.6)}
  if (plot.confint) {p <- p + geom_ribbon(data = dfci, aes(x = x, ymin = lci, ymax = uci), inherit.aes = FALSE, fill = "gray", alpha = 0.6)}
  p <- p +
    geom_abline(data = dfcf, aes(intercept = intercept, slope = slope), size = lsize) +
    theme_bw() +
    theme(strip.background = element_blank())
  return(list(model = m, df = df, dfeq = dfeq, plot = p, confint = dfci, progint = dfpi))
}




# ### Bsp:
# require(tidyverse)
# require(MASS)
# require(mcr)
# require(gam)
#
#
# df <- readRDS("I:/Dokumente/SiJ/Programmieren/Regressionen/NABEL_PM_2016.RDS")
#
#
# ### rlm over all data
# regr <- doLinRegr(df, fmla="PM10kont ~ PM10grav", model="rlm", plot.confint=TRUE, plot.progint=TRUE)
# regr$model
# regr$plot + geom_abline(slope=1, intercept=0, lty=2, size=0.75, col="gray40")
#
#
# ### rlm grouped by site with modified site labels
# unique(df$site)
# df$site <- factor(df$site, labels=list(DUE="D?bendorf", BAS="Basel", BER="Bern", LUG="Lugano", ZUE="Z?rich"))
# regr <- doLinRegr(df, fmla="PM10kont ~ PM10grav", model="rlm", group="site", plot.confint=TRUE, plot.progint=TRUE)
# regr$plot
#
#
# ### deming
# regr <- doLinRegr(df, fmla="PM10kont ~ PM10grav", model="Deming", group=NA, plot.confint=TRUE, plot.progint=TRUE)
# regr$plot
# regr$progint
# regr <- doLinRegr(df, fmla="PM10kont ~ PM10grav", model="Deming", group="site", plot.confint=TRUE, plot.progint=TRUE)
# regr$plot
#
#
# ### weighted deming = robuste Methode
# regr <- doLinRegr(df, fmla="PM10kont ~ PM10grav", model="WDeming", group="site")
# regr$plot
#
#
# ### passing bablock
# regr <- doLinRegr(df, fmla="PM10kont ~ PM10grav", model="PaBa", group="site")
# regr$plot


