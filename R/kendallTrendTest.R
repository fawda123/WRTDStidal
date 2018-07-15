######
#' Kendall trend test
#'
#' Nonparametric test for monotonic trend based on Kendall's Tau statistic
#' 
#' @param y an object containing data for the trend test. In the default method, the argument \code{y} must be numeric vector of observations. In the formula method, \code{y} must be a formula of the form \code{y ~ 1} or \code{y ~ x}. The form \code{y ~ 1} indicates use the observations in the vector \code{y} for the test for trend and use the default value of the argument \code{x} in the call to \code{kendallTrendTest.default}. The form \code{y ~ x} indicates use the observations in the vector \code{y} for the test for trend and use the specified value of the argument \code{x} in the call to \code{kendallTrendTest.default}. Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf, -Inf}) values are allowed but will be removed.
#' @param data specifies an optional data frame, list or environment (or object coercible by \code{as.data.frame} to a data frame) containing the variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which \code{kendallTrendTest} is called.
#' @param subset specifies an optional vector specifying a subset of observations to be used.
#' @param na.action specifies a function which indicates what should happen when the data contain \code{NAs}. The default is \code{\link[stats]{na.pass}}.
#' @param x numeric vector of "predictor" values. The length of \code{x} must equal the length of \code{y}. Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf, -Inf}) values are allowed but will be removed. The default value of \code{x} is the vector of numbers \code{1, 2, ..., n} where \code{n} is the number of elements in \code{y}.
#' @param alternative character string indicating the kind of alternative hypothesis. The possible values are \code{"two.sided"} (tau not equal to 0; the default), \code{"less"} (tau less than 0), and \code{"greater"} (tau greater than 0).
#' @param correct logical scalar indicating whether to use the correction for continuity in computing the z-statistic that is based on the test statistic S'. The default value is \code{TRUE}.
#' @param ci.slope logical scalar indicating whether to compute a confidence interval for the slope. The default value is \code{TRUE}.
#' @param conf.level numeric scalar between 0 and 1 indicating the confidence level associated with the confidence interval for the slope. The default value is \code{0.95}.
#' @param warn logical scalar indicating whether to print a warning message when \code{y} does not contain at least two non-missing values, or when \code{x} does not contain at least two unique non-missing values. The default value is \code{TRUE}.
#' @param data.name character string indicating the name of the data used for the trend test. The default value is \code{deparse(substitute(y))}.
#' @param data.name.x character string indicating the name of the data used for the predictor variable \code{x}. If \code{x} is not supplied this argument is ignored. When \code{x} is supplied, the default value is \code{deparse(substitute(x))}.
#' @param parent.of.data character string indicating the source of the data used for the trend test.
#' @param subset.expression character string indicating the expression used to subset the data.
#' @param ... methods passed to or from other methods
#' 
#' @export
#' 
#' @details 
#' 
#' \code{kendallTrendTest} performs Kendall's nonparametric test for a monotonic trend, which is a special case of the test for independence based on Kendall's tau statistic (see \code{\link[stats]{cor.test}}). The slope is estimated using the method of Theil (1950) and Sen (1968). When \code{ci.slope=TRUE}, the confidence interval for the slope is computed using Gilbert's (1987) Modification of the Theil/Sen Method.
#'
#' Kendall's test for a monotonic trend is a special case of the test for independence based on Kendall's tau statistic. The first section below explains the general case of testing for independence. The second section explains the special case of testing for monotonic trend. The last section explains how a simple linear regression model is a special case of a monotonic trend and how the slope may be estimated. 
#'
#' @return A list object with elements for results of the test
#' 
#' @references 
#' 
#' Hirsch, R.M., Slack, J.R., Smith, R.A. 1982. Techniques of trend analysis for monthly water quality data. Water Resources Research, 18:107-121.
#'  
#' Millard, S. P. 2013. EnvStats: An R Package for Environmental Statistics. Springer, New York. 
#' 
#' @examples
#' kendallTrendTest(res ~ dec_time, tidfitmean)
kendallTrendTest <- function (y, ...) UseMethod("kendallTrendTest")

#' @rdname kendallTrendTest
#'
#' @export
#'
#' @method kendallTrendTest default
kendallTrendTest.default <- function (y, x = seq(along = y), alternative = "two.sided", correct = TRUE, 
            ci.slope = TRUE, conf.level = 0.95, warn = TRUE, data.name = NULL, 
            data.name.x = NULL, parent.of.data = NULL, subset.expression = NULL, 
            ...){
    if (is.null(data.name)) 
      data.name <- deparse(substitute(y))
    y <- as.vector(unlist(y))
    if (!is.numeric(y)) 
      stop("All elements of 'y' must be a numeric")
    miss <- FALSE
    if (missing(x)) {
      data.name.x <- NULL
      if ((bad.obs <- sum(!(y.ok <- is.finite(y)))) > 0) {
        if (warn) {
          is.not.finite.warning(y)
          warning(paste(bad.obs, "observations with NA/NaN/Inf in 'y' removed."))
        }
        x <- x[y.ok]
        y <- y[y.ok]
      }
      n <- length(y)
      if (n < 2) {
        if (warn) 
          warning(paste("'y' does not contain at least two non-missing,", 
                        "finite observations"))
        miss <- TRUE
      }
    }
    else {
      if (is.null(data.name.x)) 
        data.name.x <- deparse(substitute(x))
      x <- as.vector(unlist(x))
      if (!is.numeric(x) || length(x) != length(y)) 
        stop(paste("All elements of 'x' must be a numeric, and", 
                   "'x' must have the same number of elements as 'y'"))
      names(data.name.x) <- "x"
      names(data.name) <- "y"
      if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(y)))) > 
          0) {
        if (warn) {
          is.not.finite.warning(x)
          is.not.finite.warning(y)
          warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and 'y' removed."))
        }
        x <- x[ok]
        y <- y[ok]
      }
      n <- length(y)
      if (n < 2) {
        if (warn) 
          warning(paste("'x' and 'y' do not contain at least two non-missing,", 
                        "finite observations"))
        miss <- TRUE
      }
      else if (length(unique(x)) < 2) {
        if (warn) 
          warning("'x' does not contain at least 2 distinct values")
        miss <- TRUE
      }
    }
    alternative <- match.arg(alternative, c("two.sided", "greater", 
                                            "less"))
    if (ci.slope && !is.vector(conf.level, mode = "numeric") || 
        length(conf.level) != 1 || conf.level <= 0 || conf.level >= 
        1) 
      stop("'conf.level' must be a numeric scalar between 0 and 1")
    if (miss) {
      stat <- c(z = NA)
      p.value <- NA
      estimate <- c(tau = NA, slope = NA, intercept = NA)
      method <- "Kendall's Test for Trend"
      estimation.method <- paste("slope:      Theil/Sen Estimator", 
                                 "intercept:  Conover's Estimator", sep = paste("\n", 
                                                                                paste(rep(" ", 33), sep = "", collapse = ""), sep = ""))
      S <- NA
      var.S <- NA
      slopes <- NA
    }
    else {
      vark <- function(x, y) {
        ties.x <- rle(sort(x))$lengths
        ties.y <- rle(sort(y))$lengths
        n <- length(x)
        t1 <- n * (n - 1) * (2 * n + 5)
        t2 <- sum(ties.x * (ties.x - 1) * (2 * ties.x + 5))
        t3 <- sum(ties.y * (ties.y - 1) * (2 * ties.y + 5))
        v1 <- (t1 - t2 - t3)/18
        if (n > 2) {
          t1 <- sum(ties.x * (ties.x - 1) * (ties.x - 2))
          t2 <- sum(ties.y * (ties.y - 1) * (ties.y - 2))
          v2 <- (t1 * t2)/(9 * n * (n - 1) * (n - 2))
        }
        else v2 <- 0
        t1 <- sum(ties.x * (ties.x - 1)) * sum(ties.y * (ties.y - 
                                                           1))
        v3 <- t1/(2 * n * (n - 1))
        v1 + v2 + v3
      }
      index <- 2:n
      S <- sum(sapply(index, function(i, x, y) {
        sum(sign((x[i] - x[1:(i - 1)]) * (y[i] - y[1:(i - 
                                                        1)])))
      }, x, y))
      tau <- (2 * S)/(n * (n - 1))
      slopes <- unlist(lapply(index, function(i, x, y) (y[i] - 
                                                          y[1:(i - 1)])/(x[i] - x[1:(i - 1)]), x, y))
      slopes <- sort(slopes[is.finite(slopes)])
      slope <- median(slopes)
      intercept <- median(y) - slope * median(x)
      estimate <- c(tau, slope, intercept)
      names(estimate) <- c("tau", "slope", "intercept")
      method <- "Kendall's Test for Trend"
      estimation.method <- paste("slope:      Theil/Sen Estimator", 
                                 "intercept:  Conover's Estimator", sep = paste("\n", 
                                                                                paste(rep(" ", 33), sep = "", collapse = ""), sep = ""))
      var.S <- vark(x, y)
      if (correct) {
        method <- paste(method, "(with continuity correction)", 
                        sep = paste("\n", paste(rep(" ", 33), sep = "", collapse = ""), sep = ""))
        stat <- (S - sign(S))/sqrt(var.S)
      }
      else stat <- S/sqrt(var.S)
      names(stat) <- "z"
      p.value <- switch(alternative, greater = 1 - pnorm(stat), 
                        less = pnorm(stat), two.sided = 2 * pnorm(-abs(stat)))
    }
    parameters <- NULL
    null.value <- 0
    attr(null.value, "names") <- "tau"
    data.name <- c(data.name, data.name.x)
    ret.list <- list(statistic = stat, parameters = parameters, 
                     p.value = p.value, estimate = estimate, null.value = null.value, 
                     alternative = alternative, method = method, estimation.method = estimation.method, 
                     sample.size = n, data.name = data.name, bad.obs = bad.obs, 
                     S = S, var.S = var.S, slopes = slopes)
    if (!miss && ci.slope) {
      if (n < 3) {
        stop(paste("When ci.slope=TRUE, there must be at least", 
                   "3 non-missing, finite observations"))
      }
      N.prime <- length(slopes)
      type <- switch(alternative, two.sided = "two-sided", 
                     greater = "lower", less = "upper")
      alpha <- 1 - conf.level
      Z <- ifelse(type == "two-sided", qnorm(1 - alpha/2), 
                  qnorm(conf.level))
      C.alpha <- Z * sqrt(var.S)
      M1 <- (N.prime - C.alpha)/2
      M2 <- (N.prime + C.alpha)/2
      limits <- switch(type, `two-sided` = approx(1:N.prime, 
                                                  slopes, xout = c(M1, M2 + 1))$y, lower = c(approx(1:N.prime, 
                                                                                                    slopes, xout = M1)$y, Inf), upper = c(-Inf, approx(1:N.prime, 
                                                                                                                                                       slopes, xout = M2 + 1)$y))
      names(limits) <- c("LCL", "UCL")
      if (any(is.na(limits))) 
        warning(paste("Sample size too small for Normal approximation", 
                      "for confidence interval for slope.\n"))
      interval <- list(name = "Confidence", parameter = "slope", 
                       limits = limits, type = type, method = paste("Gilbert's Modification", 
                                                                    "of Theil/Sen Method", sep = paste("\n", paste(rep(" ", 33), sep = "", collapse = ""), 
                                                                                                       sep = "")), conf.level = conf.level, sample.size = N.prime)

      ret.list <- c(ret.list, list(interval = interval))
    }
    if (!is.null(parent.of.data)) 
      ret.list$parent.of.data <- parent.of.data
    if (!is.null(subset.expression)) 
      ret.list$subset.expression <- subset.expression

    ret.list
  }

#' @rdname kendallTrendTest
#'
#' @export
#'
#' @method kendallTrendTest formula
kendallTrendTest.formula <- function (y, data = NULL, subset, na.action = na.pass, ...){
    if (missing(y) || (length(y) != 3L)) 
      stop("formula missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
    m$... <- NULL
    m$formula <- m$y
    m$y <- NULL
    m$na.action <- na.action
    requireNamespace("stats", quietly = TRUE)
    m[[1L]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    arg.list <- list(y = mf[, response])
    names.mf <- names(mf)
    names.list <- list(data.name = names.mf[response])
    if (ncol(mf) > 1) {
      x <- mf[, -response]
      arg.list <- c(arg.list, list(x = x))
      names.list <- c(names.list, list(data.name.x = names.mf[-response]))
    }
    dot.list <- list(...)
    match.vec <- pmatch(names(dot.list), c("data.name", "data.name.x"), 
                        nomatch = 0)
    if (length(match.vec) == 0 || all(match.vec == 0)) 
      arg.list <- c(arg.list, names.list, dot.list)
    else arg.list <- c(arg.list, names.list[-match.vec], dot.list)
    if (!missing(data)) 
      arg.list$parent.of.data <- deparse(substitute(data))
    if (!missing(subset)) 
      arg.list$subset.expression <- deparse(substitute(subset))
    do.call(kendallTrendTest.default, arg.list)
  }