#' Calculate arithmetic mean
#'
#' This function calculates the average or mean of a numerical object
#'
#' @param x Numeric vector.
#'
#' @return A numeric value corresponding to the mean.
#' If x contains missing values, the function will remove them and compute mean for available values.
#' If x is null or contains a single value, the function will return an error message.
#'
#' @examples
#' df <- data.frame(
#'    patid = c("PO1", "P02", "P03", "P04", "P05"),
#'    glucose = c(5.6, 7.5, 4.3, 8.4, 10.2)
#'    )
#'
#'  calc_mean(df$glucose)
#'
#' @export
calc_mean <- function(x){

  ### Checks parameter of the function###
  if (is.null(x)) {
    stop("x must be numeric, not NULL.")
  }

  checkmate::assert_numeric(x)

  if (any(is.na(x))) {
    warning(
      paste0(
        "There are ",
        sum(is.na(x)),
        " missing value(s) in x. Please note the function removes the missing values ",
        "and returns a mean for the available values."
      )
    )
  }

  ### Calculates mean using baseR
  mean(x, na.rm = TRUE)
}

#' Calculate median
#'
#' This function calculates the median of a numerical object
#'
#' @param x Numeric vector.
#'
#' @return A numeric value corresponding to the median.
#' If x contains missing values, the function will remove them and compute median for available values.
#' If x is null or contains a single value, the function will return an error message.
#'
#' @examples
#' df <- data.frame(
#'    patid = c("PO1", "P02", "P03", "P04", "P05"),
#'    glucose = c(5.6, 7.5, 4.3, 8.4, 10.2)
#'    )
#'
#' calc_median(df$glucose)
#'
#' @export
calc_median <- function(x){

  ### Checks parameter of the function###
  if (is.null(x)) {
    stop("x must be numeric, not NULL.")
  }

  checkmate::assert_numeric(x)

  if (any(is.na(x))) {
    warning(
      paste0(
        "There are ",
        sum(is.na(x)),
        " missing value(s) in x. Please note the function removes the missing values ",
        "and returns a median for the available values."
      )
    )
  }

  #### Leverage median function from stats package ####
  stats::median(x, na.rm = T)
}

#' Calculate mode (handle ties and no mode cases)
#'
#' This function calculates the mode of a numerical object
#'
#' @param x A numeric vector
#'
#' @return A numeric value corresponding to the mode.
#' If x contains missing values, the function will remove them and compute mode for available values.
#' If x is null or contains a single value, the function will return an error message.
#'
#' @examples
#' df <- data.frame(
#'    patid = c("PO1", "P02", "P03", "P04", "P05"),
#'    glucose = c(5.6, 7.5, 4.3, 8.4, 10.2)
#'    )
#'
#' calc_mode(df$glucose)
#'
#' @export
calc_mode <- function(x){

  ### Check parameter of the function###
  if (is.null(x)) {
    stop("x must be numeric, not NULL.")
  }

  checkmate::assert_numeric(x)

  if (any(is.na(x))) {
    warning(
      paste0(
        "There are ",
        sum(is.na(x)),
        " missing value(s) in x. Please note the function removes the missing values ",
        "and returns a mode for the available values."
      )
    )
  }

  ###Leveraging mode functin from DescTools package
  #Using a numeric to remove the frequency element of the returned object using the Mode function
  as.numeric(DescTools::Mode(x, na.rm = T))

}

#' Calculate first quartile (Q1)
#'
#' This function calculates the first quartile of a numerical object
#'
#' @param x A numeric vector
#'
#' @return A numeric value corresponding to the first quartile value.
#' If x contains missing values, the function will remove them and compute the first quartile for available values.
#' If x is null or contains a single value, the function will return an error message.
#'
#' @examples
#' df <- data.frame(
#'    patid = c("PO1", "P02", "P03", "P04", "P05"),
#'    glucose = c(5.6, 7.5, 4.3, 8.4, 10.2)
#'    )
#'
#' calc_q1(df$glucose)
#'
#' @export
calc_q1 <- function(x){

  ### Check parameter of the function###
  if (is.null(x)) {
    stop("x must be numeric, not NULL.")
  }

  checkmate::assert_numeric(x)

  if (any(is.na(x))) {
    warning(
      paste0(
        "There are ",
        sum(is.na(x)),
        " missing value(s) in x. Please note the function removes the missing values ",
        "and returns the first quartile based on the available values."
      )
    )
  }

  #### Leveraging  quantile function from stats package
  stats::quantile(x, 0.25, na.rm=T)
}

#' Calculate third quartile (Q3)
#'
#' This function calculates the third quartile of a numerical object
#'
#' @param x A numeric vector
#'
#' @return A numeric value corresponding to the third quartile value.
#' If x contains missing values, the function will remove them and compute the first quartile for available values.
#' If x is null or contains a single value, the function will return an error message.
#'
#' @examples
#' df <- data.frame(
#'    patid = c("PO1", "P02", "P03", "P04", "P05"),
#'    glucose = c(5.6, 7.5, 4.3, 8.4, 10.2)
#'    )
#'
#' calc_q3(df$glucose)
#'
#' @export
calc_q3 <- function(x){

  ### Check parameter of the function###
  if (is.null(x)) {
    stop("x must be numeric, not NULL.")
  }

  checkmate::assert_numeric(x)

  if (any(is.na(x))) {
    warning(
      paste0(
        "There are ",
        sum(is.na(x)),
        " missing value(s) in x. Please note the function removes the missing values ",
        "and returns the third quartile based on the available values."
      )
    )
  }

  #### Leveraging  quantile function from stats package
  stats::quantile(x, 0.75, na.rm=T)
}

#' Calculate interquartile range (IQR)
#'
#' This function calculates the IQR of a numerical object
#'
#' @param x A numeric vector
#'
#' @return A numeric value corresponding to the IQR value.
#' If x contains missing values, the function will remove them and compute the IQR for available values.
#' If x is null or contains a single value, the function will return an error message.
#'
#' @examples
#' df <- data.frame(
#'    patid = c("PO1", "P02", "P03", "P04", "P05"),
#'    glucose = c(5.6, 7.5, 4.3, 8.4, 10.2)
#'    )
#'
#' calc_iqr(df$glucose)
#' @export
calc_iqr <- function(x){

  ### Check parameter of the function###
  if (is.null(x)) {
    stop("x must be numeric, not NULL.")
  }

  checkmate::assert_numeric(x)

  if (any(is.na(x))) {
    warning(
      paste0(
        "There are ",
        sum(is.na(x)),
        " missing value(s) in x. Please note the function removes the missing values ",
        "and returns the third quartile based on the available values."
      )
    )
  }

  #### Leveraging IQR function from stats package
  stats::IQR(x, na.rm = T)
}
