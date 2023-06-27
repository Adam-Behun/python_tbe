

# %notin% Not used + messes up documentation, comment out:
# Shorthand for Negate('%in')
# `%notin%` <- Negate(`%in%`)


#' Send sprintf output to the screen
#'
#' Part of bdf_utils collected from different sources.
#'
#' @param ... Arguments passed directly to sprintf
#' @return the string that was just printed
#' @family bdf_utils
#' @export
#' @examples
#' printf('Hello %s',Sys.getenv("USER"))

printf <- function(...) { cat(sprintf(...),'\n') }

#' Make siteid from sitename by removing special characters and shortening
#' siteid is lower case, snake case
#'
#' @return siteid Shortened site name for use in programming
#' @family bdf_utils
#' @export
#' @examples
#' stid <- saq_sitename2id(stname)
saq_sitename2id <- function(stname='') {
    stid = tolower(stname)
    stid = str_replace_all(stid,'[.,\\\'() /-]+','_')
    stid = str_replace_all(stid,'(?i)_of_','_')
    stid = str_replace_all(stid,'(?i)university','u')
    stid = str_replace_all(stid,'(?i)univ','u')
    stid = str_replace_all(stid,'(?i)airport','apt')
    stid = str_replace_all(stid,'(?i)campus','')
    stid = str_replace_all(stid,'(?i)school','')
    stid = str_replace_all(stid,'_+$','')
    stid = str_replace_all(stid,'^_+','')
    stid = str_replace_all(stid,'_+','_')
    # shorten cox_s_bazar to cox_bazar:
    stid = str_replace_all(stid,'cox_s','cox')
    return ( stid )
}

#' Get attribute from tibble with metadata, return attribute or empty string
#'
#' @param tc Tibble with metadata (attributes)
#' @param varselect Row entry to select inside tibble (assumes names are in Variable)
#' @param attselect Attribute to select
#'
#' @return attribute or ''
#' @family bdf_utils
#' @export
#'
#' @importFrom rlang .data

get_attribute <- function(tc=tibble(Variable=c('date','pm25','pm10'),
                                    Units=c('Asia/Dhaka','ug/m3','ug/m3'),
                                    DisplayName=c('Local Time','PM2.5','PM10')),
                          varselect='pm25',
                          attselect='Units') {

  ## single line with no checking:
  #var_attribute <- filter(tc,Variable==varselect)[[attselect]]
  #var_attribute <- filter(tc,Variable==varselect)$Units

  if ( ! any(names(tc)=='Variable') ) {
    return( '' )
  }
  if ( ! any(names(tc)==attselect) ) {
    return( '' )
  }

  tcf <- filter(tc, .data$Variable==varselect)

  if ( nrow(tcf) == 0 ) {
    return( '' )
  } else if ( nrow(tcf) > 1 ) {
    return( '' )
  }

  var_attribute <- tcf[[attselect]]

  return( var_attribute )

}

#' Get attribute from tibble with metadata with checking and diagnostics
#'
#' @param tc Tibble with metadata (attributes)
#' @param varselect Row entry to select inside tibble (assumes names are in Variable)
#' @param attselect Attribute to select
#'
#' @return result: attribute value, error: Error code if failed
#' @family bdf_utils
#' @export
#'
#' @importFrom rlang .data

get_attribute_check <- function(tc=tibble(Variable=c('date','pm25','pm10'),
                                    Units=c('Asia/Dhaka','ug/m3','ug/m3'),
                                    DisplayName=c('Local Time','PM2.5','PM10')),
                          varselect='pm25',
                          attselect='Units') {

  # single line with no checking:
  #var_attribute <- filter(tc,Variable==varselect)[[attselect]]

  if ( ! any(names(tc)=='Variable') ) {
    return( list(result=NULL, error=sprintf('Could not find Variable in input tibble for get_attribute_check')) )
  }
  if ( ! any(names(tc)==attselect) ) {
    return( list(result=NULL, error=sprintf('Could not find attribute %s in input tibble for get_attribute_check',attselect)) )
  }

  tcf <- filter(tc, .data$Variable==varselect)

  if ( nrow(tcf) == 0 ) {
    return( list(result=NULL, error=sprintf('Could not find entry for %s in input tibble for get_attribute_check',varselect)) )
  } else if ( nrow(tcf) > 1 ) {
    return( list(result=NULL, error=sprintf('Found multiple (%d) entries for %s in input tibble for get_attribute_check',nrow(tcf),varselect)) )
  }

  var_attribute <- tcf[[attselect]]

  return( list(result=var_attribute, error=NULL) )

}

#' Transpose a tibble, eg. for writing to file
#' Ian Gow, 30 May 2021
#' https://stackoverflow.com/questions/28917076/transposing-data-frames/
#'
#' Part of bdf_utils collected from different sources.
#'
#' @param df Input tibble
#' @return Transposed tibble
#' @export
#'
#' @family bdf_utils
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom rlang .data

df_transpose <- function(df) {
  first_name <- colnames(df)[1]
  # there should be no duplicates in the first column:
  if ( any(duplicated(df[,1]))) {
   df[,1] <- make.unique(pull(df,1),sep='_')
   printf('Made unique names in df_transpose:')
   print(df)
  }

  # all variables should be converted to chr:
  df_inum <- sapply(df, is.numeric)
  if ( sum(df_inum) > 0 ) {
    df[,df_inum] <- as.data.frame(apply(df[,df_inum],2,as.character))
  }

  # transpose:
  dft <-
    df %>%
    tidyr::pivot_longer(-1) %>%
    tidyr::pivot_wider(names_from = 1, values_from = "value")

  colnames(dft)[1] <- first_name
  return(dft)
}

#' Check if a number is an integer
#'
#' Part of bdf_utils collected from different sources.
#'
#' @family bdf_utils
#' @export
#' @param x Number to be compared to machine precision
#' @param tol Tolerance for integer determination
#' @return logical: 1 if the number is an integer within machine tolerance
#' @examples
#' utc_offset<-5.5;is.wholenumber(utc_offset)

###https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

### clear all plots in RStudio: dev.off() (see dev.list())

#' Clear all viewer objects
#'
#' This is the equivalent of using dev.off() for plots.
#'
#' @family bdf_utils
#' @export
#' @importFrom rstudioapi viewer

# https://stackoverflow.com/questions/49217915/how-to-clear-all-charts-from-viewer-pane-in-rstudio

clear_viewer_pane <- function() {
  dir <- tempfile()
  dir.create(dir)
  TextFile <- file.path(dir, "blank.html")
  writeLines("", con = TextFile)
  rstudioapi::viewer(TextFile)
}


## get leaflet legend to have minimum value at the bottom:
## replace addLegend(...) with addLegend_decreasing(..., decreasing=TRUE)
## https://stackoverflow.com/questions/40276569/reverse-order-in-r-leaflet-continuous-legend
## edited Mar 20, 2021 at 15:01 by Sinval; answered Mar 19, 2021 at 15:57 by Matias Poullain
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors,
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(),
                                  title = NULL, className = "info legend", layerId = NULL,
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {

    position <- match.arg(position)
    type <- "unknown"
    na.color <- NULL
    extra <- NULL
    if (!missing(pal)) {
        if (!missing(colors))
            stop("You must provide either 'pal' or 'colors' (not both)")
        if (missing(title) && inherits(values, "formula"))
            title <- deparse(values[[2]])
        values <- evalFormula(values, data)
        type <- attr(pal, "colorType", exact = TRUE)
        args <- attr(pal, "colorArgs", exact = TRUE)
        na.color <- args$na.color
        if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 0) {
            na.color <- NULL
        }
        if (type != "numeric" && !missing(bins))
            warning("'bins' is ignored because the palette type is not numeric")
        if (type == "numeric") {
            cuts <- if (length(bins) == 1)
                        pretty(values, bins)
                    else bins
            if (length(bins) > 2)
                if (!all(abs(diff(bins, differences = 2)) <=
                         sqrt(.Machine$double.eps)))
                    stop("The vector of breaks 'bins' must be equally spaced")
            n <- length(cuts)
            r <- range(values, na.rm = TRUE)
            cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
            n <- length(cuts)
            p <- (cuts - r[1])/(r[2] - r[1])
            extra <- list(p_1 = p[1], p_n = p[n])
            p <- c("", paste0(100 * p, "%"), "")
            if (decreasing == TRUE){
                colors <- pal(rev(c(r[1], cuts, r[2])))
                labels <- rev(labFormat(type = "numeric", cuts))
            }else{
                colors <- pal(c(r[1], cuts, r[2]))
                labels <- rev(labFormat(type = "numeric", cuts))
            }
            colors <- paste(colors, p, sep = " ", collapse = ", ")
        }
        else if (type == "bin") {
            cuts <- args$bins
            n <- length(cuts)
            mids <- (cuts[-1] + cuts[-n])/2
            if (decreasing == TRUE){
                colors <- pal(rev(mids))
                labels <- rev(labFormat(type = "bin", cuts))
            }else{
                colors <- pal(mids)
                labels <- labFormat(type = "bin", cuts)
            }
        }
        else if (type == "quantile") {
            p <- args$probs
            n <- length(p)
            cuts <- quantile(values, probs = p, na.rm = TRUE)
            mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
            if (decreasing == TRUE){
                colors <- pal(rev(mids))
                labels <- rev(labFormat(type = "quantile", cuts, p))
            }else{
                colors <- pal(mids)
                labels <- labFormat(type = "quantile", cuts, p)
            }
        }
        else if (type == "factor") {
            v <- sort(unique(na.omit(values)))
            colors <- pal(v)
            labels <- labFormat(type = "factor", v)
            if (decreasing == TRUE){
                colors <- pal(rev(v))
                labels <- rev(labFormat(type = "factor", v))
            }else{
                colors <- pal(v)
                labels <- labFormat(type = "factor", v)
            }
        }
        else stop("Palette function not supported")
        if (!any(is.na(values)))
            na.color <- NULL
    }
    else {
        if (length(colors) != length(labels))
            stop("'colors' and 'labels' must be of the same length")
    }
    legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
                   na_color = na.color, na_label = na.label, opacity = opacity,
                   position = position, type = type, title = title, extra = extra,
                   layerId = layerId, className = className, group = group)
    invokeMethod(map, data, "addLegend", legend)
}
