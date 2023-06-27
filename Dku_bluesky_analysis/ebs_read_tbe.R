#' Read tbe file
#'
#' By default: All table names will be converted to lower case
#' "Timeseries" table will load into tb (data) and tc (metadata)
#' Other tables will load into eg. tb_sites and tc_sites
#'
#' Returns a list "result" containing data tibbles (tc_...) and associated metadata (tc_...) or an error message
#'
#' Variable name conventions:
#' Prefixes:
#' tf: table read using fread
#'   tf: all
#'   tf_tbl: single table read
#' tb: tibble of data
#' tc: tibble of metadata
#'
#' hdr: header variables
#' att: attribute variables (aka metadata)
#' cmt: comments
#'
#' i...: index of row in the file
#' n...: number of records/rows/attributes
#'
#' @param flin File name
#' @param flsource Source: extdata or directory
#' @param tblselect Null: Read all tables, or name of table to read
#'
#' @return result: List of tibbles with tables and metadata, error: Error Message
#' @export
#'
#' @importFrom data.table fread
#' @importFrom stringr str_sub str_replace str_replace_all str_match
#' @importFrom lubridate force_tz
#' @importFrom dplyr select
#' @importFrom rlang .data

library(tidyverse)
library(lubridate)
library(data.table)

ebs_read_tbe <- function(flin = 'ebs_sites_20220125_tbe.csv',
                         flsource = 'extdata',
                         tblselect = NULL) {
  #flin <- 'site_meta.csv'
  #flsource <- './'
  #tblselect <- NULL # or table name to read, eg. 'seasons'
  debugout <- 0 # debugout > 0: output debug info from reading TBE file

  result = list() # initialize output

  if ( identical(flsource,'extdata')) {
    flin_full <- system.file("extdata", flin, package="Aqsebs")
    if ( identical(flin_full,'') ) {
      return( list(result=NULL, error=sprintf('File not found: %s',flin)) )
    }
  } else {
    flin_full <- paste(flsource,flin,sep='')
  }

  if ( ! file_test('-f',flin_full) ) {
    return( list(result=NULL, error=sprintf('File not found: %s',flin_full)) )
  } else {
    if ( debugout ) { printf('Will read file %s',flin_full) }
  }

  # read and parse header column, read second column to fill in missing EOT:
  tf <- fread(flin_full, header=FALSE, sep=',', select=c(1,2), fill=TRUE)

  #itbl_header <- which(str_detect(a$V1,'^TBL'))
  nrecs <- length(tf$V1)
  tf_codes <- str_sub(tf$V1,1,3)
  tf_description <- str_sub(tf$V1,5,-1)
  itbl_header <- which( tf_codes == 'TBL' )
  ntables = length(itbl_header)

  for ( ntbl in 1:ntables ) {
    iheader = itbl_header[ntbl]
    tbl_str = tolower(tf_description[iheader])
    if ( !is.null(tblselect) && tbl_str != tolower(tblselect) ) {
      if ( debugout ) { printf('Skipping table %s',tbl_str) }
      next
    }

    # get last possible row in this table: either the row preceding the next table, or the end of the file:
    if ( ntbl < ntables) {
      ilast = itbl_header[ntbl+1]-1
    } else {
      ilast = nrecs
    }

    # Find end of data:
    ieot <- which( tf_codes[(iheader+1):ilast]=='EOT' )
    skipdata <- 0
    if ( length(ieot) > 1 ) {
      return( list(result=NULL,
                   error=sprintf('Too many EOT found for table in ebs_read_tbe')) )
    } else if ( length(ieot) == 0 ) {
      inotblank = which( tf$V2[(iheader+1):ilast] != '' & tf$V1[(iheader+1):ilast] == '' )
      if ( length(inotblank) > 0 ) {
        iend <- iheader + max(inotblank)
        if ( debugout ) {
          printf('EOT not found for table %d/%d, using iend = %d',ntbl,ntables,iend)
        }
      } else {
        # look for metadata:
        inotblank2 = which( tf$V2[(iheader+1):ilast] != '' )
        if ( length(inotblank2) > 0 ) {
          iend <- iheader + max(inotblank2)
          if ( debugout ) {
            printf('Found metadata but no data for table %d/%d, using iend = %d',ntbl,ntables,iend)
          }
          skipdata <- 1
        } else {
          if ( debugout ) {
            printf('No data and no metadata found for table %d/%d (consider fixing your file)',ntbl,ntables)
          }
          next
        }
      }
    } else {
      iend <- ieot + iheader
    }

    # Find start of data:
    if ( skipdata ) {
      # set istart to after table so all attributes are read
      istart <- iend + 1
      ndata <- 0
    } else {
        ibgn <- which( tf_codes[(iheader+1):iend]=='BGN' )
    if ( length(ibgn) > 1 ) {
      return( list(result=NULL,
                   error=sprintf('Too many BGN found for table in ebs_read_tbe')) )
    } else if ( length(ibgn) == 0 ) {
      iblank <- which(tf$V1[(iheader+1):iend]=='')
      if ( length(iblank) > 0 ) {
        # Use first row with blank code in the first column:
        istart <- iheader + min(iblank)
      } else {
        # only one row of data signaled by EOT
        istart <- iend
      }
      if ( debugout ) {
        printf('BGN not found for table %d/%d, using istart = %d',ntbl,ntables,istart)
      }
    } else {
      istart <- ibgn + iheader
    }
    ndata <- iend - istart + 1
    }

    # Read column names:
    hdr_all <- fread(flin_full, header=FALSE, sep=',',
                     skip=iheader-1, nrows=1, fill=TRUE)
    tbl_name <- str_replace(hdr_all$V1, '^TBL ', '')
    tbl_name <- str_replace_all(tbl_name, '\\s', '_')
    hdr_select <- which(!is.na(hdr_all))
    hdr <- hdr_all[ , hdr_select, with=FALSE]
    hdr$V1 <- paste('TBL_', tbl_name,sep='')
    hdr_data <- hdr_all[ , hdr_select, with=FALSE]
    #hdr_data<-hdr # when I do this, setting V1:=NULL affects both hdr_data and hdr: bug or feature?
    hdr_data <- select(hdr_data,-1) # alternative to (without raising warnings): hdr_data[,V1:=NULL]

    # Find and read metadata:
    iatt <- which( tf_codes[(iheader+1):(istart-1)]=='ATT' )
    if ( length(iatt) == 0 ) {
      if ( debugout ) {
        printf('No attributes found for Table %d/%d',ntbl,ntables)
      }
      att_trans = NULL
    } else {
      iatt1 = min(iatt) + iheader
      iatt2 = max(iatt) + iheader
      natt = iatt2 - iatt1 + 1
      att <- fread(flin_full, header=FALSE, sep=',',
                   skip=iatt1-1, nrows=natt, select=hdr_select, fill=TRUE)
      att_o <- att
      att <- att[iatt,]
      # In column 1 we have attribute names, remove ATT to keep just the names:
      att$V1 <- str_replace(att$V1,'^ATT ','')
      colnames(att) <- as.character(hdr)
      att_trans <- df_transpose(att)
      colnames(att_trans)[1] <- 'Variable'
    }

    # Read data:
    if ( ndata > 0 ) {
      # Need to read first column to screen for comments:

      tf_tbl <- fread(flin_full, header=FALSE, sep=',',
                      skip=istart-1, nrows=ndata, select=hdr_select, fill=TRUE)
      tf_tbl_codes <- str_sub(tf_tbl$V1,1,3)
      icmt <- ( tf_tbl_codes == "CMT")
      ncmt <- length(which(icmt))
      if ( ncmt > 0 ) {
        if ( debugout ) {
          printf('Removing %d comments from table %s',ncmt,tbl_str)
        }
        tf_tbl <- tf_tbl[!icmt]
      }
      tf_tbl[['V1']] <- NULL
      colnames(tf_tbl) <- as.character(hdr_data)
      # reset timezones for all dates:
      ##for ( var_col in which(sapply(tf_tbl,is.POSIXt)) ) { # does not work because just dates do not get encoded as POSIXt # need to import is.POSIXt from lubridate
      var_list = names(tf_tbl)
      for ( nv in 1:length(var_list) ) {
        vselect = var_list[nv]
        rtn = get_attribute_check(att_trans, vselect, 'Units')
        if ( is.null(rtn$result) ) {
          #printf('Did not find Units for %s, ignore for now (%s)',vselect,rtn$error)
          next
        }
        ## Check if we have a date/time (is.Date did not work, class returns POSIXct and POSIXt:
        if ( pillar::type_sum(tf_tbl[[vselect]]) == 'dttm' ) {
          ## Do we have time variable, units = "xxx Time (Est/UTC) xxx":
          tzone = str_match(rtn$result,"Time\\s*\\(\\s*(.*)\\s*\\)")[2]
          if ( ! is.na(tzone) ) {
            if ( tzone %in% OlsonNames() ) {
              printf('Found valid time and timezone (%s) for column %s',tzone,vselect)
              tf_tbl[[vselect]] <- force_tz(tf_tbl[[vselect]],tzone)
              next
            } else {
              printf('Invalid timezone (%s) for column %s, leave as is',tzone,vselect)
            }
          } else if ( rtn$result %in% OlsonNames() ) {
            tzone = rtn$result
            printf('Inferred time, found timezone (%s) for column %s',tzone,vselect)
              tf_tbl[[vselect]] <- force_tz(tf_tbl[[vselect]],tzone)
              next
          } else {
            printf('WARNING: No timezone found for variable %s, leave as is',vselect)
          }
        } else {
          #printf('Variable %s (%s) not a time variable, leave as is',vselect,rtn$result)
        }
      }
    } else {
      tf_tbl <- NULL
    }

    # Add data to result:
    # Find name for tibble, use "tb" and "tc" for timeseries:
    if ( identical(tbl_str,'timeseries') ) {
      tbname = sprintf('tb')
      tcname = sprintf('tc')
      ntb <- 1
      while ( any(names(result)==tbname) || any(names(result)==tcname) ) {
        ntb <- ntb + 1
        tbname = sprintf('tb%d',ntb)
        tcname = sprintf('tc%d',ntb)
      }
    } else {
      tbname = sprintf('tb_%s',tbl_str)
      tcname = sprintf('tc_%s',tbl_str)
      ntb <- 1
      while ( any(names(result)==tbname) || any(names(result)==tcname) ) {
        ntb <- ntb + 1
        tbname = sprintf('tb_%s%d',tbl_str,ntb)
        tcname = sprintf('tc_%s%d',tbl_str,ntb)
      }
    }

    if ( is.null(tf_tbl) ) {
      result[[tbname]] <- NULL
    } else if ( tolower(tbl_str) == 'global' ) {
      tb_gbl <- df_transpose(tibble(tf_tbl))
      #tb_gbl[[1]] <- NULL # if we don't need variable/value as a pair.
      result[[tbname]] <- tb_gbl
    } else {
      result[[tbname]] <- tibble(tf_tbl)
    }
    if ( is.null(att_trans) ) {
      result[[tcname]] <- NULL
    } else {
      result[[tcname]] <- tibble(att_trans)
    }

    # Summary message:
    printf('Tibbles %s and %s for table %d/%d: %s',tbname, tcname,ntbl,ntables,tf_description[iheader])
    if ( debugout ) {
      if ( ndata > 0 ) {
        printf('  Rows: header at %d, data from %d to %d',iheader,istart,iend)
      } else if ( !is.null(att_trans) ) {
        printf('  Rows: header at %d, metadata only from %d to %d',iheader,iatt1,iatt2)
      } else {
        printf('  Rows: header only at %d, no data, no metadata',iheader)
      }
      printf('  Columns: %s',paste(hdr_select,collapse=', '))
    }
  }
  printf('Finished reading file: %s',flin_full)
  return( list(result=result, error=NULL) )
}
