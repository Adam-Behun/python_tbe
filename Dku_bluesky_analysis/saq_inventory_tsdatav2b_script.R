#' Make inventory of time series from South Asia air Quality project data file
#' If available, read file with harmonization factors and map them by sitename
#'
#' Create tibble from csv file
#' Variables (columns) to read are hard-coded below, see var_in, var_out, units_out
#' Columns with all rows the same converted to attributes, see att_name, att_name_out
#'
#' Variable name conventions:
#' tf: table read using read.csv
#' tb: tibble of data
#' tc: tibble of metadata
#'
#'
#' @param flsource directory containing csv file ('extdata' to read test data)
#' @param dotbe Control output of TBE csv file with inventory
#'
#' @return result: tb: Tibble with inventory data, tc: Metadata, error: Error message
#' @export
#'
#' @importFrom dplyr count rename
#' @importFrom lubridate mdy_hm with_tz
#' @importFrom tibble tibble tribble tibble_row
#' @importFrom utils read.csv
#' @importFrom countrycode countrycode
#' @importFrom stringr str_replace_all str_detect
#' @importFrom lutz tz_lookup_coords tz_offset
#' @importFrom gtools mixedorder
#'
#' @examples
#' tb <- saq_inventory_tsdata(flsource='/d/b/Aqs_saq/Data_tsiv2b/Nepal/')

#saq_inventory_tsdatav2b <- function(flsource = '/d/b/Aqs_saq/Data_tsiv2b/Nepal/',
#                                 dotbe = 1
#                                 ) {

rm(list=ls())
library(tidyverse)
library(lubridate)
library(countrycode)
library(lutz)
library(gtools)
library(utils)

## we need printf from bdf_utils.R:
source('bdf_utils.R')

flsource = '/d/b/Aqs_saq/Data_tsiv2b/Nepal/'
region_str = 'npl' # for output file name

#flsource = 'C:/Users/bdefoy/Desktop/Dku_data_pull/Our\ Data/Bangladesh/'
flsource = '../Dku_data_pull/Our\ Data/Bangladesh/'
region_str = 'bgd' # for output file name

dotbe = 1

  varscale <- 'pm25' # hard-coded below, search for pm25_scale and pm25_offset

  afldata = Sys.glob(sprintf('%s*/*/Level1.csv',flsource))
  flscale = NULL
  flsource_scale = NULL

  tzone_output = 'UTC'

  ## Get the user name from the system - or specify your own here
  user_name = Sys.getenv('USERNAME')
  ##user_name = 'Benjamin de Foy'

  ## Input file source and information:
  source_str = 'Duke Bluesky Sensors'

  ## Variables to read from LCS file, hardcode output name and units:
  var_in = c('PM1.0..ug.m3.','PM2.5..ug.m3.')
  var_out = c('pm1','pm25')
  units_out = c('ug/m3','ug/m3')

  ## Attributes to read from LCS file, hardcode output name:
  att_name = c( 'Site.Name','Serial.Number', 'Latitude', 'Longitude' )
  ## Not used:
  ##att_name_out = c( 'sitename','serial_number','latitude','longitude' )

  nf <- 0
  for ( flcsv in afldata ) {
    if ( identical(flsource,'extdata')) {
      flcsv_full <- system.file("extdata", flcsv, package="Aqsebs")
      if ( identical(flcsv_full,'') ) {
        return( list(tb=NULL, tc=NULL, error=sprintf('File not found: %s',flcsv)) )
      }
    } else {
      ##flcsv_full <- paste(flsource,flcsv,sep='')
      flcsv_full <- flcsv
    }

    if ( !file.exists(flcsv_full) ) {
      return( list(tb=NULL, tc=NULL, error=sprintf('File not found: %s',flcsv_full)) )
    } else {
      printf('Will read file %s',flcsv_full)
    }

    ## Read data into a data.frame:
    tf_step <- read.csv(flcsv_full, header=TRUE, sep=",")
                                        # read_table(flcsv_full) does not work (because of non-conventional column names?)
    if ( nf == 0 ) {
      tf <- tf_step
    } else {
      tf <- bind_rows(tf,tf_step)
    }
    nf <- nf + 1
  }

  ## Check that variables and attributes are present in file:
  nvar_notfound = 0
  var_notfound = c()
  for ( ivar in 1:length(var_in) ) {
    if ( ! var_in[ivar] %in% names(tf) ) {
      nvar_notfound = nvar_notfound + 1
      var_notfound[nvar_notfound] = var_in[ivar]
    }
  }
  natt_notfound = 0
  att_notfound = c()
  for ( iatt in 1:length(att_name) ) {
    if ( ! att_name[iatt] %in% names(tf) ) {
      natt_notfound = natt_notfound + 1
      att_notfound[natt_notfound] = att_name[iatt]
    }
  }

  if ( nvar_notfound > 0 || natt_notfound > 0 ) {
    error_msg = sprintf('Error reading file: %d variables not found: %s, %d attribute variables not found: %s',nvar_notfound,paste(var_notfound,collapse=', '),natt_notfound,paste(att_notfound,collapse=', ') )

    ## Header from airnow.gov: Site,Parameter,Date (LT),Year,Month,Day,Hour,NowCast Conc.,AQI,AQI Category,Raw Conc.,Conc. Unit,Duration,QC Name
    if ( 'NowCast.Conc.' %in% names(tf) ) {
      error_msg = sprintf('%s; Check: is your file an airnow.gov file instead of Duke Bluesky? If so, use ebs_create_tbe()',error_msg)
     }
    return( list(tb=NULL, tc=NULL, error=error_msg))
  }

  # Calculate date/time, Bluesky data is in UTC:
  date <- as.POSIXct(mdy_hm(tf$Timestamp..UTC.),format="%Y-%m-%d %H:%M:%S")
  date <- with_tz(date,tzone_output)
  tf$date = date

  ## Read harmonization factors
  if ( ! is.null(flscale) ) {
    flscale_full = sprintf('%s%s.csv',flsource_scale,flscale)
    tf_scale <- read.csv(flscale_full, header=TRUE, sep=",")
    doaddscale <- 1
    flscale_out = flscale
  } else {
    doaddscale <- 0
    flscale_out = 'External'
  }

  ## For each Serial Number: Find unique Lat/Lon, Find min/max date
  tf$stloc = sprintf('%.0f_%g_%g',tf$Serial.Number,tf$Latitude,tf$Longitude)
  tsite = tibble(sitename=character(),plocation=numeric())
  nrec = 0

  for ( site  in unique(tf$stloc) ) {
    tfs = filter(tf,stloc==site)
    sitename = unique(tfs$Site.Name)
    if ( length(sitename) > 1 ) {
      return( list(tb=NULL, tc=NULL, error=sprintf('Too many sitenames (%d) found for location %s',length(sitename),site)) )
    }

    nrec = nrec + 1

    ## get scale factor
    offset_factor = 0 # place holder for now
    if ( doaddscale == 1 ) {
      tfsc = filter(tf_scale,Location==sitename)
      if ( nrow(tfsc) == 1 ) {
        scale_factor = tfsc$Harmonization.Factor
        printf('Found scale factor %f for site %s',scale_factor,sitename)
      } else if ( nrow(tfsc) == 0 ) {
        scale_factor = 1
        printf('Scale factor not found for site %s, using %f',sitename,scale_factor)
      } else {
        scale_factor = 1
        printf('Too many scale factors found for site %s: %d, using %f',sitename,length(scale_factor),scale_factor)
      }
    } else {
      scale_factor = 1
    }

    ## Create Site ID:
    stcountry = tfs$Country[1]
    stctry = countrycode(stcountry,origin='country.name',destination='iso3c',nomatch=NULL)

    stname = tfs$Site.Name[1]
    stid = saq_sitename2id(stname)

    if ( str_detect(stid,sprintf('^(?i)%s',stctry)) ) {
    } else {
      stid = sprintf('%s_%s',tolower(stctry),stid)
    }

    pm25_calibration <- unique(!is.na(tfs$Applied.PM2.5.Custom.Calibration.Factor))
    if ( ! pm25_calibration ) { pm25_calibration <- 1 }

    ## Screen for sites that have moved around:
    tsite = add_row(tsite,sitename=tfs$Site.Name[1],plocation=1)
    tsite$plocation[nrec] = sum(tsite$sitename==tfs$Site.Name[1],na.rm=TRUE)

    ## Get timezone: (method="accurate" requires package sf which has problems on Linux at the moment), stipulate some countries to avoid errors:
    if ( stcountry == 'Bhutan' ) {
      tz_name = 'Asia/Thimphu'
      tz_offset = 6
    } else if ( stcountry == 'India' ) {
      tz_name = 'Asia/Kolkata'
      tz_offset = 5.5
    } else if ( stcountry == 'Nepal' ) {
      tz_name = 'Asia/Kathmandu'
      tz_offset = 5.75
    } else {
      tz_name = tz_lookup_coords(tfs$Latitude[1],tfs$Longitude[1],method="fast")
      tz_offset = tz_offset("2020-01-01",tz_name)$utc_offset_h
    }

    ## Create new row for inventory table
    tr = tibble_row(country=stcountry,
                    sitename=stname,
                    siteid=stid,
                    serial_number=tfs$Serial.Number[1],
                    plocation=tsite$plocation[nrec],
                    latitude=tfs$Latitude[1],
                    longitude=tfs$Longitude[1],
                    is_indoors=tfs$is_indoors[1],
                    nmeasurements=sum(tfs$Entry.Count),
                    nrecords=nrow(tfs),
                    date_start=min(tfs$date),
                    date_end=max(tfs$date),
                    timezone=tz_name,
                    utc_offset=tz_offset,
                    pm25_calibration=pm25_calibration,
                    pm25_scale=scale_factor,
                    pm25_offset=offset_factor)

    if ( nrec == 1 ) {
      tb = tibble(tr)
      tc <- tribble(~Variable,~Units,~Description,
                    'country','Name','',
                    'sitename','Name','Original Name from Research Group',
                    'siteid','Name','Unique ID for Aqsebs',
                    'serial_number','Integer','TSI Number',
                    'plocation','Integer','>1: New site location',
                    'latitude','degrees N','',
                    'longitude','degrees E','',
                    'is_indoors','True=inside','',
                    'nmeasurements','Number','Number of original data points',
                    'nrecords','Number','Number of records in input file',
                    'date_start','Time (Etc/UTC)','',
                    'date_end','Time (Etc/UTC)','',
                    'timezone','IANA tzdata','Olson Names',
                    'utc_offset','hours','Offset at New Year',
                    'pm25_calibration','None','From TSIv2',
                    'pm25_scale','None',sprintf('From %s',flscale_out),
                    'pm25_offset','ug/m3','Placeholder' )
    } else {
      tb = add_row(tb, tr)
    }
    printf('Found Site %s, %d records from %s to %s',site,nrow(tfs),min(tfs$date),max(tfs$date))
  }

  if ( nrec == 0 ) {
    error_msg = 'No entries found'
    return( list(tb=NULL, tc=NULL, error=error_msg))
  }

  ## Sort data by siteid:
  ## Alphabetical messes up numbers (10 before 1):
  ##tb <- tb %>% arrange(siteid)
  ## Mixed order (from gtools) to get 1,2,3,...10,11,...:
  tb <- tb[mixedorder(tb$siteid),]

  ## Use min/max dates for output filename:
  date_str_flname <- sprintf('%s_%s',format(min(tb$date_start),'%Y%m%d'),format(max(tb$date_end),'%Y%m%d'))
  fl_tbe <- sprintf('saq_bluesky_%s_%s_inv_tbe.csv',region_str,date_str_flname)

  if ( dotbe ) {
    ## Write TBE File with inventory - code from saq_create_tbe.R
    tc_trans <- df_transpose(tc)
    colnames(tc_trans)[1] <- 'TBL Sites'
    tc_trans[[1]] <- sprintf('ATT %s',tc_trans[[1]])

    ## Create tibble with global information:
    tb_global <- tibble(Variable = c('Title','Source','Author','Date','Comment'),
                        Value = c(sprintf('Inventory for %s',region_str),source_str,user_name,as.character(Sys.time()),'Automatically generated by saq_inventory_tsdata.R'))

    nfl <- length(afldata)
    if ( nfl > 10 ) {
        nf <- 1; flcsv <- afldata[nf]
        tb_global <- bind_rows(tb_global,tibble_row(Variable=sprintf('InputFile%d',nf), Value=flcsv))
        nf <- nfl; flcsv <- afldata[nf]
        tb_global <- bind_rows(tb_global,tibble_row(Variable=sprintf('InputFile%d',nf), Value=flcsv))
    } else {
        nf <- 0
        for ( flcsv in afldata ) {
            nf <- nf + 1
            tb_global <- bind_rows(tb_global,tibble_row(Variable=sprintf('InputFile%d',nf), Value=flcsv))
        }
    }

    ## Write Global Information:
    tbout <- add_column(tb_global,'TBL Global'='',.before=1)
    tbout[1,1] <- 'BGN'; tbout[nrow(tbout),1] <- 'EOT Global'
    write_csv(tbout,fl_tbe,append=FALSE,col_names=TRUE)
    write(',,,',fl_tbe,append=TRUE) # I want a blank line between tables for clarity

    # Write Time Series:
    write_csv(tc_trans,fl_tbe,append=TRUE,col_names=TRUE)
    tb_displayname = sprintf('%s (%s)',tc$Variable,tc$Units)
    tbout <- tb;
    # convert date to string for csv (write_csv writes times as ISO8601 only)
    tbout$date_start <- as.character(tbout$date_start)
    tbout$date_end <- as.character(tbout$date_end)
    names(tbout) <- tb_displayname
    tbout <- add_column(tbout,'ATT DisplayName'='',.before=1)
    tbout[1,1] <- 'BGN'; tbout[nrow(tbout),1] <- 'EOT Timeseries'
    if ( any(tolower(names(tc))=='displayname') ) {
      write_csv(tbout,fl_tbe,append=TRUE,col_names=FALSE)
    } else {
      # If we do not have display names in tc, then write out col_names here:
      write_csv(tbout,fl_tbe,append=TRUE,col_names=TRUE)
    }
    printf('%s written with inventory for %s sites',fl_tbe,nrec)
  }

  ## we now have tibble "tb" with inventory data; tc with metadata
#  return( list(tb=tb, tc=tc, error=NULL) )

#}
