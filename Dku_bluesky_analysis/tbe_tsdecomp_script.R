#' Time series analysis using Kolmogorov-Zurbenko filter
#'
#' Differentiate different time scales present in time series
#' dokz: Use Kolmogorov-Zurbenko filter with parameters from Hogrefe et al., AE 2003
#' dosma: Use Successive Moving Average Subtraction with m_sma window, see Watson and Chow, JAWMA 2001
#' Preliminary draft, based on saq_plot_tbe.R
#' 9 Feb 2023: streamline code, merge tbe_tsdecomp.R and tbe_tsdecomp_des.R
#' 
#' @param indir Directory with tbe or rda files
#' @param stidsct Site ID to plot
#' @param varfile Pollutant used in file name
#' @param varsct Pollutant for plotting
#' @param yearfile Specific year to look for, NULL for wildcard
#' @param years_sct Vector of years to plot, or NULL to use all data
#' @param months_sct Vector of months to plot, or "all"
#' @param grp_season Variable in TBE file to use for seasons (eg. season0), or season: default MAM, ...
#'
#' @return result: Message, error: Error Message
#' @export
#'
#' @importFrom ncdf4 nc_open nc_close ncatt_get ncvar_get
#' @importFrom RNetCDF utcal.nc
#' @importFrom stats var
#' @importFrom readr read_rds
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble add_row
#' @importFrom magrittr %>%
#' @import lubridate
#' @import openair
#' @import kza
#' @import ggplot2
#' @import forcats
#' @import dplyr
#' @import tidyr
#' @import gridExtra
#' @import plotly
#' 

rm(list=ls())
library(tidyverse)
library(lubridate)
library(openair)
library(kza)
library(gridExtra)
library(plotly)

## we need printf from bdf_utils.R:
source('bdf_utils.R')

#tbe_tsdecomp <- function(series = 'mds',
#                         stidsct = 'dkuroof',
#                         varfile = 'pm25',
#                         dotchoice = 2, # 1: hourly, 2: 5-min
#                         yearfile = NULL,
#                         years_sct = NULL,
#                         months_sct = c("all"),
#                         grp_season = 'season0',
#                         season_sct = 'Dry', # NULL, 'All', 'Dry','Wet', 'DJF'
#                         timemin_sct = ymd_hm('2021-10-01 00:00'),
#                         timemax_sct = ymd_hm('2023-01-20 00:00')
#                         ) {
if ( 1 == 1 ) {  
  series = 'saqflat'
  stidsct = '3. Mohakhali'
  stid_out = saq_sitename2id(stidsct)
  varfile = 'pm25'
  dotchoice = 1
  grp_season = 'season0'
  season_sct = NULL
  timemin_sct = NULL
  timemax_sct = NULL
  
  ## years_sct: NULL do not select for years and months, otherwise use selectByDate
  years_sct = NULL
  ##years_sct <- 2021

  ## timeofday selection: this needs checking, not sure it is OK
  timeofdaysct <- NULL # NULL, 'Day', 'Night'
  
  timezone_output = 'UTC'
  ##indir = 'C:/Users/bdefoy/Dropbox/Bdfdrop/Tmp/'
  ##flroot = 'aqs_fresno_4_2013_2017_pm25'
  
  months_sct <- c("nov", "dec","jan","feb","mar")
  months_sct <-  c("all")
  if ( months_sct[1] == "all" ) {
    months_sct <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  }
  
  user_name = Sys.getenv('USERNAME')
  if ( user_name == '' ) { user_name = Sys.getenv('USER') }
  
  ## Flags to select which plots to do:
  dolog = 0 # take log10 of data
  dopexp = 0 # 1: to 10^value for plotting if dolog == 1 
  thresh_log = 1 # minimum for log: replace with NA below this

  dosummary = 0 # debug: for all variables in the table
  dotimevar = 0 # debug: boxplot of time scale variation (openair)
  
  nrow_min = 500
  
  dokz = 1 # KZ with kzprms - Kolmogorov-Zurbenko Filter
  dokzi = 1 # KZi with kziprms - Iterated KZ Filter
  dosma = 1 # SMA with smaprms - Successive Moving Average subtraction
  afilters = c('kz','kzi','sma')
  
  niteration = 2 # for KZi
  dosma_type = 2 # 1: finish with min (all > 0), 2: finish with kz (some < 0, extra iteration)
  
  if ( dotchoice == 1 ) {
    ## kz for hourly data: c(3,13,103,3,5,5) 
                                        #kzprms = array( c(3,13,103,3,5,5), dim=c(3,2))
    kzprms = array( c(3,25,361,1,3,5), dim=c(3,2))
    kziprms = array( c(3,13,361,1,1,1), dim=c(3,2))
    ## sma for hourly data: c( 4, 24, 720 )
    ## use smaprms: f+1, f*.5+1, f*.25+1 to have odd window (which code implicitly uses anyway)
    smaprms = c( 4, 24, 720 )
                                        #asmaprms = array( c(5,3,1,25,13,7,721,361,181), dim=c(3,3) )
    tchoice_str = 'hr'
  } else {
    ## kz for 5-min data: c(7,105,900,3,7,9)
    kzprms = array( c(7,105,901,3,7,9),dim=c(3,2))
    kziprms = array( c(7,105,901,1,1,1), dim=c(3,2))
    ## sma for 5-min data: c( 12, 288, 8640 )
    smaprms = c( 12, 288, 8640 )
    tchoice_str = '5mn'
  }
  
  dotbe = 1 # write output to TBE file
  
  plot2file = 1 # use dev.copy and dev.off, ggsave does not work with openair
  ## Set margins:
                                        #par(mar=c(0,0,0,0)+1.5)
  fgwidth = 800
  fgheight = 700
  fgsum = 1 # >1: larger summary plot
  ggwidth = 20
  ggheight = 25
  ggdpi = 100
  
  ## Look for data file:
  flprefix = sprintf('%s_',series)
  stidfile = stidsct
  varsct = varfile
  if ( series == 'saqflat' ) {
    ## Flat csv file (single row header) from Dku_bluesky_analysis
    fltype = 'flat'
    dotbesites = 0 # 0: if we don't have tb_sites, tc_sites
    indir = ''
    flroot = 'dku_bluesky_mohakhali_20211001_20230502_coords_15min_lt'
    flsuffix = '.csv'
  } else if ( series == 'aqs' ) {
    fltype = 'des'
    dotbesites = 1 # 0: if we don't have tb_sites, tc_sites
    indir = '/d/b/Aqs_v2/Des_myr_kza/'
    flsuffix = '.nc'
  } else if ( series == 'dku' ) {
    fltype = 'tbe'
    stidfile = 'bcd'
    varsct = sprintf('%s_%s',varfile,stidsct)
    dotbesites = 1
    indir = '/d/b/Aqs_rstats/'
    if ( dotchoice == 1 ) {
      flsuffix = '_l2_era5_hr_tbe.csv'
    } else {
      flsuffix = '_l1_5min_tbe.csv'
    }
  } else {
    fltype = 'tbe'
    dotbesites = 1
    indir = sprintf('/d/b/Aqs_%s/Tbe/',series)
    if ( dotchoice == 1 ) {
      if ( series == 'mds' ) {
        flsuffix = '_l2_era5_hr_tbe.csv'
      } else {
        flsuffix = '_tbe.csv'
      }
    } else {
      # only MDS has dotchoice == 2
      flsuffix = '_l1_5min_tbe.csv'
    }
  }

  if ( series != 'saqflat' ) {
    ## Find data files:
    rtn <- ebs_find_tbe(indir, stidfile, varfile, yearfile, flprefix, flsuffix)
    if ( is.null(rtn$error) ) {
      flroot <- rtn$flroot
    } else {
      return( list(result=NULL, error=sprintf('tbe_tsdecomp.R error: %s',rtn$error)) )
    }
    printf('File found, indir,flroot,flsuffix: %s %s %s',indir,flroot,flsuffix)
  }
  
  ## Read Data:
  if ( fltype == 'flat' ) {
    ## Flat csv file:
    flin = paste(flroot,flsuffix,sep='')
    tb <- read_csv(flin)
    varunits = 'ug/m3'
    duration_str = tchoice_str
    tc <- tribble(~Variable, ~Units, ~Site, ~Duration, ~Source, ~DisplayName,
                  'date', sprintf('Time (%s)',timezone_output), stidsct, '?', 'SAQ', 'date (Time (?))' )
  } else if ( fltype == 'tbe' ) {
    ## TBE csv file:
    flin = paste(flroot,flsuffix,sep='')
    rtn <- ebs_read_tbe(flin=flin, flsource=indir, tblselect=NULL)
    if ( is.null(rtn$error) ) {
      tb <- rtn$result$tb
      tc <- rtn$result$tc
      tb_sites <- rtn$result$tb_sites
      tc_sites <- rtn$result$tc_sites
    } else {
      return( list(result=NULL, error=sprintf('tbe_tsdecomp.R read error from ebs_read_tbe: %s',rtn$error)) )
    }
  } else if ( fltype == 'des' ) {
    ## DES NetCDF file:
    flnc = sprintf('%s%s.nc',indir,flroot)
    printf('Reading AQS data from %s',flnc)
    
    ncaqs <- nc_open(flnc)
    tunits <- ncatt_get(ncaqs,'time','units')
    time  <- ncvar_get(ncaqs,'time')
    date <- utcal.nc(tunits$value,time,type='c')
    date <- with_tz(date,timezone_output)
    tb <- tibble(date)
    
    ## create tibble with metadata:
    duration_str <- 'Seq 1 HR'
    tc <- tribble(~Variable, ~Units, ~Site, ~Duration, ~Source, ~DisplayName,
                  'date', sprintf('Time (%s)',timezone_output), stidsct, duration_str, 'AQS', 'date (Time (UTC))' )
    
    ff <- ncvar_get(ncaqs, varsct)
    ## NB: ncvar_get returns an array, but tibbles like vectors.
    ## Failure to use as.vector is not noticeable except in rare circumstances (eg. write_csv())
    tb[[varsct]] <- as.vector(ff)
    rtn <- ncatt_get(ncaqs, varsct, 'units')
    if ( rtn$hasatt ) {
      ffunits = rtn$value
    } else {
      ffunits = NULL
    }
    tc <- add_row(tc, Variable=varsct, Units=ffunits,
                  Site=stidsct, Duration=duration_str,
                  Source='AQS', DisplayName=sprintf('%s (%s)',varsct,ffunits))

    ## read sites information and create tb_sites, tc_sites (note hardcoded stid, Units)
    avars_sites = c( 'sitename','location_code','lat','lon','elev','gmtoffset','methodselect' )
    avars_sites_out = c( 'sitename','location_code','latitude','longitude','elevation','utc_offset','methodselect' )
    
    atts_sites = c( 'Units' )
    tb_sites <- tribble(~siteid,stidsct)
    tc_sites <- tribble(~Variable,~Units,'siteid','None')
    
    nv <- 0
    for ( vsct in avars_sites ) {
      nv <- nv + 1
      vsct_out <- avars_sites_out[nv]
      ff <- ncvar_get(ncaqs,tolower(vsct))
      tb_sites[[vsct_out]] <- as.vector(ff)
      tc_sites <- add_row(tc_sites, Variable=vsct_out)
      for ( attsct in atts_sites ) {
        rtn <- ncatt_get(ncaqs, tolower(vsct), tolower(attsct))
        if ( rtn$hasatt ) {
          tc_sites[tc_sites$Variable==vsct_out,attsct] <- rtn$value
        }
      }
    }

    nc_close(ncaqs)
  }
  
  if ( series != 'saqflat' ) {
    varunits = get_attribute(tc,varsct,'Units')
    duration_str = get_attribute(tc,varsct,'Duration')
  }
  
  # Look for specific season column in data:
  if ( is.null(grp_season) ) {
    printf('No season group specified, will use default seasons')
    grp_season = 'season'
  } else if ( ! grp_season %in% names(tb) ) {
    printf('Season group %s not found, will use default seasons',grp_season)
    grp_season = 'season'
  }

  ## create default season if need be:
  if ( ! grp_season %in% names(tb) ) {
    tb_season <- tibble(name=c('DJF','MAM','JJA','SON','DJF'),start_mmdd=c(1,301,601,901,1201))

    printf('Season group %s not found, will create default seasons using tb_season:',grp_season)
    print(tb_season)
    tb[[grp_season]] <- base::cut(100*month(tb$date)+day(tb$date),
                                  breaks=c(tb_season$start_mmdd-1,1231),
                                  labels=tb_season$name)
    tc <- rows_upsert(tc,tibble(Variable=grp_season,Units='Factor',Site=stidsct,
                                Duration=duration_str,Source='Calc',DisplayName='season (Factor)'))

  }
  
  ## Select times for plot:
  if ( !is.null(years_sct) ) {
    tb = selectByDate(tb, year=years_sct, month=months_sct)
    printf('Filtered tb by year and month %d: %s to %s',nrow(tb),min(tb$date),max(tb$date))
  }

  ## select season:
  if  ( !is.null(season_sct) && season_sct != 'All' ) {
    tb = filter(tb, tb[[grp_season]] == season_sct)
    printf('Filtered tb by season %s, %d: %s to %s',grp_season,nrow(tb),min(tb$date),max(tb$date))
  }
  if ( !is.null(timemin_sct) ) {
    tb = filter(tb, (tb$date >= timemin_sct & tb$date <= timemax_sct) )
    printf('Filtered tb by timemin/max_sct %d: %s to %s',nrow(tb),min(tb$date),max(tb$date))
  }

  ## create time of day categories for plotting:
  tb$timeofday = c('Night','Trans','Day','Trans','Night')[cut(hour(tb$date),c(-1,8.5,10.5,16.5,18.5,24))]
  tb$timeofday = as.factor(tb$timeofday)
  if ( ! is.null(timeofdaysct) ) {
    tb <- tb %>% filter( timeofday == timeofdaysct)
  }
  
  ## Date string for plot titles:
  date_str = sprintf('%s to %s',date(min(tb$date)),date(max(tb$date)))
  date_str_flname = sprintf('%s_%s',format(min(tb$date),'%Y%m%d'),format(max(tb$date),'%Y%m%d'))
  if  ( !is.null(season_sct) ) {
    date_str_flname = sprintf('%s_%s',date_str_flname,tolower(season_sct))
  }
  if  ( !is.null(timeofdaysct) ) {
    date_str_flname = sprintf('%s_%s',date_str_flname,tolower(timeofdaysct))
  }
  
  flrootout = sprintf('tsdecomp_%s_%s_%s_%s_i%dt%d',varfile,stid_out,date_str_flname,tchoice_str,niteration,dosma_type)

  printf('Data in tb %d: %s to %s',nrow(tb),min(tb$date),max(tb$date))
  if ( nrow(tb) < nrow_min ) {
    return( list(result=NULL, error=sprintf('Not enough data: %d < %d',nrow(tb),nrow_min)) )
  }

  dosynthetic = 0
  if ( dosynthetic == 1 ) {
    ## Trigonometry:
    t_hr = (as.numeric(tb$date)-as.numeric(tb$date[1]))/3600
    freq_hr = 1/24;
    tb[[varsct]] = cos(2*pi*t_hr*freq_hr) + 1
    freq_hr = 1/8760
    tb[[varsct]] = tb[[varsct]] + cos(2*pi*t_hr*freq_hr) + 1
  } else if ( dosynthetic == 2 ) {
    ## Random walk:
    t_hr = (as.numeric(tb$date)-as.numeric(tb$date[1]))/3600
    trw <- vector(mode="double", length=length(t_hr) )
    trw[1] = 0
    trw_min = 0
    trw_max = 100
    trw_sd = 1
    freq_rw = 1/24
    for ( i in 2:length(t_hr) ) {
      dtrw = rnorm(1,1,trw_sd) * cos(2*pi*freq_rw*t_hr[i-1]) - rnorm(1,1,trw_sd)*trw[i-1]
      trw[i] = max(min(trw[i-1]+dtrw,trw_max),trw_min)
    }
    tb[[varsct]] = trw
    plrw = ggplot(tb, aes_string(x='date', y=varsct))+
      geom_line(aes(color=varsct))+
      ggtitle(sprintf('Random Walk (%s, %s)',varsct,stidsct))
    show(plrw)

    timeVariation(tb, pollutant=varsct,
                  group=grp_season, normalise=TRUE,
                  main=sprintf('%s (%s) by season\n%s', varsct, varunits, date_str))
  }
  
  if ( dolog ) {
    varlog = paste0(varsct,'log')
    tb[[varlog]] = log10(tb[[varsct]])
    tb[[varlog]][is.infinite(tb[[varlog]])] = NA
    tb[[varlog]][tb[[varsct]] < thresh_log] = NA
    vselect = varlog
  } else {
    vselect = varsct
  }
  
  ## Generate Debug Plots:  
  if ( dosummary ) {
    ## Summary plot to begin with:
    nstart = 3 # 3: ignore iqa(2) for now
    nlast = length(names(tb))
    nblh = which(names(tb)=='blh')
    if ( length(nblh) == 0 ) {
      ## Plot all variables to single figure (no meteorology in table):
      summaryPlot(tb)
      if ( plot2file ) {
        dev.copy(png,paste0(flrootout,"_a_sum_all.png"), width=fgwidth*fgsum, height=fgheight*fgsum)
        dev.off()
      }
    } else {
      ## Plot pollution + blh separately from meteorology:
      summaryPlot(tb[,c(1,nstart:nblh)])
      if ( plot2file ) {
        dev.copy(png,paste0(flrootout,"_a_sum_conc.png"), width=fgwidth*fgsum, height=fgheight*fgsum)
        dev.off()
      }
      summaryPlot(tb[,c(1,(nblh+1):nlast)])
      if ( plot2file ) {
        dev.copy(png,paste0(flrootout,"_a_sum_met.png"), width=fgwidth*fgsum, height=fgheight*fgsum)
        dev.off()
      }
    }
  }

  if ( dotimevar ) {
    ## TS: plot variation by different timescales:
    timeVariation(tb, pollutant=varsct,
                  group=grp_season, normalise=TRUE,
                  main=sprintf('%s (%s) by season\n%s', varsct, varunits, date_str))
    if ( plot2file ) {
      flout = paste0(flrootout,"_tvar_ts_norm.png")
      dev.copy(png, flout, width=fgwidth, height=fgheight)
      dev.off()
      printf('Time variation plot created: %s',flout)
    }
    
    timeVariation(tb, pollutant=varsct,
                  group=grp_season, normalise=FALSE,
                  main=sprintf('%s (%s) by season\n%s', varsct, varunits, date_str))
    if ( plot2file ) {
      flout = paste0(flrootout,"_tvar_ts.png")
      dev.copy(png, flout, width=fgwidth, height=fgheight)
      dev.off()
      printf('Time variation plot created: %s',flout)
    }
  }
  
  ## Now do filtering
  
  if ( dokz ) {
    ## Kolmogorov-Zurbenko Filter
    ## Calculate smoothed time series from hourly time series
    ## (Christian Hogrefe, AtmEnv 2003)
    tb$kzs1 = kz(tb[[vselect]],kzprms[1,1],kzprms[1,2])
    tb$kzs2 = kz(tb[[vselect]],kzprms[2,1],kzprms[2,2])
    tb$kzs3 = kz(tb[[vselect]],kzprms[3,1],kzprms[3,2])

    ## Reset hours with missing input data to missing
    ## kz extends averages into missing blocks so long as there is some data in the window
    tb$kzs1[is.na(tb[[vselect]])|is.nan(tb[[vselect]])]=NA
    tb$kzs2[is.na(tb[[vselect]])|is.nan(tb[[vselect]])]=NA
    tb$kzs3[is.na(tb[[vselect]])|is.nan(tb[[vselect]])]=NA

    ## Get components:
    tb$kzc1 = tb[[vselect]] - tb$kzs1
    tb$kzc2 = tb$kzs1 - tb$kzs2
    tb$kzc3 = tb$kzs2 - tb$kzs3
    tb$kzc4 = tb$kzs3
  }

  if ( dokzi ) {
    ## KZ iterated filter:
    tb$kzis1 = tb[[vselect]]
    for ( iiteration in 1:niteration ) {
      tb$kzis1 = apply(cbind(tb[[vselect]],kz(tb$kzis1,kziprms[1,1],kziprms[1,2])),1,min)
    }
    if ( dosma_type == 2 ) {
      tb$kzis1 = kz(tb$kzis1,kziprms[1,1],kziprms[1,2])
    }
    
    tb$kzis2 = tb$kzis1
    for ( iiteration in 1:niteration ) {
      tb$kzis2 = apply(cbind(tb$kzis1,kz(tb$kzis2,kziprms[2,1],kziprms[2,2])),1,min)
    }
    if ( dosma_type == 2 ) {
      tb$kzis2 = kz(tb$kzis2,kziprms[2,1],kziprms[2,2])
    }
    
    tb$kzis3 = tb$kzis2
    for ( iiteration in 1:niteration ) {
      tb$kzis3 = apply(cbind(tb$kzis2,kz(tb$kzis3,kziprms[3,1],kziprms[3,2])),1,min)
    }
    if ( dosma_type == 2 ) {
      tb$kzis3 = kz(tb$kzis3,kziprms[3,1],kziprms[3,2])
    }
    
    ## Get components:
    tb$kzic1 = tb[[vselect]] - tb$kzis1
    tb$kzic2 = tb$kzis1 - tb$kzis2
    tb$kzic3 = tb$kzis2 - tb$kzis3
    tb$kzic4 = tb$kzis3
  }

  if ( dosma ) {
    ## Successive Moving Average Subtraction (Watson & Chow 2001)
    ## this should be applied to 1-min or 5-min data
    ## This is the same as retaining the positive difference of the high resolution ts minus the kz averaged ts
    
    tb$smas1 = apply(cbind(tb[[vselect]],kz(tb[[vselect]],smaprms[1]+1,1)),1,min)
    tb$smas1 = apply(cbind(tb[[vselect]],kz(tb$smas1,smaprms[1]*.5+1,1)),1,min)
    if ( dosma_type == 1 ) {
      tb$smas1 = apply(cbind(tb[[vselect]],kz(tb$smas1,smaprms[1]*.25+1,1)),1,min)
    } else {
      tb$smas1 = kz(tb$smas1,smaprms[1]*.25+1,1)
    }
    
    tb$smas2 = apply(cbind(tb$smas1,kz(tb$smas1,smaprms[2]+1,1)),1,min)
    tb$smas2 = apply(cbind(tb$smas1,kz(tb$smas2,smaprms[2]*.5+1,1)),1,min)
    if ( dosma_type == 1 ) {
      tb$smas2 = apply(cbind(tb$smas1,kz(tb$smas2,smaprms[2]*.25+1,1)),1,min)
    } else {
      tb$smas2 = kz(tb$smas2,smaprms[2]*.25+1,1)
    }
    
    tb$smas3 = apply(cbind(tb$smas2,kz(tb$smas2,smaprms[3]+1,1)),1,min)
    tb$smas3 = apply(cbind(tb$smas2,kz(tb$smas3,smaprms[3]*.5+1,1)),1,min)
    if ( dosma_type == 1 ) {
      tb$smas3 = apply(cbind(tb$smas2,kz(tb$smas3,smaprms[3]*.25+1,1)),1,min)
    } else {
      tb$smas3 = kz(tb$smas3,smaprms[3]*.25+1,1)
    }
    
    ## Get components:
    tb$smac1 = tb[[vselect]]-tb$smas1
    tb$smac2 = tb$smas1 - tb$smas2
    tb$smac3 = tb$smas2 - tb$smas3
    tb$smac4 = tb$smas3
  }
  
  ## Create Tibble with Mean, Stdev, Variance of KZ results:
  ## Specify the order of the factors (should match kz_mean, kz_var etc... below)
  component_str = c("C1","C2","C3","C4")
  component_fct = factor(component_str, levels=component_str)
  
  tb_kz = tibble('Process'="TS",'Component'='All','Mean'=mean(tb[[vselect]],na.rm=TRUE),'Mean_prc'=100,'Variance'=var(tb[[vselect]],na.rm=TRUE),'Variance_prc'=100)
  
  tc_kz = tribble(~Variable, ~Units, ~Sites, ~Duration, ~Source,
                  'Process','Filter_Name',stidsct,duration_str,'tbe_tsdecomp',
                  'Component','Name',stidsct,duration_str,'tbe_tsdecomp',
                  'Mean',varunits,stidsct,duration_str,varsct,
                  'Mean_prc','%',stidsct,duration_str,varsct,
                  'Variance',sprintf('(%s)^2',varunits),stidsct,duration_str,varsct,
                  'Variance_prc','%',stidsct,duration_str,varsct)
  
  if ( dokz ) {
    kz_mean = c(mean(tb$kzc1,na.rm=TRUE), mean(tb$kzc2,na.rm=TRUE), mean(tb$kzc3,na.rm=TRUE), mean(tb$kzc4,na.rm=TRUE))
    kz_mean_prc = kz_mean/sum(kz_mean)*100
    
    kz_var = c(var(tb$kzc1,na.rm=TRUE), var(tb$kzc2,na.rm=TRUE), var(tb$kzc3,na.rm=TRUE), var(tb$kzc4,na.rm=TRUE))
    kz_var_prc = kz_var/sum(kz_var)*100
    
    tb_kz = add_row(tb_kz,'Process'='KZ','Component'=component_fct,'Mean'=kz_mean,'Mean_prc'=kz_mean_prc,'Variance'=kz_var,'Variance_prc'=kz_var_prc)
  }
  
  if ( dokzi ) {
    kzi_mean = c(mean(tb$kzic1,na.rm=TRUE), mean(tb$kzic2,na.rm=TRUE), mean(tb$kzic3,na.rm=TRUE), mean(tb$kzic4,na.rm=TRUE))
    kzi_mean_prc = kzi_mean / sum(kzi_mean,na.rm=TRUE) * 100
    
    kzi_var = c(var(tb$kzic1,na.rm=TRUE), var(tb$kzic2,na.rm=TRUE), var(tb$kzic3,na.rm=TRUE), var(tb$kzic4,na.rm=TRUE))
    kzi_var_prc = kzi_var/sum(kzi_var)*100
    
    tb_kz = add_row(tb_kz,'Process'='KZi','Component'=component_fct,'Mean'=kzi_mean,'Mean_prc'=kzi_mean_prc,'Variance'=kzi_var,'Variance_prc'=kzi_var_prc)
  }
  
  if ( dosma ) {
    sma_mean = c(mean(tb$smac1,na.rm=TRUE), mean(tb$smac2,na.rm=TRUE), mean(tb$smac3,na.rm=TRUE), mean(tb$smac4,na.rm=TRUE))
    sma_prc = sma_mean / sum(sma_mean,na.rm=TRUE) * 100
    sma_var = c(var(tb$smac1,na.rm=TRUE), var(tb$smac2,na.rm=TRUE), var(tb$smac3,na.rm=TRUE), var(tb$smac4,na.rm=TRUE))
    sma_var_prc = sma_var / sum(sma_var,na.rm=TRUE) * 100

    ## Add to tb_kz for plotting
    tb_kz = add_row(tb_kz,'Process'='SMA','Component'=component_fct,'Mean'=sma_mean,'Mean_prc'=sma_prc,'Variance'=sma_var,'Variance_prc'=sma_var_prc)
  }

  ## Organize factors in summary tibble:
  tb_kz$Process <- as.factor(tb_kz$Process)
  tb_kz$Process <- fct_relevel(tb_kz$Process,'TS','KZ','KZi','SMA')

  tb_kz$Component <- as.factor(tb_kz$Component)
  tb_kz$Component <- fct_relevel(tb_kz$Component,c('All',component_str))

  tb_kz$Stdev <- sqrt(tb_kz$Variance)
  tc_kz <- add_row(tc_kz, Variable='Stdev', Units=varunits,
                   Sites=stidsct, Duration=duration_str,
                   Source=varsct)
  
  
  doplotbyfilter <- 1 # ts overlay all components for one filter
  dotimevar <- 1 # timevar for each filter
  doplotbycomponent <- 1 # ts overlay one component from all filters
  dobars <- 1 # summary of mean/var/stdev
  
  np = 0
  p_filter = list()
  if ( doplotbyfilter || dotimevar ) {
    for ( filtertype in afilters ) {
      ts_plot = c('date',vselect)
      for ( component in component_str ) {
        ts_plot = append(ts_plot,sprintf('%s%s',filtertype,tolower(component)))
      }
      fct_plot = factor(ts_plot,levels=ts_plot)
      tb_plot <- tb %>% select(all_of(ts_plot)) %>%
        gather(key='Component',value='Concentration',-date)

      if ( dolog && dopexp ) {
        ## Convert back to concentration units:
        tb_plot$Concentration = 10^(tb_plot$Concentration)
      }
      
      ## Order plot for proper overlay:
      tb_plot$Component = factor(tb_plot$Component,levels=fct_plot)
      
      if ( doplotbyfilter ) {
        ## Make time series plot:
        np = np + 1
        pts = ggplot(tb_plot, aes(x=date, y=Concentration))+
          geom_line(aes(color=Component))+
          ggtitle(sprintf('%s time series (%s, %s)',filtertype,vselect,stidsct))
        show(pts)
        ggsave(sprintf('%s_tsf_%s.png',flrootout,filtertype), width=ggwidth*2, height=ggheight, units="cm", dpi=ggdpi)
        
        p_filter = c(p_filter,list(pts))
      }
      
      ## Make time variation plot with openair:
      if ( dotimevar ) {
        timeVariation(tb_plot, pollutant='Concentration',
                      group='Component', normalise=FALSE,
                      main=sprintf('%s %s (%s) by component\n%s', filtertype, varsct, varunits, date_str))
        if ( plot2file ) {
          flout = sprintf('%s_tvar_%s.png',flrootout,filtertype)
          dev.copy(png, flout, width=fgwidth, height=fgheight)
          dev.off()
          printf('Time variation plot created: %s',flout)
        }
      }
    }
  }

  np = 0
  p_component = list()
  if ( doplotbycomponent ) {
    for ( component in component_str ) {
      ts_plot = c('date',vselect)
      for ( filtertype in afilters ) {
        ts_plot = append(ts_plot,sprintf('%s%s',filtertype,tolower(component)))
      }
      fct_plot = factor(ts_plot,levels=ts_plot)
      tb_plot <- tb %>% select(all_of(ts_plot)) %>%
        gather(key='Component',value='Concentration',-date)

      if ( dolog && dopexp ) {
        ## Convert back to concentration units:
        tb_plot$Concentration = 10^(tb_plot$Concentration)
      }
      
      ## Order plot for proper overlay:
      tb_plot$Component = factor(tb_plot$Component,levels=fct_plot)
      
      ## Make time series plot:
      np = np + 1
      ptsc = ggplot(tb_plot, aes(x=date, y=Concentration))+
        geom_line(aes(color=Component))+
        ggtitle(sprintf('%s time series (%s, %s)',component,vselect,stidsct))
      show(ptsc)
      ggsave(sprintf('%s_tsc_%s.png',flrootout,tolower(component)), width=ggwidth*2, height=ggheight, units="cm", dpi=ggdpi)

      p_component = c(p_component,list(ptsc))
    }
  }

  if ( dobars ) {
    ## Pivot data to make single plot with both mean and stdev in conc units:
    tb_kzl <- pivot_longer(tb_kz,cols=c('Mean','Stdev'),names_to='Metric',values_to='Values')
    tb_kzl$ProcMetric  <- interaction(tb_kzl$Process,tb_kzl$Metric)
    pb = ggplot(tb_kzl,aes(x=ProcMetric,y=Values,fill=Component)) +
      geom_bar(stat='identity') +
      ggtitle(sprintf('TS Mean using KZ, KZi and SMAS (%s, %s)',vselect,stidsct))
    show(pb)
    ggsave(paste0(flrootout,"_a_conc.png"), width=ggwidth, height=ggheight, units="cm", dpi=ggdpi)
    
    ## Pivot data to make single plot with percentage mean and variance:
    tb_kzlf <- pivot_longer(tb_kz,cols=c('Mean_prc','Variance_prc'),names_to='Metric',values_to='Percentage')
    tb_kzlf$ProcMetric  <- interaction(tb_kzlf$Process,tb_kzlf$Metric)
    pbf = ggplot(tb_kzlf,aes(x=ProcMetric,y=Percentage,fill=Component)) +
      geom_bar(stat='identity') +
      ggtitle(sprintf('TS Fraction using KZ, KZi and SMAS (%s, %s)',vselect,stidsct))
    show(pbf)
    ggsave(paste0(flrootout,"_a_prc.png"), width=ggwidth, height=ggheight, units="cm", dpi=ggdpi)
  }
  
  ## Create tibble with global information:
  tb_global <- tibble(Variable = c('Title','Varsct','Author','Date','Comment'),
                      Value = c('Timeseries Analysis using KZ KZi SMA',varsct,user_name,as.character(Sys.time()),'Automatically generated by tbe_tsdecomp.R'))
  
  ## For now, use code from saq_create_tbe.R
  ## xxx: should put into a function
  if ( dotbe ) {
    tb_ts <- data.frame(tb)
    tc_ts <- data.frame(tc)
    tb <- tb_kz
    tc <- tc_kz
    ## use tc_sites_trans for both tbe and xlsx
    ## xxx: there are common things in tbe and xlsx, and multiple tables should be handled in a loop
    tc_trans <- df_transpose(tc)
    colnames(tc_trans)[1] <- 'TBL FilterResults'
    tc_trans[[1]] <- sprintf('ATT %s',tc_trans[[1]])
    if ( dotbesites ) {
      tc_sites_trans <- df_transpose(tc_sites)
      colnames(tc_sites_trans)[1] <- 'TBL Sites'
      tc_sites_trans[[1]] <- sprintf('ATT %s',tc_sites_trans[[1]])
    }
    
                                        # write to single tbe file
    fl_tbe = paste(flrootout,'_tbe.csv',sep='')

                                        # Write Global Information:
    tbout <- add_column(tb_global,'TBL Global'='',.before=1)
    tbout[1,1] <- 'BGN'; tbout[nrow(tbout),1] <- 'EOT Global'
    write_csv(tbout,fl_tbe,append=FALSE,col_names=TRUE)
    write(',,,',fl_tbe,append=TRUE) # I want a blank line between tables for clarity

    if ( dotbesites ) {
                                        # Write Site Information:
      write_csv(tc_sites_trans,fl_tbe,append=TRUE,col_names=TRUE)
      tb_displayname = sprintf('%s (%s)',tc_sites$Variable,tc_sites$Units)
      tbout <- tb_sites; names(tbout) <- tb_displayname
      tbout <- add_column(tbout,'ATT DisplayName'='',.before=1)
      tbout[1,1] <- 'BGN'; tbout[nrow(tbout),1] <- 'EOT Sites'
      if ( any(tolower(names(tc_sites))=='displayname') ) {
        write_csv(tbout,fl_tbe,append=TRUE,col_names=FALSE)
      } else {
        write_csv(tbout,fl_tbe,append=TRUE,col_names=TRUE)
      }
      write(',,,',fl_tbe,append=TRUE) # I want a blank line between tables for clarity
    }

                                        # Write Time Series:
    write_csv(tc_trans,fl_tbe,append=TRUE,col_names=TRUE)
    tb_displayname = sprintf('%s (%s)',tc$Variable,tc$Units)
    tbout <- tb;
                                        # convert date to string for csv (write_csv writes times as ISO8601 only)
    if ( "date" %in% names(tbout) ) {
      tbout$date <- as.character(tbout$date)
    }
    names(tbout) <- tb_displayname
    tbout <- add_column(tbout,'ATT DisplayName'='',.before=1)
    tbout[1,1] <- 'BGN'; tbout[nrow(tbout),1] <- 'EOT Timeseries'
    if ( any(tolower(names(tc))=='displayname') ) {
      write_csv(tbout,fl_tbe,append=TRUE,col_names=FALSE)
    } else {
                                        # If we do not have display names in tc, then write out col_names here:
      write_csv(tbout,fl_tbe,append=TRUE,col_names=TRUE)
    }
  }

  printf('Wrote %s with %s from %s',fl_tbe,varsct,flroot)

  return( list(result=tb, error=NULL) )  
}
