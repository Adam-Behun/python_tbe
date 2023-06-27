## dku_boxplot.R: Diurnal / Weekly Boxplot of multiple sites
##
## BdF, 13 Mar 2023

rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggplot2)
library(openair)

## we need printf from bdf_utils.R:
source('bdf_utils.R')

## Input directory and file name: indir, flroot
indir = ''
#flroot = 'dku_bluesky_sylhet_ns2_20220421_20230202_coords_utc'; ftype = 1
flroot = 'dku_bluesky_sylhet_ns2_20220421_20230202_coords_15min_lt'; ftype = 2

flin = sprintf('%s%s.csv',indir,flroot)
flrootout = sprintf('p%s',flroot)

varselect = 'pm25'
timezone_output = 'Asia/Dhaka'
seasonsct = NULL # NULL, 'Dry', 'Wet'

## Read data:
tb <- read_csv(flin)

## Sites to select: either specify in asites, or use unique to select all
##asites = c('65. Sylhet MPU','66. Sylhet Ambarkhana')
asites = unique(tb$sitename)

if ( ftype == 1 ) {
    tb$date <- with_tz(tb$date,timezone_output) # for files in UTC, shift to local
} else {
    tb$date <- force_tz(tb$date,timezone_output) # for files in local, force local
    tb <- tb %>% select(-'...1')
}

printf('Read %d rows from %s to %s',nrow(tb),min(tb$date),max(tb$date))

tb$sitename = as.factor(tb$sitename)

## create vector of seasons for filtering:
tb_season <- tibble(name=c('Dry','Wet','Dry'),start_mmdd=c(1,401,1101))
tb$season0 <- base::cut(100*month(tb$date)+day(tb$date),
                        breaks=c(tb_season$start_mmdd-1,1231),
                        labels=tb_season$name)

## create time of day categories for plotting:
tb$timeofday = c('Night','Trans','Day','Trans','Night')[cut(hour(tb$date),c(-1,8.5,10.5,16.5,18.5,24))]
tb$timeofday = as.factor(tb$timeofday)


## filter data by season if required:
if ( !is.null(seasonsct) ) {
    tb <- tb %>% filter(season0==seasonsct)
    if ( nrow(tb) == 0 ) {
        stop(sprintf('No data found, check data selection in seasonsct: %s',seasonsct),call.=FALSE)
    } else {
        printf('Filtered tb by season %s: %d rows from %s to %s',seasonsct,nrow(tb),min(tb$date),max(tb$date))
    }
}

## Create a diurnal boxplot:
pdiel = ggplot(tb) +
    geom_boxplot(aes(x=hour(date),y=pm25,group=interaction(hour(date),sitename),col=sitename),outlier.colour=NULL)

show(pdiel)

ggsave(sprintf("%s_bp_diurnal.png",flrootout),width=12,height=10,units="cm",dpi=200)
printf('Diurnal boxplot saved to file %s_bp_diurnal.png',flrootout)

## Create a day of week boxplot:
tb$wday = wday(tb$date,label=TRUE,week_start=7)

pweekly = ggplot(tb) +
    geom_boxplot(aes(x=wday,y=pm25,group=interaction(wday,sitename),col=sitename),outlier.colour=NULL)

show(pweekly)

ggsave(sprintf("%s_bp_weekly.png",flrootout),width=12,height=10,units="cm",dpi=200)
printf('Weekly boxplot saved to file %s_bp_weekly.png',flrootout)

