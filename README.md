# TBE Library project for Open Source with SLU

Here's an overview of the files in the project.

### Files
- **Bdefoy_20220126_data_analysis_for_saaq.docx**: White paper for project data format.
- **Saq_bluesky_dku_20210715_20230131_inv_tbe.csv**: Example TBE file with inventory of Bangladesh Low Cost Sensor (LCS) data.
- **Saq_bgd_2_dhaka_u_pm25_2021_2022_tbe.csv**: Example TBE file with pollutant concentration (pm25), meteorological data and calculated columns.

### Entry Point
- **dku_read_blueskyv2b.R**: Adjust paths to read files in Bangladesh_level1 and create flat csv file.
  - To create tbe file, refer to `saq_create_tbe.R` in Aqsebs which created `saq_bgd_2_dhaka_u_pm25_2021_2022_tbe.csv`. Note that this is not set up to read Bangladesh_level1 (although it would not be much work to adjust it, saq_proc_sites.R is the driver routine). 

### Bangladesh: 
- Source data for PM25, we will use pm25 from Level1.csv files

### Bangladesh_level1: 
- All source data with Level1.csv files only
  - At this point, we just want to work with Level1.csv and not worry about anything else.

### Dku_bluesky_analysis:
- R code to read Level1.csv files, create flat csv file with specific files, do preliminary analysis.
- See readme file inside directory

### Aqsebs_20230525:
- Snapshot of my first effort at creating an R-Package to read the data and create TBE files.
- **saq_create_tbe.R** is the program used to generate time series of data in TBE format.

### bdf_utils.R 
- contains utility routines. In particular, it contains `saq_sitename2id` to create a site ID from a site name.

### Aqslcs: 
- BdF test programs to try things out in python:
  - **Saq_read_era5.py**: Read NetCDF files (era5_202101_met_dhaka.nc) and create TBE file (era5_2375n_9050e_202101_202102_tbe.csv)
  - **Saq_read_tbe.py**: test program to read TBE file into python data structure. (df with the data, dg with the metadata)
  - **Tbe_timeseries.py**: test at reading TBE file and making a plot

### Rstats_examples:
- **ebs_read_tbe.R**: Read TBE files into R tibbles: tb with the data, tc with the metadata
- **saq_create_tbe.R**: Create TBE file in R from data and metadata tibbles
- **dku_read_bluesky.R**: read previous version of data like Level1.csv, merge, remove duplicates, create uniform intervals, make hourly plot. Creates a flat csv (not TBE).
- **Saq_report_bgd_3_mohakhali.pdf**: Automated report from TBE file containing pm25 concentrations and meteorological data for one sensor.

## To do short term:
1. Python script to read data in Bangladesh and create TBE file for each site with the time series measurements, and single TBE file with data inventory
2. Python script to create TBE from gridded ERA5 NetCDF file
3. Python script to create merged TBE file from multiple TBE files - use a TBE file as an input file with filename and variables to merge
4. Python script to do data exploration plots. See for example Openair package in R (eg. saq_report_bgd_3_mohakhali.pdf)

## To do long term:
1. Machine learning to simulate pm25 given different inputs (meteorology, time factors (time of day, day of week, time of year, …))
2. Develop methods for interpretable machine learning.
    - I am planning on testing this using h2o in R for now.
    - I’m thinking once you have python tools set up, it would be better to use scikit-learn or tensorflow or something like that.
