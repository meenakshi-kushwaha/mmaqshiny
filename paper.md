---
title: 'mmaqshiny v1.0: R-Shiny package to explore air-pollution mobile monitoring data'
tags:
  - R
  - Mobile monitoring 
  - air quality
  - shiny
authors:
  - name: Adithi R. Upadhya
    orcid: 0000-0002-1764-1379
    affiliation: "1"
  - name: Pratyush Agarwal
    affiliation: "1"
  - name: Sreekanth Vakacherla
    orcid: 0000-0003-0400-6584
    affiliation: "2"
  - name: Meenakshi Kushwaha
    affiliation: "1"
affiliations:
 - name: ILK Labs
   index: "1"
 - name: CSTEP
   index: "2"
date: 19 May 2020
bibliography: paper.bib

---

## Summary


Mobile monitoring of air quality is being gradually adapted by research groups and governments to complement their existing stationary monitoring facilities, to understand the hyper-local nature of the air quality levels.

The R-Shiny package `mmaqshiny` is for analysing, visualising and spatial-mapping of high-resolution air quality data collected by specific devices installed on a moving platform.

High frequency `1-Hz` data of PM2.5 (mass concentrations of particulate matter with size less than 2.5 microns), Black carbon mass concentrations (BC), ultra-fine particle number concentrations, carbon-di-oxide along with GPS coordinates and relative humidity (RH) data are collected by some popular portable instruments (`TSI DustTrak-8530, Aethlabs microAeth-AE51, TSI CPC3007, LICOR Li-850, Garmin GPSMAP 64s, Omega USB RH probe` respectively) can be handled by this package. The package incorporates device-specific cleaning and correction algorithms. RH correction is packagelied to DustTrak PM2.5 following a method described in @Chakrabarti:2004. If required, user can also input input linear regression coefficients for correcting the PM2.5 data. The package cleans BC data for the vibration generated noise, by adopting a statistical procedure as explained in @Apte:2011, followed by a loading correction as suggested by @Ban-Weiss:2009. For the ultra-fine particle number concentration data, provision is given for dilution correction factor (if a diluter is used with CPC3007; default value is 1).

The package joins the raw, cleaned and corrected data from the above mentioned instruments and generates a downloadable csv file. It accepts multiple files for each parameter. The input files should have a date prefix of the format `yyyy_mm_dd`. The package can process files from the same date at one time and the file prefix is used to perform the check. If the user inputs files from different dates an error message will be generated.

The package requires GPS file (.gpx) as a mandatory input along with the input timezone (a link to all accepted timezone formats in R is also included). All other pollutant files are optional.

All the raw and processed data will be displayed in the `Joined File` tab, while a basic statistical summary of each parameter is provided in the `Summary` tab. The `Plots` tab displays interactive  time series line plots (using plotly) for select parameters, while the `Map` tab provides a spatial map for the user selected pollutant. `Alarm and Settings` tab displays each instruments’ settings and alarms (if any).


The output is displayed in five different tabs.

1) `Joined File` displays all cleaned and joined data
2) `Summary` displays summary statistics for each parameter,
3)  `Plots` displays interactive  time series line plots for all parameters.
4) `Map` provides a spatial map for the user selected pollutant on an OpenStreetMap background.
5) `Alarm and Settings` tab displays each instruments settings and alarms (if any).


## Limitations

1) handles single day data at a time
2) only provision for linear correction coefficients of PM2.5
3) instrument specific
4) file renaming (with date prefix) is required


## Installation

`mmaqshiny` can be intsalled from [github](https://github.com/) with:

``` r
devtools::install_github("meenakshi-kushwaha/mmaqshiny")
```

Load and run the app as follows:

``` r
library(mmaqshiny)
mmaqshiny::mmaqshiny_run()
```
A preloaded dataset appears which is a joined file of sample data collected during a mobile monitoring campaign in Bangalore, India.

## Glossary
- `Latitude`:Latitude
- `Longitude`: Longitude
- `AE51_BC`: Raw BC data 
- `AE51_BC_NR`: Noise removed BC 
- `AE51_BC_NR_LC`: Noise removed and loading corrected BC
- `DT8530_PM2.5`: Raw PM2.5
- `DT8530_PM2.5_RHC`: RH corrected PM2.5
- `DT8530_PM2.5_RHC_Ref`: Reference and RH corrected PM2.5
- `DT8530_PM2.5_Ref`: Reference corrected PM2.5
- `CPC3007_Particle Concentration`: Dilution corrected ultra-fine particle number concentration
- `Li-COR_CO2`: CO2 data

## Community guidelines

1. Contribute to the software

- Please open an issue in the issue tracker of the project that describes the changes you would like to make to the software and open a pull request with the changes.

2. Report issues or problems with the software / Seek Support

- Please open an issue in the [issue tracker of the project.](https://github.com/meenakshi-kushwaha/mmaqshiny/issues)


# Acknowledgements

We wish to thank Prof. Julian Marshall (University of Washington, Seattle), Prof. Joshua Apte (University of California, Berkeley), Dr. Maëlle Salmon, Dr. Florencia D'Andrea and R Ladies community for their help and support.

# References
