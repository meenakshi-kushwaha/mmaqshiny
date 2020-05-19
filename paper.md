---
title: 'mmaqshiny v1.0: R-Shiny package to explore air quality mobile monitoring data'
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
 - name: ILK Labs, Bengaluru, India
   index: "1"
 - name: Center for Study of Science, Technology & Policy, Bengaluru, India
   index: "2"
date: 19 May 2020
bibliography: paper.bib

---

## Summary


Mobile monitoring of air quality is being gradually adapted by research groups and governments to complement their existing stationary monitoring facilities, in order to understand the hyper-local nature of air pollution levels.

The R-Shiny package `mmaqshiny` is for analysing, visualising and spatial-mapping of high-resolution air quality data collected by specific devices installed on a moving platform.

High frequency (1-Hz) data of PM2.5 (mass concentrations of particulate matter with size less than 2.5 microns); black carbon mass concentrations (BC); ultra-fine particle number concentrations; carbon-di-oxide; GPS coordinates and relative humidity (RH) data are collected by some popular portable instruments (TSI DustTrak-8530, Aethlabs microAeth-AE51, TSI CPC3007, LICOR Li-850, Garmin GPSMAP 64s and Omega USB RH probe, respectively), can be handled by this package. The package incorporates device-specific cleaning and correction algorithms. RH correction is applied to DustTrak PM2.5 following a method described in [@Chakrabarti:2004]. If required, user can also input linear regression coefficients for correcting the PM2.5 data. The package cleans BC data for the vibration generated noise, by adopting a statistical procedure as explained in [@Apte:2011], followed by a loading correction as suggested by [@Ban-Weiss:2009]. For the ultra-fine particle number concentration data, provision is made for dilution correction factor (if a diluter is used with CPC3007; default value is 1).

The package joins the raw, cleaned and corrected data from the above mentioned instruments and generates a downloadable csv file. It accepts multiple files for each parameter. The input files should have a date prefix of the format `yyyy_mm_dd` in their file names. The package can process multiple files for a given date at a time and the file name prefix is used to perform the check. An error message will be generated if the prefix is not matched between various pollutant filenames.

The package requires GPS file (.gpx) as a mandatory input along with timezone (a link to all accepted timezone formats in R is also included). All other pollutant files are optional.


The output is displayed in five different tabs.

1) `Joined File` displays raw, cleaned and corrected data
2) `Summary` displays summary statistics for each parameter
3)  `Plots` displays interactive  time series line plots for all parameters
4) `Map` provides a spatial map for the user selected pollutant on OpenStreetMap background.
5) `Alarm and Settings` tab displays each instruments settings and alarms (if any)


## Limitations

1) it can handle only single day data at a time
2) there is provision for linear correction coefficients of PM2.5 only
3) it is instrument specific
4) file renaming (with date prefix) is required


## Installation

`mmaqshiny` can be intsalled from [github](https://github.com/).

Load and run `mmaqshiny` as follows:

``` r
devtools::install_github("meenakshi-kushwaha/mmaqshiny")
library(mmaqshiny)
mmaqshiny::mmaqshiny_run()
```
A preloaded dataset appears which is a joined file of sample data collected during a mobile monitoring campaign in Bangalore, India.

## Community guidelines

1. Contribute to the software

- Please open an issue in the issue tracker of the project that describes the changes you would like to make to the software and open a pull request with the changes.

2. Report issues or problems with the software / Seek Support

- Please open an issue in the [issue tracker of the project.](https://github.com/meenakshi-kushwaha/mmaqshiny/issues)

# Acknowledgements

We wish to thank Prof. Julian Marshall (University of Washington, Seattle), Prof. Joshua Apte (University of California, Berkeley), Dr. Jai Asundi (CSTEP), Dr. Maëlle Salmon, Dr. María Florencia D’Andrea and R Ladies community for their help and support.

# References
