
# mmaqshiny v1.0: R-Shiny package to explore air-pollution mobile monitoring data 

<!-- badges: start -->
<!-- badges: end -->

## Summary

Mobile monitoring of air quality is being gradually adapted by research groups and governments to complement their existing stationary monitoring facilities, to understand the hyper-local nature of air pollution.

The R-Shiny package `mmaqshiny` is for analysing, visualising and spatial-mapping of high-resolution air quality data collected by specific devices installed on a moving platform. 

High frequency `1-Hz` data of PM2.5 (mass concentrations of particulate matter with size less than 2.5 microns), Black carbon mass concentrations (BC), ultra-fine particle number concentrations, carbon-di-oxide along with GPS coordinates and relative humidity (RH) data are collected by some popular portable instruments (`TSI DustTrak-8530, Aethlabs microAeth-AE51, TSI CPC3007, LICOR Li-830, Garmin GPSMAP 64s, Omega USB RH probe` respectively) can be handled by this package. The package incorporates device-specific cleaning and correction algorithms. RH correction is packagelied to DustTrak PM2.5 following a method described in Chakrabarti et al., (2004). If required, user can also input input linear regression coefficients for correcting the PM2.5 data. The package cleans BC data for the vibration generated noise, by adopting a statistical procedure as explained in Apte et al., (2011), followed by a loading correction as suggested by Ban-Weiss et al., (2009). For the ultra-fine particle number concentration data, provision is given for dilution correction factor (if a diluter is used with CPC3007; default value is 1).

The package joins the raw, cleaned and corrected data from the above mentioned instruments and generates a downloadable csv file. It accepts multiple files for each parameter. The input files should have a date prefix of the format `yyyy_mm_dd`. The package can process files from the same date at one time and the file prefix is used to perform the check. If the user inputs files from different dates an error message will be generated. 

The package requires GPS file (.gpx) as a mandatory input along with the input timezone (a link to all accepted timezone formats in R is also included). All other pollutant files are optional. 

The output is displayed in five different tabs.

1) `Joined File` displays all cleaned and joined data 
2) `Summary` displays summary statistics for each parameter,
3)  `Plots` displays interactive  time series line plots for all parameters. 
4) `Map` provides a spatial map for the user selected pollutant on an OpenStreetMap background. 
5) `Alarm and Settings` tab displays each instrument's settings and alarms (if any).

## Limitations

1) Handles single day data at a time,
2) only provision for linear correction coefficients of PM2.5,
3) instrument specific, 
4) file renaming (with date prefix) is required


## Installation

You can install the released version of `mmaqshiny` from [github](https://github.com/) with:

``` r
devtools::install_github("meenakshi-kushwaha/mmaqshiny")
```

## Demo

Load and run the app as follows: 

``` r
library(mmaqshiny)
mmaqshiny::mmaqshiny_run()
```

A preloaded dataset appears which is a joined file of sample data collected during a mobile monitoring campaign in Bangalore, India.

## User Guide

To use the app for your own data follow the steps below.

1. The user needs to add the input timezone in the the text box. 

2. Add .gpx files for the GPSMAP64s - location file inputs. NOTE: A warning message will appear if input files are from different dates.

![\label{fig:example}](007.png)

3. Add .csv files for the pollutant data available (multiple files are to be selected all at once).

4. User can add the slope and intercept if a linear correction equation is available for the PM2.5 reference grade corrected. 

5. User can also input a Dilution factor if diluter is used for data collection. Default value is 1 (no dilutor).

6. Click the join button to generate a single joined file. 

7. Click the download button to download the joined csv file.

8. Summary tab allows user to check the summary statistics of each pollutant.

![\label{fig:example}](008.png)

9. The Plot tab displays time series plots for instant checks on instrument operation on field.

![\label{fig:example}](005.png)

10. The Map tab displays interactive maps for spatial visualization of select pollutant.

![\label{fig:example}](002.png)

11. Alarms and Settings tab display any staus errors and settings during data collection. 

![\label{fig:example}](003.png)


## Glossary
- `Latitude`:Latitude;
- `Longitude`: Longitude;
- `AE51_BC`: Raw BC data; 
- `AE51_BC_NR`: Noise removed BC; 
- `AE51_BC_NR_LC`: Noise removed and loading corrected BC; 
- `DT8530_PM2.5`: Raw PM2.5; 
- `DT8530_PM2.5_RHC`: RH corrected PM2.5; 
- `DT8530_PM2.5_RHC_Ref`: Reference and RH corrected PM2.5; 
- `DT8530_PM2.5_Ref`: Reference corrected PM2.5; 
- `CPC3007_Particle Concentration`: Dilution corrected ultra-fine particle number concentration;
- `CO2`: CO2 concentration


## Community guidelines

Report Issues:

- Questions, feedback, bug reports: please open an issue in the issue tracker of the project here.

Contribution to the software:

- Please open an issue in the issue tracker of the project that describes the changes you would like to make to the software and open a pull request with the changes. The description of the pull request must references the corresponding issue.


## Instrument Description

1. DustTrak 8530

A DustTrak is a portable mid-range monitoring instrument to measure PM 2.5 concentrations. It utilises the well-established aerosol light scattering technique to estimate the real-time aerosol mass loadings and works at a flow rate of 3 LPM (liters per minute).

2. MicroAeth AE51

It is a highly sensitive, palm-held and battery-operated instrument designed for measuring the optically-absorbing BC component of aerosol particles.It measures the rate of change in absorption of transmitted light (880 nm) due to continuous collection of aerosols load on to the filter ticket and has a wide dynamic range of measurement from 0 to 1 mg/m³.

3. Condensation Particle Counter (CPC 3007)

It is an alcohol based handheld instrument used to measure ultrafine particles. It works on the optical detection principle, and operates at a flow rate of 0.7 LPM.The instrument detects and measures the particles in the size range of 10 nm to > 1 µm. 


## Acknowledgements

We wish to thank Dr. Julian Marshall, Dr. Joshua Apte, Dr Maelle Salmon,Dr. Florencia D'Andrea and R Ladies community for their help and support.

