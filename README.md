
# mmaqshiny v1.1: R package to explore air quality mobile monitoring data  

[![status](https://joss.theoj.org/papers/4640e0e9a6f03a68833905bf378a0652/status.svg)](https://joss.theoj.org/papers/4640e0e9a6f03a68833905bf378a0652)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3911659.svg)](https://doi.org/10.5281/zenodo.3911659)
[![R-CMD-check](https://github.com/meenakshi-kushwaha/mmaqshiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/meenakshi-kushwaha/mmaqshiny/actions/workflows/R-CMD-check.yaml)

2021-10-19: Add in's for v1.1!!
- Added two new instruments DustTrak 8533 and Equinox RH sensor.
- All codes now changed to functional programs. 

2021-01-04: Add in's!!
- Added qq plot and density plot.

2020-09-23: Bug fixes!!
- Changed the code to tidy version! Usage of reactive elements and joined table preview (removed repeated code of joining and manipulation).
- Fixed shinyjs update, shifting from XML to xml2 happening soon!
## Summary

`mmaqshiny` is for analysing, visualising and spatial plotting of high-resolution air quality data collected by specific devices installed on a moving platform.  With the click of a button, the app generates: summary statistics, time series plots and spatial map of pollutant concentrations. This app reduces the time consumed for analysing each pollutant individually. It helps check the quality of the data at near real time (same day) and instantly visualise pollution hotspots. The time series plots of each pollutant help in understanding the temporal patterns of concentrations and performance of the instruments. 

High frequency (1 Hz) data of PM<sub>2.5</sub> (mass concentrations of particulate matter with size less than 2.5 microns), Black carbon mass concentrations (BC), ultra-fine particle number concentrations, carbon-di-oxide (CO<sub>2</sub>) along with GPS coordinates and relative humidity (RH) data are collected by some popular portable instruments (TSI DustTrak-8530 / 8533, Aethlabs microAeth-AE51, TSI CPC3007, LICOR Li-850, Garmin GPSMAP 64s, Omega USB RH probe and Equinox RH sensor respectively) can be handled by this package. The package incorporates device specific cleaning and correction algorithms. RH correction is applied to DustTrak PM<sub>2.5</sub> following a method described in @Chakrabarti:2004. If required, user can also input linear regression coefficients for correcting the PM<sub>2.5</sub> data.
An example of DustTrak PM<sub>2.5</sub> raw and corrected data, using slope = 0.21, intercept = 11.1 is shown below.

| ![\Uncorrected{fig: `DT8530_PM2.5`}](inst/images/Image9.JPG) | 
|:--:| 
| *Uncorrected* `DT8530_PM2.5` |


| ![\Corrected{fig: `DT8530_PM2.5_Ref`}](inst/images/Image10.JPG) | 
|:--:| 
| *Corrected* `DT8530_PM2.5_Ref` |

The package cleans BC data for the vibration generated noise, by adopting a statistical procedure as explained in @Apte:2011, followed by a loading correction as suggested by @Ban-Weiss:2009. For the ultra-fine particle number concentration data, provision is given for dilution correction factor (if a diluter is used with CPC3007; default value is 1). 


The package joins the raw, cleaned and corrected data from the above mentioned instruments and generates a downloadable csv file. It accepts multiple files for each parameter. 

The package requires GPS file (.gpx) as a mandatory input along with timezone which can be selected using the dropdown menu (a link to all accepted timezone formats in R is also included). All other pollutant files are optional. A testing data set is provided in the "data" folder inside inst/shiny; with each instrument folder containing 2 days of data.


The example data included [here](https://github.com/meenakshi-kushwaha/mmaqshiny/tree/master/inst/shiny/data) contains with following file names - 

- xxx_Garmin correspond to Garmin GPS location files
- xxx_AE51 correspond to AE51 BC files
- xxx_DT8530 correspond to DustTrak 8530 PM<sub>2.5</sub> files
- xxx_DT8533 correspond to DustTrak 8533 PM files
- xxx_RH correspond to RH files
- xxx_RH172 correspond to Equinox RH files
- xxx_CPC3007 correspond to CPC3007 Particle Concentration files
- xxx_LI_COR correspond to LI-COR CO<sub>2</sub> files


The output is displayed in five different tabs.

1) `Joined File` displays all cleaned and joined data.
2) `Summary` displays summary statistics for each parameter.
3)  `Plots` displays interactive  time series line plots, density and qq plots for the selected parameter. 
4) `Map` provides a spatial map for the user selected pollutant on an OpenStreetMap basemap. 
5) `Alarm and Settings` tab displays each instrument's settings and alarms (if any).


## Limitations

1) It has provision for only linear correction coefficients of PM<sub>2.5</sub>.
2) It is instrument specific.


## Installation

`mmaqshiny` can be installed from [github](https://github.com/).

Load and run `mmaqshiny` as follows:

``` r
install.package("devtools")
devtools::install_github("meenakshi-kushwaha/mmaqshiny")
mmaqshiny::mmaqshiny_run()
```
A preloaded dataset appears which is a joined file of sample data collected during a mobile monitoring campaign in Bangalore, India.

## User Guide

To use the app, follow the steps below.

1. User needs to add the timezone of data collection. This is a mandatory step. Since all the files are joined using the date time column. 

2. Add .gpx files from GPSMAP64s (location data). NOTE: A warning message will appear if input files are from different dates. This is a mandatory step too, since all the files are joined to GPS file, since mobile monitoring studies are based on location this file is required.

3. Add pollutant .csv files as downloaded from the instruments (the number of files in each instrument and also the number of different instrument's data available does not matter since the app is capable of handling any number).

4. User can add slope and intercept if a linear correction is required for the DustTrak measured PM<sub>2.5</sub>. 

![\label{fig:example}](inst/images/Image1.JPG)

5. User can also input a Dilution factor if diluter is used for CPC-3007. Default value is 1 (no diluter).

6. Click join button. 

7. Click the download button to download the joined csv file.

![\label{fig:example}](inst/images/Image2.JPG)

![\label{fig:example}](inst/images/Image3.JPG)

8. Summary tab allows user to check the summary statistics for each of the pollutant. 

![\label{fig:example}](inst/images/Image4.JPG)

9. The Plots tab now allows user to select a parameter to plot time series, density plot and qq plot of all the parameters. 

![\label{fig:example}](inst/images/Image11.JPG)

![\label{fig:example}](inst/images/Image12.JPG)

![\label{fig:example}](inst/images/Image13.JPG)

10. The Map tab visualises interactive maps for spatial visualization of selected pollutant.

![\label{fig:example}](inst/images/Image6.JPG)

![\label{fig:example}](inst/images/Image7.JPG)

11. Alarms and Settings tab display any status errors during data collection and instrument setting. 

![\label{fig:example}](inst/images/Image8.JPG)


## Glossary
- `Latitude`:Latitude
- `Longitude`: Longitude
- `BC`: Raw BC data 
- `BC_NR`: Noise removed BC 
- `BC_NR_LC`: Noise removed and loading corrected BC
- `PM2.5`: Raw PM2.5 from DustTrak 8530
- `PM2.5_RHC`: RH corrected PM2.5 from DustTrak 8530
- `PM2.5_RHC_Ref`: Reference and RH corrected PM2.5 from DustTrak 8530
- `PM2.5_Ref`: Reference corrected PM2.5 from DustTrak 8530
- `PM2.5`: Raw PM2.5 from DustTrak 8533
- `PM2.5_RHC`: RH corrected PM2.5 from DustTrak 8533
- `PM2.5_RHC_Ref`: Reference and RH corrected PM2.5 from DustTrak 8533
- `PM2.5_Ref`: Reference corrected PM2.5 from DustTrak 8533
- `Particle Concentration`: Dilution corrected ultra-fine particle number concentration
- `CO2`: CO<sub>2</sub> data


## Community guidelines

1. [Contribute to the software](.github/CONTRIBUTING.md)

2. Report issues or problems with the software / Seek Support

- Please open an issue in the [issue tracker of the project.](https://github.com/meenakshi-kushwaha/mmaqshiny/issues)

3. Contributors must adhere to the [Code of Conduct](.github/CODE_OF_CONDUCT.md).



## Instrument Description


1. DustTrak 

It utilises the well-established aerosol light scattering technique to estimate the real-time aerosol mass loadings and works at a flow rate of 3 LPM (liters per minute). Detailed specifications can be [found here.](https://tsi.com/products/aerosol-and-dust-monitors/dust-monitors/dusttrak-ii-aerosol-monitor-8530/)


2. microAeth AE51

It is a highly sensitive, palm-held and battery-operated instrument designed for measuring the optically-absorbing BC component of aerosol particles.It measures the rate of change in absorption of transmitted light (880 nm) due to continuous collection of aerosols load on to the filter ticket and has a wide dynamic range of measurement from 0 to 1 mg/m³. More details of AE51 can be [found here.]( https://aethlabs.com/microaeth/ae51/overview)

3. Condensation Particle Counter 3007 (CPC)

It is an alcohol based handheld instrument used to measure ultra-fine particles. It works on the optical detection principle, and operates at a flow rate of 0.7 LPM.The instrument detects and measures the particles in the size range of 10 nm to > 1 µm.  More technical details of the instrument can be [found here.](https://www.tsi.com/condensation-particle-counter-3007/)

4. LI-COR 850

It is a CO<sub>2</sub>/H<sub>2</sub>O gas analyzer which has a measurement range of 0-20,000 ppm and accuracy of 1.5%. For logging the data it requires a laptop with the software called LI-COR. More details can be [found here.]( https://www.licor.com/env/products/gas_analysis/LI-830_LI-850/)

5. Omega RH-USB

It is an instrument used for measuring RH and temperature. It has an accuracy of ±3% for Relative humidity and  ±1°C (±1.8°F) for temperature. It requires a software to log the data called TRH central. The frequency of data logging can be changed as per need using this software. For more [information click here.]( https://www.omega.com/en-us/calibration-equipment/handheld-calibrator/p/RH-USB-Series)

6. GPSMAP-64s

It works on the ‘trilateration’ mathematical principle of GPS and usually connects to 4 satellites to give the accurate location. More technical details of the instrument can be [found here.]( https://www.garmin.co.in/products/outdoor/gpsmap64s-sea/)


