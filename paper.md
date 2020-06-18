---
title: 'mmaqshiny v1.0: R-Shiny package to explore air quality mobile monitoring data'
authors:
- affiliation: '1'
  name: Adithi R. Upadhya
  orcid: 0000-0002-1764-1379
- affiliation: '1'
  name: Pratyush Agrawal
- affiliation: '2'
  name: Sreekanth Vakacherla
  orcid: 0000-0003-0400-6584
- affiliation: '1'
  name: Meenakshi Kushwaha
date: "19 May 2020"
bibliography: paper.bib
tags:
- R
- Mobile monitoring
- air quality
- shiny
affiliations:
- index: '1'
  name: ILK Labs, Bengaluru, India
- index: '2'
  name: Center for Study of Science, Technology & Policy, Bengaluru, India
---

# Summary

Stationary air quality monitors do not capture spatial variations in air pollution. Mobile monitoring or “sensors on a mobile platform” is an increasingly popular approach to measure high resolution pollution data at the street level. Coupled with location data, spatial visualization of air quality parameters helps locate localized areas of high air pollution, also called hotspots. In this approach, portable sensors are mounted on a vehicle and driven on predetermined routes to collect high frequency data (1 Hz). Analyzing this data involves cleaning and combining location data with pollutant data from different instruments. Some instruments are sensitive to factors like signal attenuation, humidity, vibrations and require additional correction algorithms unique to each instrument. A typical measurement campaign involves collecting millions of data points and a high burden of quality assurance and quality control. Our package attempts to automate and simplify the process using a shiny app.

The R-Shiny package `mmaqshiny` is for analysing, visualising and spatial mapping of high-resolution air quality data collected by specific devices installed on a moving platform.  With the click of a button, the app generates: summary statistics, time series plots and spatial map of pollutant concentrations. This app reduces the time consumed for analysing each pollutant individually. It helps check the quality of the data at near real time (same day) and instantly visualise pollution hotspots. The time series plots of each pollutant help in understanding the temporal patterns of concentrations and performance of the instruments. This app will help both air pollution researchers and citizen scientists interested in conducting mobile measurements. 

Specific details of instruments and parameters measured can be found in the “Instrument Details” section. `mmaqshiny` package can handle high frequency (1 Hz) measurements collected by some popular portable instruments. Specifically, the following parameters are measured : PM2.5 (mass concentrations of particulate matter with size less than 2.5 microns), black carbon mass concentrations (BC), ultra-fine particle number concentrations; carbon dioxide (CO<sub>2</sub>), GPS coordinates and relative humidity (RH).

Under the hood, this app performs two basic functions: data correction and joining (location data + pollutant concentration data). As mentioned before, air pollution measurements, especially BC from microAeth, on a mobile platform are susceptible to vibration-related noise because of the road conditions and vehicle suspension efficiency. The package incorporates an algorithm to remove this noise following the @Apte:2011 method, followed by a loading correction as suggested by @Ban-Weiss:2009. PM<sub>2.5</sub> measurements from optical instruments are also sensitive to Relative Humidity (RH). To correct for RH, an algorithm is applied to DustTrak PM<sub>2.5</sub> following the @Chakrabarti:2004 method. Furthermore, for calibration of DustTrak PM<sub>2.5</sub> values, we have derived a calibration equation using a reference grade device. This  equation can vary with study location and season. Hence, several linear calibration equations are available in literature from individual studies. Researchers can derive their own equation as well. Accommodating for these options, the app allows users to input linear regression coefficients of their choice for calibrating the  PM<sub>2.5</sub> data. Lastly, on-road concentrations can be much higher than ambient concentrations and sometimes even exceed the measurement range of sensors. CPC3007 has a fixed dynamic measurement range, and number concentration measurements beyond this range can be biased. Traditionally, this is addressed by applying a diluter (characterised by a dilution factor) to the instrument inlet. Therefore, for the ultra-fine particle number concentration data from CPC3007, provision is made for a dilution correction factor (default value is 1, implying no diluter used). After data correction, the app joins the data to generate a downloadable csv file. 

The app is designed to accept multiple files for each parameter from a single day of measurements. The input file names should have a date prefix of the format `yyyy_mm_dd`. The app uses this prefix to check that all input files are from the same date and generates an error message if the prefixes don’t match. The package requires GPS file (.gpx) as a mandatory input along with timezone (a link to all accepted timezone formats in R is also included). All other pollutant files are optional.

Mobile monitoring is a relatively new technique and the instruments used are also highly customised. As of now, there are no applications which have been able to integrate the above mentioned devices. To the authors’ knowledge there are a few apps that deal with cleaning,  and visualization from a single instrument [@Salmon:2017]. In contrast, this app integrates data from several devices along with location data, combined with unique correction algorithms for different instruments and parameters as described above. 


### App Display

The output is displayed in five different tabs.

1) `Joined File` displays raw, cleaned and corrected data
2) `Summary` displays summary statistics for each parameter
3) `Plots` displays interactive  time series line plots for raw parameters
4) `Map` provides a spatial map for the user selected pollutant on OpenStreetMap basemap
5) `Alarm and Settings` tab displays each instruments settings and alarms (if any)


### Limitations

1) It can handle only single day data at a time
2) There is provision for only linear correction coefficients of PM<sub>2.5</sub> 
3) It is instrument specific
4) It is mandatory to rename files (with date prefix)


### Installation

`mmaqshiny` can be installed from [github](https://github.com/).

Load and run `mmaqshiny` as follows:

``` r
devtools::install_github("meenakshi-kushwaha/mmaqshiny")
library(mmaqshiny)
mmaqshiny::mmaqshiny_run()
```
A preloaded dataset appears which is a joined file of sample data collected during a mobile monitoring campaign in Bangalore, India.


### Instrument Details


| Instrument (model/ make) | Parameters | Units| Operating Principle | 
| :-----:|:-----:|:-----:|:-----:|
| GPSMAP 64s (Garmin) | Location (latitude, longitude, altitude) | °, °, m | Trilateration|
| USB RH probe (Omega) | Relative Humidity (RH) | % | Electrical detection | 
| Micro-Aethalometer (AE51, Aethlabs)  | Black Carbon mass concentration (BC) | ng/m³ | Light absorption technique |
| DustTrak II (DT8530, TSI) | PM<sub>2.5</sub> | mg/m³ |  Light scattering technique | 
| Li-850 (LICOR) | CO<sub>2</sub> | ppm | Non-Dispersive Infrared |
| CPC (3007, TSI) | Particle concentration | count/cm³ | Light scattering technique |



# Acknowledgements

We wish to thank Prof. Julian Marshall (University of Washington, Seattle), Prof. Joshua Apte (University of California, Berkeley), Dr. Jai Asundi (CSTEP), Dr. Maëlle Salmon, Dr. María Florencia D’Andrea and R Ladies community for their help and support.

# References
