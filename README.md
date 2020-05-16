
# mmaqshiny v1.0: R-Shiny package to explore air-pollution mobile monitoring data 

<!-- badges: start -->
<!-- badges: end -->

## Summary

Mobile monitoring of air pollution is being gradually adapted by research groups and governments to complement their existing stationary monitoring facilities, to understand the hyper-local nature of the air-pollution levels.

The R-Shiny package `mmaqshiny` is for analysing, visualising and spatial-mapping of high-resolution air-pollution data collected by specific devices installed on a moving platform. 

`1-Hz` data of PM2.5 (mass concentrations of particulate matter with size less than 2.5 microns), Black carbon mass concentrations (BC), ultra-fine particle number concentrations, carbon-di-oxide along with GPS coordinates and relative humidity (RH) data collected by some popular portable instruments (`TSI DustTrak-8530, Aethlabs microAeth-AE51, TSI CPC3007, LICOR Li-830, Garmin GPSMAP 64s, Omega USB RH probe` respectively) can be handled by this package. It incorporates device-specific cleaning and correction algorithms. RH correction is applied to DustTrak PM2.5 following [@Chakrabarti:2004]. Provision is given to input linear regression coefficients for correcting the PM2.5 data (if required). BC data will be cleaned for the vibration generated noise, by adopting the statistical procedure as explained in [@Apte:2011], followed by a loading correction as suggested by [@Ban-Weiss:2009]. For the number concentration data, provision is given for dilution correction factor (if a diluter is used with CPC3007; default value is 1).

The package joins the raw, cleaned and corrected data from the above said instruments and outputs as a downloadable csv file. It accepts multiple files for each parameter. The raw files downloaded from each instrument have to be renamed starting with `yyyy_mm_dd`, for using as inputs into the package, since it matches the first 10 characters of the file name to check for consistency.

The package will require GPS file (.gpx) as a mandatory input along with the timezone of the at which the data is collected (a link to all accepted timezones in R is also included). All other pollutant files can be optional. 

All the raw and processed data will be displayed in the `Joined File` tab, while a basic statistical summary of each parameter is provided in the `Summary` tab. The `Plots` tab displays interactive  time series line plots (using plotly) for select parameters, while the `Map` tab provides a spatial map for the user selected pollutant. `Alarm and Settings` tab displays each instruments’ settings and alarms (if any).


`mmaqshiny` handles high-resolution mobile monitoring air-pollution data, automates several data cleaning and correction algorithms, outputs a combined csv file and generates interactive time series plots and spatial maps on an OpenStreetMap background. The joined fie can then ne used for further analysis.


## Limitations

1) Handles single day data at a time,
2) only provision for linear correction coefficients of PM2.5,
3) instrument specific, 
4) file renaming is required.


## Installation

You can install the released version of `mmaqshiny` from [GITHUB](https://github.com/) with:

``` r
devtools::install_github("meenakshi-kushwaha/mmaqshiny")
```

## Example

- A preloaded data appears which is a joined file of existing data collected during the mobile monitoring campaign in Bangalore, India.

- There are warning messages if different dates of data are being joined. 

- This is how the app can be used- 

``` r
library(mmaqshiny)
mmaqshiny::mmaqshiny_run()
```

## User Guide

1. The user needs to add the input timezone in the the text box. 

2. Add .gpx files for the GPSMAP64s - location file inputs. 

![\label{fig:example}](007.png)

3. Add .csv files for the pollutant data available (multiple files are to be selected all at once).

4. User can add the slope and intercept if a linear correction equation is available for the PM2.5 reference grade corrected. 

5. User can also input the Dilution factor is diluter is used for monitoring. 

6. Click the join button to give a single joined file. 

7. Download button to download the joined file as a single csv.

8. Summary tab allows user to check the summary statistics of each pollutant, which helps to check for the for distribution. 

![\label{fig:example}](008.png)

9. The Plot tab helps in plotting the raw pollutant data, which helps to check for the instrument working time. 

![\label{fig:example}](005.png)

10. The Map tab helps to spatially visualise the any pollutant at a finer resolution.

![\label{fig:example}](002.png)

11. Alarms and Settins tab check for any staus errors and notes in each pollutant to keep a track of health of the instrument. 

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

1. DustTrak

DustTrak 8530 Aerosol Monitor was used to measure PM 2.5 concentrations. It utilises the well-established aerosol light scattering technique to estimate the real-time aerosol mass loadings and works at a flow rate of 3 LPM (liters per minute).

2. MicroAeth AE51

It is a highly sensitive, palm-held and battery-operated instrument designed for measuring the optically-absorbing Black Carbon (BC) component of aerosol particles.It measures the rate of change in absorption of transmitted light (880 nm) due to continuous collection of aerosols load on to the filter ticket and has a wide dynamic range of measurement from 0 to 1 mg/m³.

3. Condensation Particle Counter (CPC 3007)

CPC 3007 is an alcohol based handheld instrument by TSI used to measure ultrafine particles. It works on the optical detection principle, and operates at a flow rate of 0.7 LPM.The instrument detects and measures the particles in the size range of 10 nm to > 1 µm. 


## Acknowledgements

We wish to thank Dr. Julian Marshall (University of Washington, Seattle), Dr. Joshua Apte (University of California, Berkeley), Dr. Maelle Salmon, Dr. Florencia D'Andrea and R Ladies community for their help and support.

