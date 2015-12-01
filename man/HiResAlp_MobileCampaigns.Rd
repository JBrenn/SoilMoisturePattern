\name{HiResAlp_MobileCampaigns}
\alias{HiResAlp_MobileCampaigns}
\docType{data}
\title{
HiResAlp Mobile Field Campaign 
}
\description{
Data frame containing field data gathered manually within the HiResAlp project 2013-2015. Data on volumetric soil moisture content are combined with topographic, vegetation and land use characteristics derived from different sources. Climatic drivers (preceding precipitation, air temperature, relative humidity, and incoming solar radiation) are measured in representative locations within the LTER site Matsch/Mazia. Land use and soil classifications were gained during intensive field investigations. Remote sensing products, like MODIS vegetation indices, accomplish the data set.
%Climatic driver are presented by preceding rainfall amounts 1-7 days before the field campaigns. 
The data set is a basis for better understanding of inter-dependencies between soil moisture, topography and atmosperic conditions/weather, land use and soil variability. It serves to investigate soil moisture variablility and stability in time and space. The campaigns aim to validate remote sensing products coming from satellites (e.g. RADARSAT2 or Sentinel images), accomplish high resolution NDVI and thermal maps (drone products) and, moreover, can be useful for calibrating hydrological models or data assimilation approaches (e.g. combining ground sensed data, EO observations and modeling).

}
\usage{data("HiResAlp_MobileCampaigns")}
\format{
  A data frame with 2443 observations on the following 58 variables.
  \describe{
    \item{\code{ID}}{integer, ID of observation}
    \item{\code{CampaignType}}{Faktor, Topology of HiResAlp campaign, RADAR for campaigns considered for EO data calibration/validation (in MUNTATSCHINIG or MATSCH valley, in both regions if not indicated), DRONE for campaigns to accomplish drone field campaign}
    
    
\bold{\emph{Datetime variables}}        
    
    \item{\code{date}}{Factor, Date of observation - format: yyyy-mm-dd}
    \item{\code{time}}{character, Time of observation - format: HH:MM:SS}
    \item{\code{year}}{integer, Year of observation - format: yyyy}
    \item{\code{month}}{integer, Month of observation}
    \item{\code{day}}{integer, Day of observation}
    \item{\code{DOY}}{integer, Day Of Year}
    \item{\code{season}}{character, Season ("DJF","MAM","JJA","SON")}
    \item{\code{vegPeriod}}{integer, Vegetation period: 1 - Apr. 1st to Nov. 1st, no Vegetation period: 0}
    

\bold{\emph{Geographic location of measurement}}
    
    \item{\code{longitude}}{numeric, geographical position longitude in degree}
    \item{\code{latitude}}{numeric, geographical position latitude in degree}
    \item{\code{utmx}}{numeric, geographical position x in meter}
    \item{\code{utmy}}{numeric, geographical position y in meter}
    
    
\bold{\emph{Data measured with WET sensor}}
    
    \item{\code{Sensor}}{character, name of sensor used}
    \item{\code{SoilMoisture_mean}}{numeric, Volumetric Soil Moisture (SMC) Mean Value (raw data); for "WET-2 Sensor, Delta-T" aggregation according to sampling strategy of HiResAlp Campaigns; for "HydroSense II, Campbell" aggregation of measurment which are very close (rounding of utm xy data)}
    \item{\code{SoilMoisture_sd}}{Volumetric Soil Moisture Standard Deviation (raw data)}
    \item{\code{SoilTemperature_mean}}{Soil Temperature Mean Value in degree Celsius; only available for "WET-2 Sensor, Delta-T"}
    \item{\code{SoilTemperature_sd}}{Soil Temperature Standard Deviation}
    \item{\code{Permittivity_mean}}{Soil Permittivity (Eb) Mean Value in mS/m; measure variable by sensor, used to derive SMC}
    \item{\code{Permittivity_sd}}{Soil Permittivity Standard Deviation}
    \item{\code{SoilMoisture_mean_Processed}}{numeric, Volumetric Soil Moisture Mean Value (processed data); for "WET-2 Sensor, Delta-T" SMC is calculated by the formula SMC = (sqrt(Eb) - a0) / a1, where the parameters a0 and a1 are soil specific paramters and have been estimated from the "HydroSense II, Campbell" data}
        

\bold{\emph{Measurement location meta data}}
        
    \item{\code{Landuse}}{Factor, Land Use Class derived from field campaign; Classes: 1-irr. Meadow; 2- non irr. Meadow; 3- addandoned Meadow; 4- Pasture; 5- Other; 6- Other: Shrubs; 7- Other: Forest; missing values were filled with Information derived from former vegetation survey in Matsch/Mazia valley - see below.}
    \item{\code{SoilCoverage}}{Factor, Soil Coverage Class derived from field campaign [percent]; Classes: 1- Bare Soil, 2- 0.25, 3- 0.50; 4- 0.75; 5- 1}
    \item{\code{VegetationHeight}}{Factor, Vegetation Height Class dervived from field campaign [cm]; Classes: 1- >10cm 2- [10cm;25cm]; 3- [25cm;50cm]; 4- [50cm;75cm]; 5- >75cm}
    \item{\code{Notes}}{character, Note taken during field campaign}
    

\bold{\emph{Topographic and hydrological variables derived from digital elevation model}}    
    
    \item{\code{elevation}}{numeric, Elevation in m a.s.l. from DEM (resolution: 2.5m)}
    \item{\code{slope}}{numeric, Slope in degree derived from DEM (resolution: 2.5m)}
    \item{\code{aspect}}{numeric, Aspectin degree derived from DEM (resolution: 2.5m)}
    \item{\code{curvProfile}}{numeric, profile Curvature derived from DEM (resolution: 2.5m)}
    \item{\code{curvHorizon}}{numeric, horizon Curvature derived from DEM (resolution: 2.5m)}
    \item{\code{elevation}}{numeric, Elevation in m a.s.l. from DEM (resolution: 2.5m)}
    \item{\code{slope}}{numeric, Slope in degree derived from DEM (resolution: 2.5m)}
    \item{\code{aspect}}{numeric, Aspectin degree derived from DEM (resolution: 2.5m)}
    \item{\code{curvProfile}}{numeric, profile Curvature derived from DEM (resolution: 2.5m)}
    \item{\code{curvHorizon}}{numeric, horizon Curvature derived from DEM (resolution: 2.5m)}
    \item{\code{flowAccum}}{numeric, Flow Accumulation derived from DEM (resolution: 2.5m), calculation with SAGA GIS: Multiple Flow Direction based on Maximum Downslope Gradient (Qin et al. 2011)
}
    \item{\code{TWI}}{numeric, Topographic Wetness Index derived from Slope and flowAccum (resolution: 2.5m), calculation with SAGA GIS: ln(a/tan(beta)), where a is the area of the hillslope per unit contour length that drains through any point, and tan(beta) is the local surface topographic slope (delta vertical) / (delta horizontal).}
    

\bold{\emph{Information derived from vegetation survey 2015 in Matsch/Mazia valley by Michaela Plaikner}}
    
    \item{\code{StrukturCL}}{integer, Structure type class}
    \item{\code{StruturTyp}}{Factor, Structure Type label}
    \item{\code{NutzungCL}}{integer, current Land Use class}
    \item{\code{AktuelleNutzung}}{Factor, current Land Use label}
    \item{\code{VegTypCL}}{integer, Vegetation type class}
    \item{\code{VegLab3}}{Factor, Vegetation Type Label 3; very specific e.g. "Goldhaferwiese"}
    \item{\code{VegLab2}}{Factor, Vegetation Type Label 2; e.g. "Sekundaere Rasengesellschaften"}
    \item{\code{VegLab1}}{Factor, Vegetation Type Label 1; general e.g. "FETTWEIDEN UND FETTRASEN"}
    \item{\code{TrittID}}{integer, Damage by treads (cattle, sheep, ...) class }
    \item{\code{Trittschaeden}}{Factor, Damage by treads label}
    \item{\code{VegShapeID}}{numeric, ID of shapes in vegetation map; useful for extracting additional information}
    
    
\bold{\emph{Information derived from former vegetation survey in Matsch/Mazia valley}}    
    
    \item{\code{landuseID}}{numeric, Land Use class}
    \item{\code{landuseLabel}}{Factor, Land Use type label}
    
\bold{\emph{Information derived from soil survey in Matsch/Mazia valley in 2011}}

    \item{\code{soilID}}{numeric, Soil class}
    \item{\code{soilID}}{Factor, Soil type}
    \item{\code{soilID}}{Factor, Texture class}
    \item{\code{soilID}}{Factor, Soil type in GEOtop model}
    
\bold{\emph{Information on atmospheric condition / weather}}
\bold{\emph{(base stations P2 - 1500 m a.s.l and B3 - 2000 m a.s.l.)}}

    \item{\code{Temp}}{Air Temperature in degC, mean value for day of observation}
    \item{\code{RH}}{Relative Humidity in percent, mean value for day of observation}
    \item{\code{SolarRadiation}}{Incoming Solar Radiation in W/m2, mean value for day of observation}
 
    \item{\code{antecedentRain1_B3}}{Rainfall Amount in mm, 24 hours before measurement, station B3}
    \item{\code{antecedentRain3_B3}}{Rainfall Amount in mm, 3 days before measurement, station B3}
    \item{\code{antecedentRain5_B3}}{Rainfall Amount  in mm, 5 days before measurement, station B3}
    \item{\code{antecedentRain10_B3}}{Rainfall Amount in mm, 10 days before measurement, station B3}
    \item{\code{antecedentRain10_B3}}{Rainfall Amount in mm, 40 days before measurement, station B3}
    
    \item{\code{antecedentRain1_P2}}{Rainfall Amount in mm, 24 hours before measurement, station P2}
    \item{\code{antecedentRain3_P2}}{Rainfall Amount in mm, 3 days before measurement, station P2}
    \item{\code{antecedentRain5_P2}}{Rainfall Amount in mm, 5 days before measurement, station P2}
    \item{\code{antecedentRain10_P2}}{Rainfall Amount in mm, 10 days before measurement, station P2}
    \item{\code{antecedentRain10_P2}}{Rainfall Amount in mm, 40 days before measurement, station P2} 

\bold{\emph{Remote Sensing Data (e.g. MODIS, RADARSAT)}}

    \item{\code{radarsat_smc}}{RADARSAT soil moisture product}
    \item{\code{MODIS.start.date}}{Date of first MODIS product retrieved}
    \item{\code{MODIS.end.date}}{Date of last MODIS product retrieved}
    \item{\code{MODIS.NDVI.mean}}{numeric, MODIS Normalized Diverence  (NDVI) mean values, EO map resolution 250m, [0;1]}
    \item{\code{MODIS_EVI}}{numeric, MODIS EVI mean value, extension of NDVI, EO map resolution 250m, [0;1]}
}
}
\details{
MODIS data are point wise extracted using the functionalities of \pkg{MODIStools}. In specific the functions  \code{\link{MODISSubsets}} and \code{\link{MODISsummary}} were used for a time period of 14 days araound the observation day. Data sources see below.
}
\source{
MODIS Vegetation Index Products (NDVI and EVI): 
MOD13Q1 (16-Day L3 Global 250m) \url{https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13q1} 

MODIS Evapotranspiration (ET and LE): MOD16A2 (8-day Global 1km) \url{ftp://ftp.ntsg.umt.edu/pub/MODIS/NTSG_Products/MOD16/}
}
\references{
Qin et al. (2011): \emph{An approach to computing topographic wetness index based on maximum downslope gradient.}, Precision Agriculture, 12(1), 32-43. \url{http://doi.org/10.1007/s11119-009-9152-y}

Tuck et al. (2014). \emph{MODISTools - downloading and processing MODIS remotely sensed data in R.} Ecology and Evolution, 4(24), 4658–4668. \url{http://doi.org/10.1002/ece3.1273}
}
\examples{
data(HiResAlp_MobileCampaigns)
}
\keyword{datasets}
