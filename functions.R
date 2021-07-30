### Data Package overview:
# This code extracts data from the fluxmeter network for submission to MFE in
# the required 'data package' format.

# Raw data frames are listed as follows:
#   Site_x.1 - Climate data from SWB climate Tab. Includes crop info.
# Site_x.2 - Soil moisture and temperature monitoring data from the 'gold sites'
# Site_x.3 - Modelled drainage at 120 cm from the SWB 'SWB' tab.
# Site_x.4 - Collected drainage, nutrient concentration and nutrient loss data
# Site_x.5 - Soil nutrients from "Diary and results files - soil chemical" tab.
# Includes mineral N, AMN, tot C, tot N, pH and OP
# Site_x.6 - Crop harvest info from "Diary and results files - Biomass Summary"
# tab

### Set Dates

Date_start <- "2014-08-31"
Date_End <- "2020-09-30"

## Site_x.1 Function for climate data

Sum_x.1 <- function(x){
  x <- select(x,c("Date",
                  Crop = "Crop_R", "Rain (mm)", "Irrigation (mm)", "Tmean (C)", "Tmin (C)", "T max(C)", "VapPress (hPa)", "Pmsl (hPa)", "Rad (MJ/m2)","WindRun (m/s)")) %>%
    mutate(Date = as.Date(Date)) %>%
    filter(Date > Date_start & Date < Date_End)
  
}



## Site_x.2 Function for soil moisture data

Sum_x.2 <- function(x){
  x <- select(x,c("Date","Average 0-30 (mm)", "Average 30-60 (mm)", "Average 60-90 (mm)", "10 cm Soil Temp (C )")) %>%
    mutate(Date = as.Date(Date))%>%
    filter(Date > Date_start & Date < Date_End)
  x
  
  x <- ddply(x, c("`Date`"), summarise,
             "SWC 0-30 cm (mm)" = mean(`Average 0-30 (mm)`),
             "SWC 30-60 cm (mm)" = mean(`Average 30-60 (mm)`),
             "SWC 60-90 cm (mm)" = mean(`Average 60-90 (mm)`),
             "10 cm soil temp (C)" = mean(`10 cm Soil Temp (C )`)) 
  
  x 
  
}


## Site_x.3 Function for modelled drainage
Sum_x.3 <- function(x){
  x <- select(x, Date = col1 , Drainage = col30) %>%
    mutate(Date = as.Date(Date)) %>%
    filter(Date > Date_start & Date < Date_End)
  x
  
  x <-   rename(x, c("Drainage"="Modelled drainage at 120 cm"))
  
}






### Site_x.4 Functions are for Drainage and Nutrient Loss
###  NB: P Data is taken from an output file from P analysis code in: \\Hawdfs41\data\Common\EXPTS\Commercial\F A R\SFF Fluxmeter\Sites and Results\Fluxmeter R\Phosphorus data analysis NB: P DATE NEEDS TO BE UPDATED/REVIEWED BEFORE BEING INCLUDED IN FINAL FIF PACKAGE

## - > Sum_x.4_Gen -> Pull out drainage and nutrient data, select date range and filter outliers (SITES 1 - 7 where outlier == x and xx)
## - > Sum_x.4_S9 -> Pull out drainage and nutrient data, select date range and filter outliers (SITES 9 where outlier == xxx)
## - > Sum_x.4_S10 -> Pull out drainage and nutrient data, select date range and filter outliers (SITES 10 where outlier == xx and xxx)
## - > Sum_x.4_S11 -> Pull out drainage and nutrient data, select date range and filter outliers (SITES 11 where outlier == xx)
## - > Sum_x.4_S12 -> Pull out drainage and nutrient data, select date range and filter outliers (SITES 11 where outlier == x)

### ------- NOTE: "OUTLIER 2' IN THE SWB FILE MUST BE DENOTED 'M' WHEN MODELLED DATA IS USED TO CALCULATE LOSSES -------


## - > Sum_x.4.1 -> Select and summarise drainage data
## - > Sum_x.4.2 -> Select nutrient concentration data, remove na values and replace 'less than (<) values), Summarise nutrient concentration data into count, mean and STDEV
## - >  Sum_x.4.3 -> Select and summarise nutrient loss data

## Sum_x.4_Gen for sites 1-7
### Note = 1 means drainage data used to calculate losses, Note = 10 means modelled drainage used to calculate losses

Sum_x.4_Gen <- function(x){
  x <- select(x, Year, Date = "Sampling Date" , "Outlier","Outlier 2",
              Drainage = "Measured Drainage (mm)", 
              Modelled_Drainage = "Period drainage modelled (mm)",  
              Nitrate = "Nitrate-N (mg/L)", 
              Ammonium = "Ammoniacal-N (mg/L)", Inorganic_N = "Inorganic-N (mg/L)", Nitrate_loss = "Nitrate-N loss (kg/ha)", Ammonium_loss = "Ammonium-N loss (kg/ha)", Inorganic_N_Loss = "Inorganic N Loss (kg/ha)") %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Note = ifelse(`Outlier 2` == "M",10,1)) %>% 
    filter(Date > Date_start & Date < Date_End) %>%
    filter(Outlier %in% 1:3)
  x
}




## Sum_x.4_S9
### Note = 1 means drainage data used to calculate losses, Note = 10 means modelled drainage used to calculate losses

Sum_x.4_S9 <- function(x){
  x <- select(x, Year, Date = "Sampling Date" , "Outlier", "Outlier 2", Drainage = "Measured Drainage (mm)", Modelled_Drainage = "Period drainage modelled (mm)",  Nitrate = "Nitrate-N (mg/L)", Ammonium = "Ammoniacal-N (mg/L)", Inorganic_N = "Inorganic-N (mg/L)", Nitrate_loss = "Nitrate-N loss (kg/ha)", Ammonium_loss = "Ammonium-N loss (kg/ha)", Inorganic_N_Loss = "Inorganic N Loss (kg/ha)") %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Note = ifelse(`Outlier 2` == "M",10,1)) %>%
    filter(Date > Date_start & Date < Date_End) %>%
    filter(Outlier %in% 1:3)
  x
}


## Sum_x.4_S10
### Note = 1 means drainage data used to calculate losses, Note = 10 means modelled drainage used to calculate losses

Sum_x.4_S10 <- function(x){
  x <- select(x,Year, Date = "Sampling Date" , "Outlier", "Outlier 2", Drainage = "Measured Drainage (mm)", Modelled_Drainage = "Period drainage modelled (mm)",  Nitrate = "Nitrate-N (mg/L)", Ammonium = "Ammoniacal-N (mg/L)", Inorganic_N = "Inorganic-N (mg/L)", Nitrate_loss = "Nitrate-N loss (kg/ha)", Ammonium_loss = "Ammonium-N loss (kg/ha)", Inorganic_N_Loss = "Inorganic N Loss (kg/ha)") %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Note = ifelse(`Outlier 2` == "M",10,1)) %>% 
    filter(Date > Date_start & Date < Date_End) %>%
    filter(Outlier %in% 1:3)
  x
}



## Sum_x.4_S11
### Note = 1 means drainage data used to calculate losses, 
#Note = 10 means modelled drainage used to calculate losses

Sum_x.4_S11 <- function(x){
  x <- select(x, Year, Date = "Sampling Date" ,
              "Outlier", "Outlier 2", 
              Measured_Drainage = "Measured Drainage (mm)", 
              Drainage = "Period drainage modelled (mm)", 
              Nitrate = "Nitrate-N (mg/L)", 
              Ammonium = "Ammoniacal-N (mg/L)", 
              Inorganic_N = "Inorganic-N (mg/L)", 
              Nitrate_loss = "Nitrate-N loss (kg/ha)", 
              Ammonium_loss = "Ammonium-N loss (kg/ha)",
              Inorganic_N_Loss = "Inorganic N Loss (kg/ha)") %>%
    mutate(Date = as.Date(Date),
           Note = ifelse(`Outlier 2` == "M",10,1),
           Drainage = ifelse(`Outlier 2` == 1, Measured_Drainage,
                             Drainage)) %>% 
    filter(Date > Date_start & Date < Date_End) %>%
    filter(Outlier %in% 1:3)
  x
}



## Sum_x.4_S12
### Note = 1 means drainage data used to calculate losses, Note = 10 means modelled drainage used to calculate losses

Sum_x.4_S12 <- function(x){
  x <- select(x,Year, Date = "Sampling Date" , "Outlier", "Outlier 2", 
              Measured_Drainage = "Measured Drainage (mm)", 
              Drainage = "Period drainage modelled (mm)", 
              Nitrate = "Nitrate-N (mg/L)", 
              Ammonium = "Ammoniacal-N (mg/L)", 
              Inorganic_N = "Inorganic-N (mg/L)", 
              Nitrate_loss = "Nitrate-N loss (kg/ha)", 
              Ammonium_loss = "Ammonium-N loss (kg/ha)", 
              Inorganic_N_Loss = "Inorganic N Loss (kg/ha)") %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Note = ifelse(`Outlier 2` == "M",10,1)) %>% 
    filter(Date > Date_start & Date < Date_End) %>%
    filter(Outlier %in% 1:3)
  x
}



## x.4.1
### Numeric 1 = drainage data used to calculate losses, Numeric 2 = modelled drainage used to calculate losses

Sum_x.4.1 <- function(x){
  x <- select(x, Year, Date, Drainage, Note)
  
  x <- ddply(x, c("Year","`Date`"), summarise,
             "Collected drainage (mm)" = mean(`Drainage`),
             "Sample count (includes zero volumes)"    = length(`Drainage`),
             "Collected drainage (SD)" = sd(`Drainage`),
             "Model ID"  = mean(Note))         %>%
    filter(Date > Date_start & Date < Date_End)
  
  x <-  mutate(x,"Drainage Note" = ifelse(`Model ID` == 10, "Measured drainage HIGH - Nutrient losses estimated using modelled drainage and measured conentrations","NA")) 
  
  x <- select(x,"Year", "Date", "Collected drainage (mm)", "Sample count (includes zero volumes)", "Collected drainage (SD)","Drainage Note")
  
  x
  
}




## x.4.2

Sum_x.4.2 <- function(x){
  x <- select(x, Date,  Nitrate, Ammonium, Inorganic_N) %>%
    na.omit() %>%
    mutate(Ammonium = ifelse(Ammonium =="<0.02","0.01",Ammonium)) 
  
  x$Ammonium <- as.numeric(as.character(x$Ammonium))
  
  x <- ddply(x, c("`Date`"), summarise,
             "Count [N]"    = length(`Nitrate`),
             "Nitrate-N (mg/L)" = mean(`Nitrate`),
             "Nitrate-N SD (mg/L)" = sd(`Nitrate`),
             "Ammonium (mg/L)" = mean(`Ammonium`),
             "Ammonium SD (mg/L)" = sd(`Ammonium`),
             "Inorganic-N (mg/L)" = mean(`Inorganic_N`),
             "Inorganic-N SD (mg/L)" = sd(`Inorganic_N`)) %>%
    filter(Date > Date_start & Date < Date_End)
  
  x
}




## x.4.3

Sum_x.4.3 <- function(x){
  x <- select(x, Date,  Nitrate_loss, Ammonium_loss, Inorganic_N_Loss)  
  
  x <- ddply(x, c("`Date`"), summarise,
             "Nitrate-N loss (kg/ha)" = mean(`Nitrate_loss`),
             "Nitrate-N loss SD (kg/ha)" = sd(`Nitrate_loss`),
             "Ammonium loss (kg/ha)" = mean(`Ammonium_loss`),
             "Ammonium SD (kg/ha)" = sd(`Ammonium_loss`),
             "Inorganic-N (kg/ha)" = mean(`Inorganic_N_Loss`),
             "Inorganic-N SD (kg/ha)" = sd(`Inorganic_N_Loss`)) %>%
    filter(Date > Date_start & Date < Date_End)
  
  x
}





### Sum_x.5 -> Soil data

Sum_x.5 <- function(x){
  x <- select(x,c("Date", "Rep", "Depth", "AMN", "Mineral-N", "Tot N","Tot C","pH","Olsen P")) %>%
    filter(!is.na(`Mineral-N`)) %>% 
    mutate(Date = as.Date(Date)) %>% 
    group_by(`Date`,`Rep`) %>%
    mutate_all(funs(replace(., is.na(.), ""))) %>%
    filter(Date > Date_start & Date < Date_End)
  x
  
  x$AMN <- as.numeric(as.character(x$AMN))
  x$"Mineral-N" <- as.numeric(as.character(x$"Mineral-N"))
  x$"Tot N" <- as.numeric(as.character(x$"Tot N"))
  x$"Tot C" <- as.numeric(as.character(x$"Tot C"))
  x$pH <- as.numeric(as.character(x$pH))
  x$"Olsen P" <- as.numeric(as.character(x$"Olsen P"))
  
  x <- ddply(x, c("Date", "Depth"), summarise,
             AMN = mean(`AMN`),
             "Mineral-N" = mean(`Mineral-N`),
             "Tot N" = mean(`Tot N`),
             "Tot C" = mean(`Tot C`),
             pH = mean(`pH`),
             "Olsen P" = mean(`Olsen P`))
  x
  
  x <-  reshape(x, idvar = "Date", timevar = "Depth", direction = "wide")
  
  x <-  select (x, -c("AMN.40-60","AMN.60-80","AMN.80-100", "pH.20-40", "pH.40-60", "pH.60-80", "pH.80-100","Olsen P.20-40","Olsen P.40-60", "Olsen P.60-80" ,"Olsen P.80-100" )) 
  
  x <-   rename(x, c("AMN.0-20"="AMN kg/ha (0-20 cm)", "AMN.20-40"="AMN kg/ha (20-40 cm)", "Mineral-N.0-20"="Min-N kg/ha (0-20 cm)", "Mineral-N.20-40"="Min-N kg/ha (20-40 cm)", "Mineral-N.40-60"="Min-N kg/ha (40-60 cm)", "Mineral-N.60-80"="Min-N kg/ha (60-80 cm)", "Mineral-N.80-100"="Min-N kg/ha (80-100 cm)", "Tot C.0-20"="Tot C % (0-20 cm)", "Tot C.20-40"="Tot C % (20-40 cm)", "Tot C.40-60"="Tot C % (40-60 cm)", "Tot C.60-80"="Tot C % (60-80 cm)", "Tot C.80-100"="Tot C % (80-100 cm)", "Tot N.0-20"="Tot N % (0-20 cm)", "Tot N.20-40"="Tot N % (20-40 cm)", "Tot N.40-60"="Tot N % (40-60 cm)", "Tot N.60-80"="Tot N % (60-80 cm)", "Tot N.80-100"="Tot N % (80-100 cm)", "pH.0-20" = "pH (0-20 cm)", "Olsen P.0-20" = "Olsen P (0-20 cm)"))
  
  x <- x[,c("Date","Min-N kg/ha (0-20 cm)", "Min-N kg/ha (20-40 cm)", "Min-N kg/ha (40-60 cm)", "Min-N kg/ha (60-80 cm)", "Min-N kg/ha (80-100 cm)","AMN kg/ha (0-20 cm)", "AMN kg/ha (20-40 cm)", "Tot C % (0-20 cm)", "Tot C % (20-40 cm)", "Tot C % (40-60 cm)", "Tot C % (60-80 cm)", "Tot C % (80-100 cm)", "Tot N % (0-20 cm)", "Tot N % (20-40 cm)", "Tot N % (40-60 cm)", "Tot N % (60-80 cm)", "Tot N % (80-100 cm)", "pH (0-20 cm)","Olsen P (0-20 cm)")] 
  
}




## Site_x.6 Function for biomass data


Sum_x.6 <- function(x){
  x <- select(x,c("Harvest date","Harvest number", "Whole crop yield (t DM/ha)", "Whole crop N uptake (kg N/ha)", "Whole crop P Uptake (kg P/ha)", "Marketable yield (t DM/ha)", "Marketable N uptake (kg N/ha)", "Marketable P Uptake (kg P/ha)", "Crop Notes")) 
  
  x
  
  x <-   rename(x, c("Harvest date"="Date")) %>%
    mutate(Date = as.Date(Date)) %>%
    filter(Date > Date_start & Date < Date_End) 
  
}



#' group_in_season
#'
#' @param DT 
#' @param date_col 
#'
#' @description  Group annual data into seasonal data 1 July to 30 June next year
#' 
#'
#' @return data.table with a column `Season`
#' @export
#'
#' @examples
group_in_season <- function(DT, date_col = "Date"){
  
  stopifnot(date_col %in% colnames(DT))
  
  period <- range(DT[[date_col]])
  noofyear <- diff.Date(period, unit = "year") %>% 
    as.numeric(.)/365
  startyear <- year(period[1])
  endyear <- year(period[2])
  startmd <- "-09-01"
  endmd <- "-08-31"
  noofseason <- round(noofyear, digits = 0)
  
  # Initial a vector to store the text as cmd
  cmd <- vector("character", noofseason)
  
  # Build a cmd to do conditional evaluation 
  
  for(i in 0:(noofseason)){
    # Key condition
    v <- paste0(date_col, " >= \"" , startyear + i, startmd,"\"","&",
                date_col, " <= \"", startyear + i + 1, endmd, "\"",",",
                "\"", startyear +i,"/", startyear + i + 1, "\"",",")
    # Check the format 
    # cat(v)
    # Store it; must be i + 1 since R has no 0 position
    cmd[i + 1] <- v
  }
  # Collapse into one string and glue the fcase function 
  cmd <- paste0("fcase( ", paste(cmd, collapse = ""), ")")
  
  # Delete the end comma
  cmd <- gsub(",)$", ")", cmd)
  # Check format again
  cat("Check if the command format is correct\r\n", cmd)
  
  DT[, Season:= eval(parse(text = cmd))]
  return(DT)
  
  
}



