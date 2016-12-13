######################################################
######################################################
# data preparation is executed in four main areas:
##    zip code and neighborhood data
###     read & arrange
###     tidy & fill
##    real estate data
###     read, filter, & tidy
###     process dates
###     caclculate realtive change
##    business data
###     read & filter
###     process dates
###     get annual totals
##    census & irs data
###     create wrapper for census API
####      gather data for 2011-2014
###     process downloaded data for 2000 & 2010
####      estimate 2010 median income from irs data
###     impute data for 2001-2009
##    combine data sets
###     save combined dataset for shiny app
######################################################
######################################################



########## zip codes and neighborhoods ##########
library(XML)
zip_url <- 'https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm'
# parse HTML & read table
zip_html <- htmlParse(readLines(zip_url))
zip_table <- readHTMLTable(zip_html, stringsAsFactors = FALSE)[[1]]
names(zip_table)[names(zip_table) == 'ZIP Codes'] <- 'ZipCode' #rename
nyc_zips <- zip_table

# borough is only populated in first row; web table had merged columns
# others havd zip NA; this is adjusted to have correct data by column
nyc_zips$ZipCode <- ifelse(is.na(zip_table$ZipCode), 
                           zip_table$Neighborhood, zip_table$ZipCode)
nyc_zips$Neighborhood <- ifelse(is.na(zip_table$ZipCode), 
                                zip_table$Borough, zip_table$Neighborhood)
nyc_zips$Borough <- ifelse(is.na(zip_table$ZipCode), NA, zip_table$Borough)

# zip codes for each neighborhood are a string with all zips
# these are split into a row for each zip to tidy the data
library(dplyr)
library(tidyr)
nyc_zips <- nyc_zips %>% mutate(ZipCode = strsplit(ZipCode, ',')) %>% 
  unnest(ZipCode) %>% select(ZipCode, Neighborhood, Borough)
library(stringr)
nyc_zips$ZipCode <- str_trim(nyc_zips$ZipCode)

# the NA boroughs introduced in the previous step must be filled
nyc_zips <- nyc_zips %>% fill(Borough)





########## real estate prices ##########
# load data
real_estate <- read.csv('data/Zip_Zhvi_AllHomes.csv', stringsAsFactors = FALSE)
# change zip codes to strings
real_estate$ZipCode <- as.character(real_estate$RegionName)
# select new york zips (Staten Island not available)
# and exclude unneeded variables
real_estate <- real_estate %>% 
  filter(ZipCode %in% nyc_zips$ZipCode) %>% 
  select(ZipCode, X1996.04:X2016.09)

# convert wide data to narrow data for easier analysis
real_estate <- real_estate %>%
  gather(Period, MedianPrice, -ZipCode)

# convert period to date; extract month and year
library(zoo)
real_estate$Period <- as.Date(as.yearmon(real_estate$Period, 'X%Y.%m'))
library(lubridate)
real_estate$Year <- year(real_estate$Period)
real_estate$Month <- month(real_estate$Period)

# data is only available for most Zips starting 2004
# data is filtered to only include 2004-2015 to track whole years
# plus December 2013 to get baseline number
real_estate <- subset(real_estate, 
                      (Year > 2003 & Year < 2016) | (Year == 2003 & Month == 12))

# percentage change from baseline (Dec 13) taken to get common scale
# almost 40% of zip codes missing 2003 data -- increase calculated from
# first available month
# 12/2013 then dropped to return to only full years
real_estate <- real_estate %>% 
  arrange(ZipCode, Period) %>% group_by(ZipCode) %>% drop_na(MedianPrice) %>%
  mutate(PriceChange = MedianPrice / first(MedianPrice) - 1) %>%
  filter(Year != 2003) %>% ungroup()



########## businesses ##########
# load data
corps <- read.csv('data/Active_Corporations___Beginning_1800.csv',
                  stringsAsFactors = FALSE)

# select five new york counties and include only needed variables
corps <- corps %>% 
  select(Period = Initial.DOS.Filing.Date, ZipCode = DOS.Process.Zip) %>%
  filter(ZipCode %in% nyc_zips$ZipCode)
# get month and year for filings, exclude day
corps$Period <- as.Date(as.yearmon(corps$Period, '%m/%d/%Y'))
corps$Year <- year(corps$Period)
corps$Month <- month(corps$Period)
# filter data to match real estate window
corps <- filter(corps, Year > 2003, Year < 2016)

# get count of new businesses by month & zip code & running total
newbiz <- corps %>%
  group_by(ZipCode, Year, Month) %>% summarize(NewBusinesses = n()) %>% 
  group_by(ZipCode, Year) %>% mutate(TotalNewBiz = cumsum(NewBusinesses)) %>%
  ungroup() %>% arrange(ZipCode, Year, Month)



########## population & income ##########
# wrapper for census API
library(jsonlite)
get_census <- function(year, dataset, variables, zips) {
  # create query to retrieve JSON results from
  query <- paste0('http://api.census.gov/data/', year, '/', dataset, 
                  '?get=', paste(variables, collapse = ','), 
                  '&for=zip+code+tabulation+area:', paste(zips, collapse = ','), 
                  '&key=', Sys.getenv('census_key'))
  # API returns matrix; convert to dataframe
  response <- as.data.frame(fromJSON(query), stringsAsFactors = FALSE)
  # fromJSON reads header as data; set names from first row then drop row
  names(response) <- response[1, ]
  response <- response[-1, ]
  # change 'zip code tabulation area' to ZipCode
  names(response)[names(response) == 'zip code tabulation area'] <- 'ZipCode'
  # return data frame
  response
}

# B01001_001E  -- total population 
# B01001_002E  -- male population
# B01001H_001E -- white (non-hispanic) population
# B01002_001E  -- median age
# B19013_001E  -- median household income

census_data <- list('2011' = data.frame(),
                    '2012' = data.frame(),
                    '2013' = data.frame(),
                    '2014' = data.frame())

library(readr)
for (i in as.character(2011:2014)) {
  # get data from census API
  census_data[[i]] <- get_census(i, 'acs5',
                                 c('B01001_001E', 'B01001_002E', 'B01001H_001E',
                                   'B01002_001E', 'B19013_001E'), 
                                 nyc_zips$ZipCode)
  # convert to numbers and percentages
  census_data[[i]] <- census_data[[i]] %>% 
    transmute(ZipCode = ZipCode, Year = parse_number(i), 
              Population = parse_number(B01001_001E),
              Male = parse_number(B01001_002E) / Population,
              White = parse_number(B01001H_001E) / Population,
              MedianAge = parse_number(B01002_001E),
              MedianIncome = parse_number(B19013_001E))
}

# 2000 & 2010 data not available through API -- read from file

# 2000

# GEO.id2   -- zip code (as int)
# HC01_VC01 -- total population
# HC01_VC03 -- male popluation
# HC01_VC29 -- white (alone) population
# HC01_VC18 -- median age
# HC01_VC64 -- median income not available

# read in data from summary file 1 (SF1)
census_data[['2000']] <- read.csv(
  'data/DEC_00_SF1_DP1.csv',
  stringsAsFactors = FALSE)
census_data[['2000']] <- census_data[['2000']] %>% 
  select(GEO.id2, HC01_VC01, HC01_VC03, HC01_VC29, HC01_VC18)
census_data[['2000']]$GEO.id2 <- as.character(census_data[['2000']]$GEO.id2)
census_data[['2000']] <- census_data[['2000']] %>%
  filter(GEO.id2 %in% nyc_zips$ZipCode)
census_data[['2000']] <- census_data[['2000']] %>% transmute(
  ZipCode = GEO.id2, Year = 2000, Population = HC01_VC01,
  Male = HC01_VC03 / Population, White = HC01_VC29 / Population,
  MedianAge = HC01_VC18
)

# read in income from SF3
income2000 <- read.csv(
  'data/DEC_00_SF3_DP3.csv',
  stringsAsFactors = FALSE)
income2000 <- income2000 %>% select(GEO.id2, HC01_VC64) %>% 
  filter(GEO.id2 %in% nyc_zips$ZipCode) %>% 
  transmute(ZipCode = GEO.id2, MedianIncome = HC01_VC64)
# add median income to other columns
census_data[['2000']] <- inner_join(census_data[['2000']], income2000, by = 'ZipCode')



# GEO.id2   -- zip code (as int)
# HD01_S001 -- total population
# HD01_S026 -- male popluation
# HD01_S078 -- white (alone) population
# HD01_S020 -- median age
# NA        -- median income not available

census_data[['2010']] <- read.csv(
  'data/DEC_10_SF1_SF1DP1.csv', 
  stringsAsFactors = FALSE)

census_data[['2010']] <- census_data[['2010']] %>% 
  select(GEO.id2, HD01_S001, HD01_S026, HD01_S078, HD01_S020)
census_data[['2010']]$GEO.id2 <- as.character(census_data[['2010']]$GEO.id2)
census_data[['2010']] <- census_data[['2010']] %>%
  filter(GEO.id2 %in% nyc_zips$ZipCode)
census_data[['2010']] <- census_data[['2010']] %>% transmute(
  ZipCode = GEO.id2, Year = 2010, Population = HD01_S001,
  Male = HD01_S026 / Population, White = HD01_S078 / Population,
  MedianAge = HD01_S020, MedianIncome = NA
)


# get median income from IRS data for 2010
library(readxl)

IRS <- read_excel('data/10zp33ny.xls', skip = 3)
# change names
names(IRS)[names(IRS) == 'ZIP\ncode [1]'] <- 'ZipCode'
names(IRS)[names(IRS) == 'Size of adjusted gross income'] <- 'Income'
names(IRS)[names(IRS) == 'Number of returns'] <- 'Returns'
# convert zip to character
IRS$ZipCode <- as.character(IRS$ZipCode)
# estimate median by getting number of returns in each bucket per zip
IRS <- IRS[, c('ZipCode', 'Income', 'Returns')]
IRS <- filter(IRS, ZipCode %in% nyc_zips$ZipCode)

# rename to allow for ordering
IRS$Income[is.na(IRS$Income)] <- 'Total'
IRS$Income[IRS$Income == '$1 under $25,000'] <- '000-025'
IRS$Income[IRS$Income == '$25,000 under $50,000'] <- '025-050'
IRS$Income[IRS$Income == '$50,000 under $75,000'] <- '050-075'
IRS$Income[IRS$Income == '$75,000 under $100,000'] <- '075-100'
IRS$Income[IRS$Income == '$100,000 under $200,000'] <- '100-200'
IRS$Income[IRS$Income == '$200,000 or more'] <- '200+'

# spread & add column for median income
IRS <- IRS %>% spread(Income, Returns)
IRS$Med <- NA

# approximate median income
med_income <- function(zip_income) {
  # find how many people at 50th percentile
  mid <- zip_income$Total / 2
  # loop through buckets of income, skipping zip, total, and Med placeholder
  for (i in 2:(length(zip_income) - 2)) {
    # if not at 50th pctile, remove to keep track
    if (mid > zip_income[i]) {
      mid <- mid - zip_income[i]
    } else { # if in correct bucket
      bin <- i # store bucket
      # figure out where in bucket 50th pctile would lie
      # assuming uniform distribution within bucket
      ratio <- mid / zip_income[i]
      break
    }
  }  
  # get bounds of bucket
  bnds <- parse_number(strsplit(names(zip_income)[bin], '-')[[1]])
  # get point in bucket corresponding to above ratio
  med <- bnds[1] + diff(bnds) * ratio
  # return value without name or row.name
  round(med[[1]] * 1000, 0)
}

# get median income approximations for each zip code
for (i in 1:nrow(IRS)) {
  IRS$Med[i] = med_income(IRS[i, ])
}

# add median income to 2010 census data
census_data[['2010']]$MedianIncome <- as.vector(
  as.data.frame(
    IRS[
      match(census_data[['2010']]$ZipCode, IRS$ZipCode),
      length(IRS)]
  )[, 1])

# combine census data
census <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(census_data)) {
  census <- rbind(census, census_data[[i]])
}


# impute census data for years between decennial censuses with real estate data
yrs_imputed <- 2001:2009

# create data frame to handle imputed values
imputed <- data.frame(a[0, ])

# for each zip code
for (zp in nyc_zips$ZipCode) {
  # filter for zip code and decennial data
  dec <- census %>% filter(ZipCode == zp & Year %in% c(2000, 2010))
  # if zip code data available in both census years
  if (nrow(drop_na(dec)) == 2) {
    # perform linear imputation for each variable
    pop <- round(approx(dec$Year, dec$Population, xout = yrs_imputed)$y, 0)
    male <- approx(dec$Year, dec$Male, xout = yrs_imputed)$y
    white <- approx(dec$Year, dec$White, xout = yrs_imputed)$y
    age <- approx(dec$Year, dec$MedianAge, xout = yrs_imputed)$y
    inc <- round(approx(dec$Year, dec$MedianIncome, xout = yrs_imputed)$y, 0)
    # create temporary data frame of imputed results for zip code
    tmp <- data.frame(ZipCode = rep(zp, length(yrs_imputed)), Year = yrs_imputed,
                      Population = pop, Male = male, White = white, 
                      MedianAge = age, MedianIncome = inc, 
                      stringsAsFactors = FALSE)
    # add to data frame of imputed results
    imputed <- rbind(imputed, tmp)
  }
}

# combine census data and imputed values
census <- rbind(census, imputed) %>% arrange(ZipCode, Year)




########## combine data sets ##########

# link zip code info to  businesses (x axis)
# taking total annual new businesses to match census data
gentrify <- inner_join(nyc_zips, newbiz %>%
                         filter(Month == 12) %>%
                         select(ZipCode, Year, NewBusinesses, TotalNewBiz),
                       by = 'ZipCode')

# add real estate data (y axis)
# taking only end of year prices to match census data
gentrify <- inner_join(gentrify, real_estate %>% 
                         filter(Month == 12) %>% 
                         select(ZipCode, Year, MedianPrice, PriceChange),
                       by = c('ZipCode', 'Year'))

# add census information
gentrify <- inner_join(gentrify, census, by = c('ZipCode', 'Year'))

# convert borough and neighborhoods to factors
gentrify$Neighborhood <- factor(gentrify$Neighborhood)
gentrify$Borough <- factor(gentrify$Borough)

# convert to tbl_df for easier investigation
gentrify <- tbl_df(gentrify)

save(gentrify, file = 'gentrification/gentrify.Rda')
