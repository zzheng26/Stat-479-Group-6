require(dplyr)
require(tidyr)

# read in and merge data

fileName = c("Chicago - OHare.csv", 
             "Atlanta - Jackson Atlanta Airport.csv",
             "Dallas - Dallas-Fort Worth Airport.csv",
             "Denver - Denver International.csv",
             "Las Vegas - McCarran Airport.csv",
             "Los Angeles - LAX.csv",
             "Madison - Dane Co Airport.csv",
             "Miami - Miami Airport.csv",
             "New York - LaGuardia.csv",
             "Seattle - Seattle-Tacoma.csv")

data = read.csv(fileName[1])[,-1] # get rid of the station name
for(i in 2:10){
  data = rbind(data, read.csv(fileName[i])[,-1])
}

# clean data

data.tidy = data %>% 
  separate(col = "NAME", sep=", ", into = c("city_location","state_country")) %>%
  separate(col = "state_country", sep = " ", into = c("state", "country")) %>%
  select(-"country") %>%
  separate(col = "city_location", sep = " ", 
           extra = "merge", into = c("city", "exact_location")) %>%
  separate(col = "DATE", sep = "-", into = c("year", "month"), convert = TRUE) %>%
  mutate(MXTRM = EMXT - EMNT)

data.tidy$exact_location[data.tidy$city=="LOS"] = "INTERNATIONAL AIRPORT"
data.tidy$city[data.tidy$city=="LOS"] = "LOS ANGELES"



#### data imputation begins ####

# it is observed that the city on the south typically only have NA meaures for
# the snow related measures. Here we assume that they don't ever snow at all (so no need
# for measuring) and impute the missing values accordingly.

check_snow_na = data.tidy %>%
  select(city, year, month, DSNW, EMSD, EMSN, SNOW) %>% 
  filter(is.na(DSNW) | is.na(EMSD) | is.na(EMSN) | is.na(SNOW))

# notice that there are only 2 cities (MIAMI, LOS ANGELES: both locate south) having NA values. 
# Examine the issue further:

check_snow_city = data.tidy %>%
  select(city, year, month, DSNW, EMSD, EMSN, SNOW) %>% 
  filter(city == "LOS ANGELES" | city == "MIAMI")

# This table shows that even when they have values, the numerical values are still all 0. 
# This agrees with our assumption that they don't have snow at all
# So we feel safe to proceed and set the missing values in these 4 columns to be 0.

data.tidy = data.tidy %>%
  mutate(
    DSNW = ifelse(is.na(DSNW), 0, DSNW),
    EMSD = ifelse(is.na(EMSD), 0, EMSD),
    EMSN = ifelse(is.na(EMSN), 0, EMSN),
    SNOW = ifelse(is.na(SNOW), 0, SNOW)
  )

#### data imputation ends ####

write.csv(data.tidy, file = "project_weather_data.csv",row.names = FALSE)


# second dataframe: order by year and month
dat2 = data.tidy %>%
  arrange(year, month, city) %>%
  mutate(city = factor(city))

write.csv(dat2, file = "project_weather_data reordered by time.csv",row.names = FALSE)


# third datafram: change resolution to year, along with new derived variables
dat3 = data.tidy %>%
  group_by(city, year) %>%
  summarise(sd_AWND = sd(AWND), AWND = mean(AWND),
            DP10 = sum(DP10), DSNW = sum(DSNW), 
            DT00 = sum(DT00), DX32 = sum(DX32), 
            DX90 = sum(DX90), EMNT = min(EMNT),
            EMSD = max(EMSD), EMSN = max(EMSN),
            EMXP = max(EMXP), EMXT = max(EMXT),
            PRCP = sum(PRCP), SNOW = sum(SNOW),
            sd_TAVG = sd(TAVG), sd_TMAX = sd(TMAX),
            sd_TMIN = sd(TMIN), MXTR = EMXT - EMNT)

write.csv(dat3, file = "project_weather_data in year resolution.csv",row.names = FALSE)


