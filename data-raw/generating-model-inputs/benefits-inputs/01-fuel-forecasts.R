#fuel price forecasts
#the price forecasts are developed from a range of sources.

#Australian institute of Petroleum data is used for historical data
#https://www.aip.com.au/aip-annual-retail-price-data
#this shows that real prices have basically been flat for the past 10 years.

#resultingly, our central estimate is no change to petrol prices.
#our 'low-price' fuel scenario is -1% change annually from 2019 rates
#(i.e. 0.77 in 2035 and $0.66 in 2050)
#our 'high-price' scenario is 1% change annually from 2019 rates.
#i.e. ~ $1.1 in 2035 compared to 0.9 today, and $1.25 in 2050

#this data was corrected to remove taxes applied to fuel to provide the before tax prices.
#tax data was obtained from here: https://data.gov.au/data/dataset/excise-data/resource/b9227cdf-4c04-492d-bd84-65031adc408e

#we have used 2019 as our base year given 2020 is an anomoly due to covid and 2021 data
#is incomplete/less accessible
#we have used an RACV report data: https://www.racv.com.au/royalauto/moving/news-information/premium-petrol-guzzling-cash.html
#to get a price comparison between the different fuel types. This % difference was assumed
#to remain constant in all years and all scenarios.

#the historical data, adjusted for inflation and stripped of taxes
#this shows that prices have declined since ~2007
historical_prices <- read_xlsx("data/aip/AIP_Annual_Retail_Price_Data.xlsx",
                            sheet = "adjusted-historical") %>%
  pivot_longer(cols = (2:3),
               names_to = "fuel",
               values_to = "prices")

historical_prices %>%
  filter(year != 2020) %>%
  ggplot(aes(x = year, y = prices, colour = fuel)) +
  geom_point() +
  geom_line(aes(colour = fuel)) +
  geom_smooth(aes(colour = fuel), method = "lm") +
  scale_y_continuous_grattan(limits = c(0, 1.5))


#however, looking at the longer term picture, prices are mostly flat over the past ~20 years.
#ie. page 2 (page 6 of pdf). As a result, our central scenario assumes rices remain flat
#https://www.accc.gov.au/system/files/20-27RPT_Petrol%2520Quarterly%2520Report%2520-%2520June%25202020_FA.pdf

fuel_forecasts <- read_xlsx("data/aip/AIP_Annual_Retail_Price_Data.xlsx",
                  sheet = "petrol-forecasts") %>%
  #in order to get our residual values beyond 2050 we're going to freeze prices at
  #2050 levels and extend these to 2050
  group_by(scenario) %>%
  complete(year = (2051:2060)) %>%
  arrange(scenario, year) %>%
  #and filling our missing values
  na.locf() %>%
  pivot_longer(cols = (3:6),
               names_to = "fuel_type",
               values_to = "price")


write_rds(fuel_forecasts, "data/model-inputs/fuel-forecasts.rds")
