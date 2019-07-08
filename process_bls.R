# load and process BLS data

require(readr)
require(dplyr)

# load BLS LAU data

lau_county <- read_tsv("la_county.County")

lau_perc <- filter(lau_county,value<101)

# load the crosswalk

lau_cross <- read_tsv("la_series.txt")

# merge in the cross

lau_perc <- left_join(lau_perc,lau_cross,by="series_id")

# only unemployment rates

lau_perc <- filter(lau_perc,grepl(pattern = "Unemployment Rate",x=series_title))
