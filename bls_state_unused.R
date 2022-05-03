# we are missing data for states with congressional districts that are the same as a state
# AK, DE, MT, ND SD, VT and WY for all years, all district 1

state_series <- read_tsv('data/la_series.txt') %>% 
  filter(area_type_code=="A",measure_code %in% c("04","06")) %>%
  mutate(series_id=trimws(series_id))

state_data <- read_tsv('data/la.data.txt')

state_series <- left_join(state_series,state_data, by="series_id")

state_series <- state_series %>%
  mutate(measure_code=recode(measure_code,`04`="unemp",
                             `06`="labor_force"),
         value=as.numeric(value)) %>%
  select(-series_id,-matches('footnote'),-seasonal,-srd_code,-series_title,-area_type_code) %>%
  spread(key = "measure_code",value="value")

area_codes <- read_tsv('data/la.area.txt') %>% 
  filter(area_type_code=="A") %>% 
  select(area_code,area_text)

state_series <- select(state_series,-begin_year,-begin_period,-end_year,
                       -end_period) %>%
  left_join(area_codes,by="area_code")

state_series <- state_series %>%
  filter(period!="M13") %>%
  mutate(period=recode(period,M01="January",
                       M02="February",
                       M03="March",
                       M04="April",
                       M05="May",
                       M06="June",
                       M07="July",
                       M08="August",
                       M09="September",
                       M10="October",
                       M11="November",
                       M12="December"),
         date_recode=paste0(year,"-",period,"-1"),
         date_recode=as.numeric(ymd(date_recode)))

# merge by FIPS code for states with only one district

state_series <- filter(state_series,area_text %in% c("Arkansas",
                                                     "Delaware",
                                                     "Montana",
                                                     "North Dakota",
                                                     "South Dakota",
                                                     "Vermont",
                                                     "Wyoming")) %>% 
  left_join(distinct(select(tigris::fips_codes,state_code,state_name)),
            by=c(area_text="state_name")) %>% 
  mutate(DISTRICT=1)