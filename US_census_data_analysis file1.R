library(tidycensus)
library(tidyverse)
api_key <- "b1f36656fd5251f0d9d3207374fdc9f31bd6e318"
census_api_key(api_key)
Sys.getenv("CENSUS_API_KEY")
state_pop <- get_decennial(geography = "state", variables = "P001001")
head(state_pop)

