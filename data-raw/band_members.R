library(tidyverse)

# fmt: skip
band_members <- tribble(
  ~ name,    ~ band,
  "Mick",  "Stones",
  "John", "Beatles",
  "Paul", "Beatles"
)

devtools::use_data(band_members)

# fmt: skip
band_instruments <- tribble(
  ~ name,   ~ plays,
  "John",  "guitar",
  "Paul",    "bass",
  "Keith", "guitar"
)

devtools::use_data(band_instruments)

# fmt: skip
band_instruments2 <- tribble(
  ~ artist, ~ plays,
  "John",  "guitar",
  "Paul",    "bass",
  "Keith",  "guitar"
)

devtools::use_data(band_instruments2)
