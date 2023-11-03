library(repurrrsive)
library(dplyr)
library(tidyr)
library(jsonlite)

# Load the gmaps_cities data
data(gmaps_cities)

# Inspect the structure
str(gmaps_cities)

library(purrr)
library(dplyr)

# A safe version of the flattening function that only tries to bind rows of data frames
safe_flatten <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  } else if (is.list(x)) {
    # Attempt to flatten lists of data frames
    data_frames <- keep(x, is.data.frame)
    if (length(data_frames) > 0) {
      # Only bind rows of data frames, ignore other types
      return(bind_rows(data_frames))
    }
    # For lists that don't contain data frames, attempt to simplify them
    return(as.data.frame(t(simplify2array(x))))
  } else {
    # Return atomic types as is
    return(as.data.frame(t(x)))
  }
}

# Apply the safe flattening function to each element
flattened_list <- map(gmaps_cities, safe_flatten)

# The result is a list of data frames that can be combined
# Since they may have different columns, use bind_rows with .id to keep track of the origin
flattened_df <- bind_rows(flattened_list, .id = "source")

# Flatten the list to a dataframe as much as possible
# This step depends on the structure you find with str()
# For example, if it's a list of lists:
#flattened_df <- map_df(gmaps_cities, flatten_if_possible)

# Write the flattened data to a JSON file
write_json(flattened_df, "data/gmaps_cities.json")
flattened_df
