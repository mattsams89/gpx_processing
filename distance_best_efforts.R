library(trackeR)
library(hms)
library(data.table)

# Need to include correct file path in the readGPX call
half_data <- fortify.zoo(trackeRdata(readGPX("D:/Downloads/2020-10-30 Half Marathon.gpx"), sport = "running")[[1]])

search_distances <- c(100, 200, 400, 800, 1000, 2000, 3000, 5000, 10000, 15000, 20000, 21097)

distance_best_efforts <- function(.df, distances){
  # fortify.zoo returns a df; sometimes I've already converted to a dt, though, but worth a check
  if(!is.data.table(.df))
    .df <- as.data.table(.df)[distance > 0 & speed > 0]
  else
    .df <- .df[distance > 0 & speed > 0] # Remove any times I wasn't moving
  
  # lapply over the distances of interest
  effort_duration <- lapply(distances, function(d){
    # Prevents checking for distances > session distance
    if(.df[, max(distance)] < d)
      duration <- as_hms(NA) # as_hms attaches attributes to the vector and won't rbindlist a simple NA as a result
    else{
      # If the session is long enough for the given distance, proceed to find the fastest split for that distance
      # For potential starting points, we only want to retain distances where it's possible for the split to exist
      starting_points <- copy(.df)[distance <= (max(distance) - d)
      ][, minimum_distance := distance + d] # copy the df and add a minimum_distance field for the join below
      
      # Likewise, it isn't worth keeping any search points < the distance of interest
      search_points <- .df[distance >= d]
      
      # I decided on a non-equi join after playing with a nested lapply for a while...MUCH faster on
      # the tests I've run so far...like, 60+ seconds faster for a test half marathon file
      # The join returns a huge dt, so I opted to include the min(difftime) as part of the join operations
      # the as_hms is just to make it pretty :D
      duration <- search_points[starting_points,
                                nomatch = NULL, # Not sure if necessary in this case, but have it anyway
                                on = .(distance >= minimum_distance),
                                as_hms(min(difftime(Index, i.Index, units = "mins")))]
    }
    data.table(effort_distance = d, duration = duration)
  })
  # Return as a dt as opposed to a list
  rbindlist(effort_duration)
}

distance_best_efforts(half_data, search_distances)
