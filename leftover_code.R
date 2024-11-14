

get_counties_deprecated <- function() {
  # Get all counties
  query_points <-
    "query counties {
       areas {
         counties {
           number
           name
         }
       }
    }"

  myqueries <- Query$new()
  myqueries$query("points", query_points)

  counties <- cli$exec(myqueries$queries$points) %>%
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
    as.data.frame() %>%
    dplyr::rename(county_number =
                    data.areas.counties.number,
                  county_name =
                    data.areas.counties.name
    ) %>%
    mutate(
      geo_number =
        case_when(
          county_number ==  3 ~ 1,
          county_number == 30 ~ 2,
          county_number == 34 ~ 3,
          county_number == 38 ~ 4,
          county_number == 42 ~ 5,
          county_number == 11 ~ 6,
          county_number == 46 ~ 7,
          county_number == 15 ~ 8,
          county_number == 50 ~ 9,
          county_number == 18 ~ 10,
          county_number == 54 ~ 11
        )
    ) %>%
    arrange(geo_number)

  return(counties)
}
