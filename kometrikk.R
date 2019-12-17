#
# Kømetrikk: hvor mange timer faller utenfor regelen:
# Timetrafikk hvor mer enn 15 % av kjøretøyene har fart under 20 km/h
# skal tas ut av datagrunnlaget.
#

# Pakker ####
library(data.table)
library(tidyverse)
library(lubridate)

# Analyse ####
vbvdata <- fread("1600219_20180601000000-20180701000000.csv",
                 dec = ",", drop = c(2:4,6:15))
colnames(vbvdata) <- c("timestamp", "speed")

timetrafikk <- vbvdata %>%
  mutate(timestamp = with_tz(ymd_hms(timestamp), "CET"),
         speed = abs(speed),
         day = mday(timestamp),
         hour = hour(timestamp)
  ) %>%
  group_by(day, hour) %>%
  summarise(slow = sum(speed < 20),
            fast = sum(speed >= 20),
            slowshare = round(slow / (fast + slow), digits = 2)
      )

timetrafikk %>%
  ggplot(aes(hour, slowshare)) +
  geom_point() +
  geom_hline(yintercept = 0.15) +
  ggtitle("1600219", subtitle = "juni 2018")

kotimer <- timetrafikk %>%
  filter(slowshare >= 0.15)

queuedays <- timetrafikk %>%
  group_by(day) %>%
  summarise(slowhours = sum(slowshare >= 0.15))

#.