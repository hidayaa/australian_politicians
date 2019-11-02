library(AustralianPoliticians)
library(lubridate)
library(tidyverse)

all <- AustralianPoliticians::all %>% as_tibble()
by_division <- AustralianPoliticians::by_division_mps %>% as_tibble()

by_division <- by_division %>%
  select(uniqueID, mpsDivision,mpsFrom,mpsTo)

dates_of_interest <- c(ymd("1901-01-01"):today())

by_division_test <- purrr::map_dfr(seq_len(length(dates_of_interest)), ~by_division)

dates_of_interest <- rep(dates_of_interest, nrow(by_division))

dates_of_interest <- sort(dates_of_interest)

by_division_test$date <- as_date(dates_of_interest)

rm(dates_of_interest)

tail(by_division_test)

by_division_test$mpsTo <- replace_na(by_division_test$mpsTo, today())

by_division_test$in_there <- int_overlaps(interval(by_division_test$mpsFrom, by_division_test$mpsTo),
                                          interval(by_division_test$date, by_division_test$date)
                                          )

number_by_date <- by_division_test %>%
  group_by(date, in_there) %>%
  summarise(number = n()) %>%
  ungroup()

number_by_date <- number_by_date %>%
  filter(in_there == TRUE)

number_by_date <- number_by_date %>%
  mutate(decade = year(date),
         decade = decade - decade %% 10)

elections <- c("1901-03-29", "1903-12-16", "1906-12-12", "1910-04-13", "1913-05-31",
               "1914-09-05", "1917-05-05", "1919-12-13", "1922-12-16", "1925-11-14",
               "1928-11-17", "1929-10-12", "1931-12-19", "1934-09-15", "1937-10-23",
               "1940-09-21", "1943-08-21", "1946-09-28", "1949-12-10", "1951-04-28",
               "1954-05-29", "1955-12-10", "1958-11-22", "1961-12-09", "1963-11-30",
               "1966-11-26", "1969-10-25", "1972-12-02", "1974-05-18", "1975-12-13",
               "1977-12-10", "1980-10-18", "1983-03-05", "1984-12-01", "1987-07-11",
               "1990-03-24", "1993-03-13", "1996-03-02", "1998-10-03", "2001-11-10",
               "2004-10-09", "2007-11-24", "2010-08-21", "2013-09-07", "2016-07-02",
               "2019-05-18")
elections <- as_date(elections)

number_by_date <- number_by_date %>%
  filter(!date %in% elections)




number_by_date %>%
  ggplot(aes(x = date, y = number)) +
  geom_point() +
  facet_wrap(vars(decade), scales = "free_x") +
  theme_classic()





