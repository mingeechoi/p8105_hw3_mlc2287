Homework 3
================
Mingee Choi
10/15/2022

Load Packages

``` r
library(tidyverse)
library(ggridges)
library(patchwork)
library(dplyr)
library(p8105.datasets)

knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

# Problem 2

Load, tidy, and wrangle accel_data.csv

``` r
accel_df =
  read_csv("./data/accel_data.csv")%>%
  janitor::clean_names()%>%
  pivot_longer(
    activity_1:activity_1440,
    names_to = "activity_count",
    values_to = "minute_of_day"
  )%>%
  mutate(weekend_weekday = ifelse(day %in% c("Friday", "Saturday", "Sunday"), "weekend", "weekday"))%>%
  mutate(day=as.factor(day))%>%
  mutate(weekend_weekday=as.factor(weekend_weekday))%>%
  select(week, day_id, day, weekend_weekday, everything())
```

    ## Rows: 35 Columns: 1443
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr    (1): day
    ## dbl (1442): week, day_id, activity.1, activity.2, activity.3, activity.4, ac...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

This dataset contains `50,400` observations of `6` variables. Variables
include week, day_id, day, weekend_weekday, activity_count, and
minute_of_day.

Aggregate across minutes to create a total activity variable for each
day, create a table, and look at trends

``` r
  accel_df$day = ordered(accel_df$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
accel_df %>%
  group_by(week, day) %>%
  summarize(total_activity = sum(minute_of_day))%>%
 pivot_wider(
   names_from = day,
   values_from = total_activity
 ) %>%
    knitr::kable(digits = 1)
```

    ## `summarise()` has grouped output by 'week'. You can override using the
    ## `.groups` argument.

| week |   Monday |  Tuesday | Wednesday | Thursday |   Friday | Saturday | Sunday |
|-----:|---------:|---------:|----------:|---------:|---------:|---------:|-------:|
|    1 |  78828.1 | 307094.2 |    340115 | 355923.6 | 480542.6 |   376254 | 631105 |
|    2 | 295431.0 | 423245.0 |    440962 | 474048.0 | 568839.0 |   607175 | 422018 |
|    3 | 685910.0 | 381507.0 |    468869 | 371230.0 | 467420.0 |   382928 | 467052 |
|    4 | 409450.0 | 319568.0 |    434460 | 340291.0 | 154049.0 |     1440 | 260617 |
|    5 | 389080.0 | 367824.0 |    445366 | 549658.0 | 620860.0 |     1440 | 138421 |

There aren’t any obvious trends, but people were slightly more active on
Friday than Monday-Thursday and Saturday. People were least active on
Saturday, especially for week 4 and 5.

# Problem 3
