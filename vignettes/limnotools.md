Limnotools Usage
================
Sam Albers and Doug Collinge
2017-05-02

Package loading
---------------

tidyverse is only needed for this vignette and not for the limnotools package itself though it is obviously a very useful tool.

``` r
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(knitr)
```

### Development version of limnotools

TO install limnotools, you need the devtools package installed. Then you can install limntools in R from github like this:

``` r
devtools::install_github("boshek/limnotools", ref="sam_exp")
library(limnotools)
```

Split and merge algorithm
-------------------------

Water column identification is provided by the split-and-merge algorithm. Implementation of the split-and-merge algorithm for a water profile occurs within two functions:

-   wtr\_layer function
-   wtr\_segments function

Useful dataset in limnotools
----------------------------

``` r
data("earlyspring")
data("latesummer")
data("latefall")
data("t11")
```

Simple application of the split and merge algorithm
---------------------------------------------------

Below is a simple one profile example of determining key water column parameters using the split-and-merge algorithm. Most users will only use two functions that are part of the limnotools package. Cline depth and mix layer depth are calculated using the wtr\_layers() function. Coordinates for the segments of the water profile are calculated using wtr\_segments. For more information, type the following into R:

``` r
?wtr_layer
?wtr_segments
```

The default behaviour for both functions is to run the algorithm *without* specifying the number of segments. Moreover, both functions adopt as defaults the convention of a minimum depth (z0) of 2.5 m, a maximum depth (zmax) of 150 m and a error threshold (thres) of 0.1.

``` r
wldf <- wtr_layer(depth = latesummer$depth, measure = latesummer$temper)
knitr::kable(wldf)
```

|  min\_depth|  nseg|     mld|     cline|
|-----------:|-----:|-------:|---------:|
|         2.5|     4|  7.0565|  16.39025|

Note that the axes of the water column profile have been reversed and flipped to better visualize the water column and conform to standard limnological displays.

``` r
plot(y = latesummer$depth, x = latesummer$temper, ylim = rev(range(latesummer$depth)))
abline(h = wldf$cline, col='blue')
abline(h = wldf$mld, col='red')
abline(h = wldf$min_depth, col='green')
text(16, wldf$cline+3, "Thermocline", col = 'blue')
text(16, wldf$mld+3, "Mix Layer Depth", col = 'red')
text(16, wldf$min_depth+3, "Minimum Depth", col = 'green')
```

![](C:\Users\tills\AppData\Local\Temp\RtmpgZ6SvA\preview-247834fd46b2.dir\limnotools_files/figure-markdown_github/unnamed-chunk-6-1.png)

More complicated example using many datafiles
---------------------------------------------

Many users will face situations where they have multiple profiles and would like to evaluate layers and/or segments on many files. There are several approaches to this type of 'grouping' problem in R. We will use the most popular approach - dplyr - and the functionla programming tool - purrr - which is part of the [tidyverse](https://CRAN.R-project.org/package=tidyverse). To generate data for this example we first need to combine all the internal dataframes from limnotools to illustrate mix layer estimation for many casts. To simplify and decrease runtime we will only do this for temperature and salinity.

``` r
## rbind all the dataframes together
earlyspring$group <- 'earlyspring'
latesummer$group <- 'latesummer'
latefall$group <- 'latefall'
rbind_df <- dplyr::bind_rows(earlyspring, latesummer, latefall)
```

We can utilize the power of a dplyr pipe (%&gt;%) and gather to convert this data into a long form.

``` r
## Convert data into grouped format
wtrprof_df <- rbind_df %>%
  select(depth, temper, salinity, group) %>% ## only keep desired columns
  gather(variable, value, -depth, -group)  ## convert data to long format
```

Use group\_by() and do() to run wtr\_layer() by group and variable outputting a dataframe.

``` r
safe_wtr_layer <- purrr::safely(wtr_layer)

wl_df <- wtrprof_df %>%  
  filter(variable == "temper") %>%
  group_by(variable, group) %>% ## group by variable and group
  nest() %>% ## create list col
  mutate(wl = map2(data, variable, ~safe_wtr_layer(depth = .$depth, measure = .$value))) %>%
  mutate(err = map(wl, 'error'),
         layer_l = map(wl, 'result')) %>%
  select(-wl) %>%
  unnest(layer_l)
wl_df
```

    ## # A tibble: 3 × 8
    ##   variable       group                 data    err min_depth  nseg     mld
    ##      <chr>       <chr>               <list> <list>     <dbl> <dbl>   <dbl>
    ## 1   temper earlyspring   <tibble [604 × 2]> <NULL>       2.5     4  8.1410
    ## 2   temper  latesummer   <tibble [881 × 2]> <NULL>       2.5     4  7.0565
    ## 3   temper    latefall <tibble [2,281 × 2]> <NULL>       2.5     4 10.4335
    ## # ... with 1 more variables: cline <dbl>

The same applies to wtr\_segments()

``` r
safe_wtr_segments <-purrr::safely(wtr_segments)

s_df <- wtrprof_df %>%  
  filter(variable == "temper") %>%
  group_by(variable, group) %>% ## group by variable and group
  nest() %>% ## create list col
  mutate(wl = map2(data, variable, ~safe_wtr_segments(depth = .$depth, measure = .$value))) %>%
  mutate(err = map(wl, 'error'),
         layer_l = map(wl, 'result')) %>%
  select(-wl) %>%
  unnest(layer_l)
s_df
```

    ## # A tibble: 12 × 6
    ##    variable       group min_depth  nseg    depth  measure
    ##       <chr>       <chr>     <dbl> <dbl>    <dbl>    <dbl>
    ## 1    temper earlyspring       2.5     4   2.5470  8.07520
    ## 2    temper earlyspring       2.5     4   8.1410  7.70020
    ## 3    temper earlyspring       2.5     4  37.2030  4.92325
    ## 4    temper earlyspring       2.5     4  49.3265  4.46620
    ## 5    temper  latesummer       2.5     4   2.5980 17.94060
    ## 6    temper  latesummer       2.5     4   7.0565 17.38405
    ## 7    temper  latesummer       2.5     4  25.7240  5.51445
    ## 8    temper  latesummer       2.5     4  98.1390  4.46375
    ## 9    temper    latefall       2.5     4   2.5070  8.38980
    ## 10   temper    latefall       2.5     4  10.4335  8.32810
    ## 11   temper    latefall       2.5     4  53.9045  4.33720
    ## 12   temper    latefall       2.5     4 149.8985  3.65640

Lastly we can plot the mix layer and cline depths and segments over the water profiles using the same limnological visualization convention described above and using ggplot2 (part of the tidyverse).

``` r
wtrprof_df %>%
  filter(variable == "temper") %>%
  ggplot(aes(x = value,y = depth)) +
  geom_path(colour = 'purple') +
  geom_path(data = s_df, aes(x = measure, y = depth), colour = 'black') +
  geom_point(data = s_df, aes(x = measure, y = depth), colour = 'black') +
  geom_hline(data = wl_df, aes(yintercept = mld, colour = group)) +
  scale_y_reverse() +
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  labs(y = "Temperature", x = "Depth (m)", 
       caption = "Black lines represent split-and-merge segments \n Mix layer depth =mld")
```

![](C:\Users\tills\AppData\Local\Temp\RtmpgZ6SvA\preview-247834fd46b2.dir\limnotools_files/figure-markdown_github/unnamed-chunk-11-1.png)
