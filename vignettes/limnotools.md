Limnotools Usage
================
Sam Albers and Doug Collinge
2017-02-02

Package loading
---------------

These packages are only need for this vignette and not for the limnotools package itself.

``` r
library(tidyverse)
```

Development version of limnotools
---------------------------------

Currently using sam\_exp branch

``` r
devtools::install_github("boshek/limnotools", ref="sam_exp")

library(limnotools)
```

Split and merge algorithm
-------------------------

Implementation of the split-and-merge algorithm result in two parts:

-   water\_layers function
-   water\_segments function
-   Then plotting for visual verification

Combine data
------------

Combine all the internal dataframes to illustrate mix layer estimation for many casts. Do this for both temperature and salinity

``` r
## rbind all the dataframes together
earlyspring$group <- 'earlyspring'
latesummer$group <- 'latesummer'
rbind_df <- rbind(earlyspring, latesummer)
```

Utilize the power of a dplyr pipe

``` r
## Convert data into grouped format
wtrprof_df <- rbind_df %>%
  select(depth, temper, salinity, group) %>% ## only keep desired columns
  gather(variable, value, -depth, -group)  ## convert data to long format
```

Use the water\_layers function
------------------------------

``` r
wl_df <- wtrprof_df %>%  
  group_by(variable, group) %>% ## group by variable and group
  do(water_layers(thres=0.1,z0=2.5,zmax=150,z=.$depth,sigma=.$value)) ##do a water_layer calc
wl_df
```

    ## Source: local data frame [4 x 5]
    ## Groups: variable, group [4]
    ## 
    ##   variable       group nimax   by_s_m thermodepth
    ##      <chr>       <chr> <dbl>    <dbl>       <dbl>
    ## 1 salinity earlyspring    19 2.634726    24.06902
    ## 2 salinity  latesummer    11 2.896572    49.17521
    ## 3   temper earlyspring     3 8.073714    22.63617
    ## 4   temper  latesummer     3 6.718292    16.33231

Use the water\_segments function
--------------------------------

``` r
s_df <- wtrprof_df %>%  
  group_by(variable, group) %>% ## group by variable and group
  do(water_segments(thres=0.1,z0=2.5,zmax=150,z=.$depth,sigma=.$value)) ##do a water_layer calc
s_df
```

    ## Source: local data frame [40 x 4]
    ## Groups: variable, group [4]
    ## 
    ##    variable       group       smz        sms
    ##       <chr>       <chr>     <dbl>      <dbl>
    ## 1  salinity earlyspring  2.547000 0.05080000
    ## 2  salinity earlyspring  2.634726 0.05041180
    ## 3  salinity earlyspring  7.722812 0.05025533
    ## 4  salinity earlyspring  9.769743 0.05015157
    ## 5  salinity earlyspring 11.699707 0.05024573
    ## 6  salinity earlyspring 13.922089 0.05005259
    ## 7  salinity earlyspring 15.676602 0.05006517
    ## 8  salinity earlyspring 17.431114 0.05016102
    ## 9  salinity earlyspring 23.279489 0.04973940
    ## 10 salinity earlyspring 24.858551 0.04974008
    ## # ... with 30 more rows

Plot the mix layer depths and segments over the water profiles
--------------------------------------------------------------

``` r
wtrprof_df %>%
  #filter(depth<50) %>%
  ggplot(aes(y=value,x=depth, colour=group)) +
  geom_path() +
  geom_path(data=s_df, aes(y=sms, x=smz), colour='black') +
  geom_vline(data=wl_df, aes(xintercept=by_s_m), colour='orange') +
  geom_vline(data=wl_df, aes(xintercept=thermodepth), colour='forestgreen') +
  facet_wrap(group~variable, scales = "free", ncol=2) +
  labs(y="Sigma (sms)", x="Depth (m; smz)", caption="Orange lines represent mix layer depth \n Black lines represent split-and-merge segments \n Green lines represent thermocline depth")
```

![](limnotools_files/figure-markdown_github/unnamed-chunk-7-1.png)
