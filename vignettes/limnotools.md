Limnotools Usage
================
Sam Albers and Doug Collinge
2017-02-17

Package loading
---------------

tidyverse is only needed for this vignette and not for the limnotools package itself though it is obviously a very useful tool.

``` r
library(tidyverse)
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
wldf
```

    ##   min_depth nseg   mld   cline
    ## 1         1    4 6.251 15.9315

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

![](limnotools_files/figure-markdown_github/unnamed-chunk-6-1.png)

More complicated example using many datafiles
---------------------------------------------

Many users will face situations where they have multiple profiles and would like to evaluate layers and/or segments on many files. There are several approaches to this type of 'grouping' problem in R. We will use the most popular approach - dplyr - which is part of the [tidyverse](https://CRAN.R-project.org/package=tidyverse). To generate data for this example we first need to combine all the internal dataframes from limnotools to illustrate mix layer estimation for many casts. To simplify and decrease runtime we will only do this for temperature and salinity.

``` r
## rbind all the dataframes together
earlyspring$group <- 'earlyspring'
latesummer$group <- 'latesummer'
rbind_df <- rbind(earlyspring, latesummer)
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
wl_df <- wtrprof_df %>%  
  group_by(variable, group) %>% ## group by variable and group
  do(wtr_layer(depth=.$depth,measure=.$value)) %>% ##do a water_layer calc
  select(-nseg) %>% ##nseg not needed here
  gather(Layer, value, -variable, -group) %>% ##gather for plotting purposes
  filter(variable == "temper")
```

    ## Warning in cline_calc(z_seg = sam_list[["smz"]], sigma_seg =
    ## sam_list[["sms"]]): Algorithm calculates cline to be in top segment. This
    ## is likely due to surface scatter. Using the next interval.

``` r
wl_df
```

    ## Source: local data frame [6 x 4]
    ## Groups: variable, group [2]
    ## 
    ##   variable       group     Layer    value
    ##      <chr>       <chr>     <chr>    <dbl>
    ## 1   temper earlyspring min_depth  1.00000
    ## 2   temper  latesummer min_depth  1.00000
    ## 3   temper earlyspring       mld  3.55500
    ## 4   temper  latesummer       mld  6.25100
    ## 5   temper earlyspring     cline 17.68575
    ## 6   temper  latesummer     cline 15.93150

The same applies to wtr\_segments()

``` r
s_df <- wtrprof_df %>%  
  group_by(variable, group) %>% ## group by variable and group
  do(wtr_segments(depth = .$depth, measure = .$value)) %>% ##do a water_layer calc
  filter(variable == "temper")
s_df
```

    ## Source: local data frame [8 x 6]
    ## Groups: variable, group [2]
    ## 
    ##   variable       group min_depth  nseg   depth  measure
    ##      <chr>       <chr>     <dbl> <dbl>   <dbl>    <dbl>
    ## 1   temper earlyspring         1     4  1.0120  8.45310
    ## 2   temper earlyspring         1     4  3.5550  7.97280
    ## 3   temper earlyspring         1     4 31.8165  5.52725
    ## 4   temper earlyspring         1     4 49.3265  4.46620
    ## 5   temper  latesummer         1     4  1.0230 18.09700
    ## 6   temper  latesummer         1     4  6.2510 17.47215
    ## 7   temper  latesummer         1     4 25.6120  5.53080
    ## 8   temper  latesummer         1     4 98.1390  4.46375

Lastly we can plot the mix layer and cline depths and segments over the water profiles using the same limnological visualization convention described above and using ggplot2 (part of the tidyverse).

``` r
wtrprof_df %>%
  filter(variable == "temper") %>%
  ggplot(aes(x = value,y = depth)) +
  geom_path(colour = 'purple') +
  geom_path(data = s_df, aes(x = measure, y = depth), colour = 'black') +
  geom_point(data = s_df, aes(x = measure, y = depth), colour = 'black') +
  geom_hline(data = wl_df, aes(yintercept = value, colour = Layer)) +
  scale_y_reverse() +
  facet_wrap(~group, scales = "free", ncol = 1) +
  labs(y = "Temperature/Salinity", x = "Depth (m)", 
       caption = "Black lines represent split-and-merge segments \n Mix layer depth =mld \n  Thermocline depth=cline")
```

![](limnotools_files/figure-markdown_github/unnamed-chunk-11-1.png)
