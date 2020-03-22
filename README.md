CoV19
================
Eli Holmes

This is a package for exploring some of the CoV-19 data. The world data
is downloaded from JHU. The US data is downloaded from JHU and
CovidTracking. The Italy data is downloaded from the Italian CDC
(Protezione Civile)

Sources: \* <https://github.com/CSSEGISandData/COVID-19> \*
<https://covidtracking.com/api/> \*
<https://github.com/pcm-dpc/COVID-19>

To install the package

    library(devtools)
    install_github(eeholmes/CoV19)

To get the data, type any of these on command line. After loading.

    states
    italy
    world

Use `head()` to look at it. Should be pretty self-evident what it is.

# Plots

There aren’t many. Yet. The basic one I am working on is new cases
versus cases after a lockdown.

``` r
library(CoV19)
plot1("states", "WA", lockdown="2020-03-16")
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
plot1("italy", "Lombardia", lockdown="2020-03-11")
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Something is odd with the Hubei data in the JHU data set.

``` r
plot1("world", "Hubei", lockdown="2020-01-23")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
