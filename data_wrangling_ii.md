Data Wrangling II
================
Vanessa Dinh

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
```

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_html = read_html(url)

drug_use_html
```

    ## {html_document}
    ## <html lang="en">
    ## [1] <head>\n<link rel="P3Pv1" href="http://www.samhsa.gov/w3c/p3p.xml">\n<tit ...
    ## [2] <body>\r\n\r\n<noscript>\r\n<p>Your browser's Javascript is off. Hyperlin ...

``` r
drug_use_html %>% 
  html_table() %>% 
  first() %>% 
  slice(-1)
```

    ## # A tibble: 56 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 Tota… 12.90a  13.36   0.002   13.28b  12.86   0.063   31.78   32.07   0.369  
    ##  2 Nort… 13.88a  14.66   0.005   13.98   13.51   0.266   34.66a  36.45   0.008  
    ##  3 Midw… 12.40b  12.76   0.082   12.45   12.33   0.726   32.13   32.20   0.900  
    ##  4 South 11.24a  11.64   0.029   12.02   11.88   0.666   28.93   29.20   0.581  
    ##  5 West  15.27   15.62   0.262   15.53a  14.43   0.018   33.72   33.19   0.460  
    ##  6 Alab… 9.98    9.60    0.426   9.90    9.71    0.829   26.99   26.13   0.569  
    ##  7 Alas… 19.60a  21.92   0.010   17.30   18.44   0.392   36.47a  40.69   0.015  
    ##  8 Ariz… 13.69   13.12   0.364   15.12   13.45   0.131   31.53   31.15   0.826  
    ##  9 Arka… 11.37   11.59   0.678   12.79   12.14   0.538   26.53   27.06   0.730  
    ## 10 Cali… 14.49   15.25   0.103   15.03   14.11   0.190   33.69   32.72   0.357  
    ## # … with 46 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`

## SW data

``` r
url = "https://www.imdb.com/list/ls070150896/"

swm_html = read_html(url)
```

How do I get the stuff I want

``` r
sw_titles = 
  swm_html %>% 
  html_elements(".lister-item-header a") %>% 
  html_text()

sw_runtime = 
  swm_html %>% 
  html_elements(".runtime") %>% 
  html_text()


sw_money = 
  swm_html %>% 
  html_elements(".text-small:nth-child(7) span:nth-child(5)") %>% 
  html_text() 

sw_df =
  tibble(
    title = sw_titles,
    runtime = sw_runtime,
    money = sw_money
  )
```

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

## APIs

``` r
water_df = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.csv") %>% 
  content("parsed")
```

    ## Rows: 43 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (4): year, new_york_city_population, nyc_consumption_million_gallons_per...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
brfss_smart2010 = 
  GET("https://chronicdata.cdc.gov/resource/acme-vg9e.csv",
      query = list("$limit" = 5000)) %>% 
  content("parsed")
```

    ## Rows: 5000 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): locationabbr, locationdesc, class, topic, question, response, data...
    ## dbl  (6): year, sample_size, data_value, confidence_limit_low, confidence_li...
    ## lgl  (1): locationid
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
poke = 
  GET("http://pokeapi.co/api/v2/pokemon/1") %>%
  content()

poke$name
```

    ## [1] "bulbasaur"
