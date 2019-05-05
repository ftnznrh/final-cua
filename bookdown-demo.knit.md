--- 
title: "An Analysis of Weather Influence on Air Traffic Flows in Changi"
author: "Fatin Zunairah"
date: "2019-05-06"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: book
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
---



<!--chapter:end:index.Rmd-->


# Methods of Analysis

Placeholder



<!--chapter:end:01-analysis-methods.Rmd-->


# Data Tidying

Placeholder



<!--chapter:end:02-data-tidying.Rmd-->


# Multiple Regression

Placeholder



<!--chapter:end:03-regression.Rmd-->

# Dimension Reduction






























With the multiple independent variables in the data, it would be difficult to see the similarity or dissimilarity between two dependent variables. Dimension reduction techniques, however, allows the data analyst to see these information within a one-dimensional cartesian space. Here, we conduct two different methods of dimension reduction: MDS and Factor Analysis.

##Multi-Dimensional Scaling (MDS)

In MDS, dimension reduction is conducted based on the *distance* between each variable in the dataset. To perform MDS a distance matrix is first determined.  

###Daily Traffic & Weather Data {-}




```r
daily_weather_mds<-daily_stats_weather %>% #a sample is chosen to show more visual clarity
  sample_n(100)

daily_date<- daily_weather_mds %>% 
  select(date)

daily_weather_matrix<-daily_weather_mds %>% 
  select(-date) %>%
  as.matrix()
```



```r
daily_weather_distance<- distances::distances(daily_weather_matrix, normalize = "studentize") %>% 
  as.matrix()

daily_mds_weather <- cmdscale(daily_weather_distance) %>% 
  as_tibble()
```

```
## Warning: `as_tibble.matrix()` requires a matrix with column names or a `.name_repair` argument. Using compatibility `.name_repair`.
## This warning is displayed once per session.
```

```r
daily_mds_weather <- daily_date %>% 
  select(date) %>% 
  bind_cols(., daily_mds_weather)
```




```r
ggplot(daily_mds_weather, aes(x = V1, y = V2)) + geom_point(aes(colour = month(date)))
```

<img src="bookdown-demo_files/figure-html/unnamed-chunk-9-1.png" width="672" />
Looking at the MDS of daily weather and traffic data visually, we see that there are some distinct clusters of dates - indicating similarity/dissimilarity in their corresponding variables. There were no distinct clusters based on the month the data was tested. It shows that despite being from different months, its tested variables were similar to each other which could probably be due to an unknown, latent variable.

###Hourly Traffic & Weather Data {-}

```r
hourly_weather_mds<- hourly_stats_weather %>% 
  sample_n(100)

hourly_date<- hourly_weather_mds%>% 
  select(local_time)

hourly_weather_matrix<-hourly_weather_mds %>% 
  select(-local_time, -weather_id, -weather_main, -weather_description, -clouds_all, -date, -arrival_tfc, -departure_tfc) %>%
  as.matrix()
```



```r
hourly_weather_distance<- distances::distances(hourly_weather_matrix, normalize = "studentize") %>% 
  as.matrix()

hourly_mds_weather <- cmdscale(hourly_weather_distance) %>% 
  as_tibble()


hourly_mds_weather <- hourly_date %>% 
  select(local_time) %>% 
  bind_cols(., hourly_mds_weather)
```



```r
ggplot(hourly_mds_weather, aes(x = V1, y = V2)) + geom_point(aes(colour = hour(local_time)))
```

<img src="bookdown-demo_files/figure-html/unnamed-chunk-12-1.png" width="672" />

Results of MDS on the hourly weather shows more variance as compared to that of the daily weather (which had distinct clusters). However, when we look at the difference between the hours visually, there emerges a pattern. The lower half of the plot corresponded with the evening hours while the upper half had traffic from the morning hours.


###Data Analysis

Results of the MDS showed some similarities between the variables despite their vast differences in dates. This indicates that there are certain variables that would affect the Changi traffic flows more than other variables. These clusters would be interesting to investigate further so we can determine if there are other missing variables that could explain its similarity. The presence of outliers may also indicate an extreme dissimilarity between the variables tested.

##Factor Analysis

A factor analysis is performed here to investigate if there *is* a latent variable that would be able to explain the variances in the data. 

###Daily Traffic & Weather Data {-}

```r
fa_daily_stats <- daily_stats_weather %>% 
  sample_n(100) %>% 
  column_to_rownames(var = "date") %>% 
  principal(nfactors = 4, rotate = "varimax")

fa_daily_stats
```

```
## Principal Components Analysis
## Call: principal(r = ., nfactors = 4, rotate = "varimax")
## Standardized loadings (pattern matrix) based upon correlation matrix
##                        RC1   RC2   RC4   RC3   h2     u2 com
## daily_rainfall_mm     0.97 -0.22 -0.01 -0.03 0.99 0.0140 1.1
## 30min_rainfall_mm     0.96 -0.25  0.00 -0.03 0.98 0.0196 1.1
## 60min_rainfall_mm     0.97 -0.23  0.02 -0.05 0.99 0.0075 1.1
## 120min_rainfall_mm    0.97 -0.20  0.00 -0.04 0.99 0.0083 1.1
## temp_mean_daily      -0.29  0.90  0.10 -0.03 0.91 0.0949 1.2
## temp_daily_max       -0.10  0.81 -0.01  0.09 0.68 0.3248 1.1
## temp_daily_min       -0.44  0.75  0.00 -0.08 0.77 0.2341 1.6
## daily_tfc            -0.07  0.02  0.06  0.99 0.99 0.0085 1.0
## daily_wind_speed     -0.17  0.69  0.57  0.03 0.82 0.1752 2.1
## daily_max_wind_speed  0.07  0.04  0.96  0.05 0.94 0.0630 1.0
## 
##                        RC1  RC2  RC4  RC3
## SS loadings           4.07 2.71 1.26 1.01
## Proportion Var        0.41 0.27 0.13 0.10
## Cumulative Var        0.41 0.68 0.80 0.91
## Proportion Explained  0.45 0.30 0.14 0.11
## Cumulative Proportion 0.45 0.75 0.89 1.00
## 
## Mean item complexity =  1.2
## Test of the hypothesis that 4 components are sufficient.
## 
## The root mean square of the residuals (RMSR) is  0.04 
##  with the empirical chi square  16.12  with prob <  0.14 
## 
## Fit based upon off diagonal values = 0.99
```


```r
fa_daily_stats[['loadings']] %>% 
  unclass() %>% 
  as_tibble(rownames = "date") %>%
  gather(key = "component", value = "value", -date) %>% 
  ggplot(aes(x = date, y = value)) + geom_hline(yintercept=0)+
  geom_point() + coord_flip() + facet_grid(~component)
```

<img src="bookdown-demo_files/figure-html/unnamed-chunk-14-1.png" width="672" />



RC4 and RC3 explains 91% of the data's variance. We then plot them on cartesian coordinates to visualise it better. If there is a latent variable, we should anticipate that there would be certain clusters still available and a randomness should not be expected. The loadings in the factor analysis shows different types of modeling. In RC1, for example, the factor describes a model that describes high rainfall levels, low temperatures and normal daily traffic levels. Inversely in RC3, a high daily traffic in Changi relates to normal weather conditions. This indicates a high level relationship between weather and Changi air traffic flows. 


```r
fa_daily_stats[['scores']] %>% 
  unclass() %>% 
  as_tibble(rownames = "date") %>%
  ggplot(aes(x = RC4, y = RC3)) + geom_point(aes(colour = month(date)))
```

<img src="bookdown-demo_files/figure-html/unnamed-chunk-15-1.png" width="672" />

###Hourly Traffic & Weather Data {-}


```r
fa_hourly_stats <- hourly_stats_weather %>% 
  sample_n(100) %>% 
  column_to_rownames(var = "local_time") %>% 
  select(-weather_id, -weather_main, -weather_description, -clouds_all, -date, -departure_tfc, -arrival_tfc) %>% 
  principal(nfactors = 4, rotate = "varimax")

fa_hourly_stats
```

```
## Principal Components Analysis
## Call: principal(r = ., nfactors = 4, rotate = "varimax")
## Standardized loadings (pattern matrix) based upon correlation matrix
##               RC1   RC2   RC3   RC4   h2    u2 com
## temp         0.98  0.05 -0.01 -0.04 0.97 0.034 1.0
## temp_min     0.95 -0.03  0.00  0.07 0.91 0.091 1.0
## temp_max     0.93  0.15 -0.02 -0.11 0.90 0.097 1.1
## pressure    -0.15 -0.12  0.03  0.97 0.99 0.015 1.1
## humidity    -0.87 -0.04  0.09  0.12 0.78 0.216 1.1
## wind_speed   0.80 -0.04 -0.11 -0.15 0.67 0.333 1.1
## wind_deg     0.11  0.15 -0.77 -0.11 0.64 0.364 1.2
## rainfall_mm  0.00  0.22  0.75 -0.07 0.62 0.380 1.2
## total_tfc   -0.01  0.92  0.03 -0.03 0.85 0.154 1.0
## hour         0.08  0.88  0.03 -0.10 0.79 0.212 1.0
## 
##                        RC1  RC2  RC3  RC4
## SS loadings           4.17 1.73 1.18 1.03
## Proportion Var        0.42 0.17 0.12 0.10
## Cumulative Var        0.42 0.59 0.71 0.81
## Proportion Explained  0.51 0.21 0.15 0.13
## Cumulative Proportion 0.51 0.73 0.87 1.00
## 
## Mean item complexity =  1.1
## Test of the hypothesis that 4 components are sufficient.
## 
## The root mean square of the residuals (RMSR) is  0.07 
##  with the empirical chi square  48.94  with prob <  9.7e-07 
## 
## Fit based upon off diagonal values = 0.97
```


```r
fa_hourly_stats[['loadings']] %>% 
  unclass() %>% 
  as_tibble(rownames = "local_time") %>%
  gather(key = "component", value = "value", -local_time) %>% 
  ggplot(aes(x = local_time, y = value)) + geom_hline(yintercept=0)+
  geom_point() + coord_flip() + facet_grid(~component)
```

<img src="bookdown-demo_files/figure-html/unnamed-chunk-17-1.png" width="672" />

The factor analysis results hourly weather and traffic dataset provides us with similar relationships. We are able to visualise how humidity inversely affects the total amount of traffic handled in the hour. Since a higher humidity value is related to temperature and points towards adverse weather (high humidity = more clouds, generally), this could indicate how weather affects hourly air traffic flows in Changi.


```r
fa_hourly_stats[['scores']] %>% 
  unclass() %>% 
  as_tibble(rownames = "local_time") %>%
  ggplot(aes(x = RC2, y = RC3)) + geom_point(aes(colour = hour(local_time)))
```

<img src="bookdown-demo_files/figure-html/unnamed-chunk-18-1.png" width="672" />

In the above plot, there is a lack of ckustering of data points. However, we can also see that there is a distinct difference in the left and right half of the plots - the variables related to the evening hours have points clustered to the right half while the early hours have its variables clustered on the left hand side of the factor analysis plot. 



##Data Analysis

Dimension reduction techniques of multidimensional scaling and factor analysis performed in this section showed that the variables tested in the dataset had some evidence of clustering. In particular, the MDS process yielded some clustering in the daily weather and Chqngi traffic reports. However the MDS process only provided us with this evidence through the creation of a distance matrix, which does not sufficiently provide us with the extent of detail of each variable's relationship with each other. 

To do so, a factor analysis was conducted on the data. Its loadings, shown in the plots above did show the inverse relationship between weather and air traffic flows in Changi. There are still some evidences of clustering in the data similarly indicating a latent variable not compensated for in the data analysed here. 



<!--chapter:end:04-dimension-reduction.Rmd-->


# Cluster Analysis

Placeholder



<!--chapter:end:05-cluster-analysis.Rmd-->

# Future Studies & Conclusion

We are not able to fully explain or determine if weather is the only factor affecting Changi air traffic flows. However, we do see that there is a relationship between weather and traffic flows. These are evidenced by the loadings of each variable on the factor analysis conducted, as well as the multiple regression analysis of independent variables on daily traffic and hourly traffic numbers out of Changi. 

The extent of these effects are similarly shown in the multiple regression analysis. The amount of rainfall is the most statistically significant variable with a p-value of less than 0.05 - indicating that its influence on the air traffic flows in Changi are quite significant. 
Future studies should look at traffic reports/incidences which could also account for a reduction in air traffic flows in Changi.



<!--chapter:end:06-findings.Rmd-->

