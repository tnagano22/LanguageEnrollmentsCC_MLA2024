RProcedureMLA2024CUNYMLEnrollmentAndDiversity
================
- Tomonori Nagano
- 2023-12-27
- To analyze the diversity data and the modern language enrollment
  trends at CUNY between 1967 and 2021

## Setting up the environment

``` r
# clear the cache
rm(list = ls())

library(ggplot2); library(gdata); library(ggthemes); library(plyr); 
library(memisc); library(reshape); library(reshape2); library(devtools)
library(tidyverse); library(dplyr)
#library(devtools); library(ipeds); library(xtable); library(openxlsx); library(foreign); 
#install.packages("acs"); install.packages("ggplot2");xt install.packages("maps"); install.packages("mapdata"); 

# turning off scientific notation of numbers
options(scipen=999)

setwd("~/Desktop/")

# change the default width
width.default <- getOption("width"); options(width=110)

# the add comma fonction
addComma<-function(x) {format(x, big.mark = ',', trim = TRUE, scientific = FALSE)}

# creating a notin function
`%notin%` <- Negate(`%in%`)

# defining own color set
library(RColorBrewer)
# myColors <- c("green","lightblue","#FDAE61","#FEE090","#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695","#FFFFBF","#FFFF99","#BEAED4","#FDC086","#1F78B4","#33A02C")
myColors <- brewer.pal(6,"BuGn")
```

## Analyzing the CUNY’s diversity data

### CUNY ethnicity data in 1967

- From “Office of Institutional Research and Assessment 1967 - 1968 Data
  Book”
  - https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/
  - Appendix IV: Ethnic Survey for Fall 1967 Extrapolation Of Total
    Enrollment Based on Data Collected From 86.7% Sample
  - Counts of matriculated students
  - The labeling of the following ethnicity levels have been modified.
    - “Negro” to “Black or African American” (see
      <https://www.pewresearch.org/social-trends/2010/01/21/race-and-the-census-the-negro-controversy/>
      for the background of the term “Negro” in the census and other
      survey data until 2000)
    - “Puerto Rican” to “Hispanic”

``` r
tempData.1967 = read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity1967.csv", sep = ",")
tempData.1967 = melt(tempData.1967,c("Year","Ethnicity"))
tempData.1967[tempData.1967=="Negro"] <- "Black or African American"
tempData.1967[tempData.1967=="Puerto Rican"] <- "Hispanic"

colnames(tempData.1967) = c("Year","Ethnicity","Control","Enrollment")
head(tempData.1967)
```

    ##   Year                 Ethnicity   Control Enrollment
    ## 1 1967                     White    Senior      56560
    ## 2 1967 Black or African American    Senior       2285
    ## 3 1967                  Hispanic    Senior        975
    ## 4 1967                     Other    Senior       2268
    ## 5 1967               No Response    Senior        468
    ## 6 1967                     White Community      12906

### CUNY ethnicity data in 1969

- From “Office of Institutional Research and Assessment 1969 - 1970 Data
  Book”
  - https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/
  - Table III: The Distribution of Students Based Upon the Type of
    College in Which They Were Enrolled (p.42)
  - Only proportions were reported in the report. The actual number of
    students were interpolated from the total number of matriculated
    students (see Table I: Enrollment Fall 1964, 1968, 1969 on p.3)
  - The labeling of the following ethnicity levels have been modified.
    - “Black” to “Black or African American”
    - “Puerto Rican” to “Hispanic”

``` r
tempData.1969 = read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity1969.csv", sep = ",")
tempData.1969$Enrollment = as.integer(tempData.1969$EnrollmentP * tempData.1969$EnrollmentT / 100)
tempData.1969 = tempData.1969[tempData.1969$Year!=1967,c("Year","Control","Ethnicity","Enrollment")]
tempData.1969[tempData.1969=="Black"] <- "Black or African American"
tempData.1969[tempData.1969=="Puerto Rican"] <- "Hispanic"

head(tempData.1969)
```

    ##    Year   Control                 Ethnicity Enrollment
    ## 6  1968 Community                     White      16011
    ## 7  1968 Community Black or African American       4885
    ## 8  1968 Community                  Hispanic       1379
    ## 9  1968 Community                     Other        677
    ## 10 1968 Community               No Response        420
    ## 11 1969 Community                     White      16176

### CUNY ethnicity data in 1972

- From “Office of Institutional Research and Assessment 1972 - 1973 Data
  Book”
  - https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/
  - Data from the ethnic survey in 1972
  - Table V Ethnic Distribution of Undergraduate Students by Type of
    College Expressed in Percentages 1968 - 1972 (p.42)
  - Only proportions were reported in the report. The actual number of
    students were interpolated from the total number of matriculated
    students (see Table I The City University of New York Enrollment,
    Fall, 1968, 1971, 1972 on p.19)
  - The labeling of the following ethnicity levels have been modified.
    - “Puerto Rican” to “Hispanic”
    - “Black” to “Black or African American”
    - “Other Spanish Surnamed Americans” to “Hispanic”
    - “American Indian” to “American Indian or Native Alaskan”
    - “Oriental” to “Asian or Pacific Islander”

``` r
tempData.1972 = read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity1972.csv", sep = ",")
tempData.1972$Enrollment = as.integer(tempData.1972$EnrollmentP * tempData.1972$EnrollmentT / 100)
tempData.1972 = tempData.1972[tempData.1972$Year %in% c(1971,1972),c("Year","Control","Ethnicity","Enrollment")]

tempData.1972[tempData.1972=="Puerto Rican"] <- "Hispanic"
tempData.1972[tempData.1972=="Black"] <- "Black or African American"
tempData.1972[tempData.1972=="Other Spanish Surnamed Americans"] <- "Hispanic"
tempData.1972[tempData.1972=="American Indian"] <- "American Indian or Native Alaskan"
tempData.1972[tempData.1972=="Oriental"] <- "Asian or Pacific Islander"

head(tempData.1972)
```

    ##    Year   Control                         Ethnicity Enrollment
    ## 22 1971 Community                             White      28761
    ## 23 1971 Community         Black or African American      13972
    ## 24 1971 Community                          Hispanic       4129
    ## 25 1971 Community                          Hispanic          0
    ## 26 1971 Community American Indian or Native Alaskan        144
    ## 27 1971 Community         Asian or Pacific Islander        864

### CUNY ethnicity data in 1975

- From “Office of Institutional Research and Assessment 1975 - 1976 Data
  Book”
  - https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/
  - Table IV: Ethnic Composition Of Undergraduate Students By
    Matriculation Status, Sentor & Community Colleges, Expressed in
    Percentages, Fall 1969-1975 (p.127)
  - Only proportions were reported in the report. The actual number of
    students were interpolated from the total number of matriculated
    students (see Table IV: Trends In Undergraduate Matriculant &
    Non-Matriculant Enrollment Expressed in Numbers & Percent–Fall
    1970-1975 (p.115))
  - The labeling of the following ethnicity levels have been modified.
    - “Puerto Rican” to “Hispanic”
    - “Black” to “Black or African American”
    - “Other Spanish Surnamed Americans” to “Hispanic”
    - “American Indian” to “American Indian or Native Alaskan”
    - “Oriental” to “Asian or Pacific Islander”

``` r
tempData.1975 = read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity1975.csv", sep = ",")
tempData.1975$Enrollment = as.integer(tempData.1975$EnrollmentP * tempData.1975$EnrollmentT / 100)
tempData.1975 = tempData.1975[tempData.1975$Year %in% c(1973,1974,1975),c("Year","Control","Ethnicity","Enrollment")]

tempData.1975[tempData.1975=="Puerto Rican"] <- "Hispanic"
tempData.1975[tempData.1975=="Black"] <- "Black or African American"
tempData.1975[tempData.1975=="Other Spanish Surnamed Americans"] <- "Hispanic"
tempData.1975[tempData.1975=="American Indian"] <- "American Indian or Native Alaskan"
tempData.1975[tempData.1975=="Oriental"] <- "Asian or Pacific Islander"

head(tempData.1975)
```

    ##    Year Control                         Ethnicity Enrollment
    ## 29 1973  Senior                             White      72458
    ## 30 1973  Senior         Black or African American      19293
    ## 31 1973  Senior                          Hispanic       6216
    ## 32 1973  Senior                          Hispanic       1822
    ## 33 1973  Senior American Indian or Native Alaskan        428
    ## 34 1973  Senior         Asian or Pacific Islander       2679

### CUNY ethnicity data in 1977

- From “Office of Institutional Research and Assessment 1977 - 1978 Data
  Book”
  - https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/
  - Table VI: Ethnic Composition of Undergraduate Students by
    Matriculation Status, Senior and Community Colleges Expressed in
    Percentages, Fall 1969, 1970, 1972, 1974, 1976, 1978, 1980 (p.111)
  - Only proportions were reported in the report. The actual number of
    students were interpolated from the total number of matriculated
    students (see Table III: Trends In Undergraduate Enrollment by
    Matriculant and Non-Degree Status, Fall 1971, 1976 and 1977 (p.103))
  - The labeling of the following ethnicity levels have been modified.
    - “Black” to “Black or African American”
    - “American Indian” to “American Indian or Native Alaskan”
    - “Oriental” to “Asian or Pacific Islander”

``` r
tempData.1977 = read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity1977.csv", sep = ",")
tempData.1977$Enrollment = as.integer(tempData.1977$EnrollmentP * tempData.1977$EnrollmentT / 100)
tempData.1977 = tempData.1977[tempData.1977$Year %in% c(1976,1977),c("Year","Control","Ethnicity","Enrollment")]

tempData.1977[tempData.1977=="Black"] <- "Black or African American"
tempData.1977[tempData.1977=="American Indian"] <- "American Indian or Native Alaskan"
tempData.1977[tempData.1977=="Oriental"] <- "Asian or Pacific Islander"

head(tempData.1977)
```

    ##   Year Control                         Ethnicity Enrollment
    ## 1 1976  Senior                             White      51158
    ## 2 1976  Senior         Black or African American      23145
    ## 3 1976  Senior                          Hispanic      11939
    ## 4 1976  Senior American Indian or Native Alaskan        918
    ## 5 1976  Senior         Asian or Pacific Islander       4684
    ## 6 1976  Senior                             Other          0

## CUNY ethnicity data in 1980

- From “Office of Institutional Research and Assessment 1980 - 1981 Data
  Book”
  - https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/
  - Table IV: Ethnic Composition of Undergraduate Students by
    Matriculation Status, Senior and Community Colleges Expressed in
    Percentages, Fall 1969, 1970, 1972, 1974, 1976”, 1978, 1980 (p. 108)
  - Only proportions were reported in the report. The actual number of
    students were interpolated from the total number of matriculated
    students (see Table III: Trends In Undergraduate Enrollment by
    Matriculants and Non-Degree Status, Fall 1980, 1978, 1979, 1980
    (p.100))
  - The labeling of the following ethnicity levels have been modified.
    - “Black” to “Black or African American”
    - “American Indian” to “American Indian or Native Alaskan”
    - “Oriental” to “Asian or Pacific Islander”

``` r
tempData.1980 = read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity1980.csv", sep = ",")
tempData.1980$Enrollment = as.integer(tempData.1980$EnrollmentP * tempData.1980$EnrollmentT / 100)
tempData.1980 = tempData.1980[tempData.1980$Year %in% c(1978,1980),c("Year","Control","Ethnicity","Enrollment")]

tempData.1980[tempData.1980=="Black"] <- "Black or African American"
tempData.1980[tempData.1980=="American Indian"] <- "American Indian or Native Alaskan"
tempData.1980[tempData.1980=="Oriental"] <- "Asian or Pacific Islander"

head(tempData.1980)
```

    ##   Year Control                         Ethnicity Enrollment
    ## 1 1978  Senior                             White      54408
    ## 2 1978  Senior         Black or African American      27617
    ## 3 1978  Senior                          Hispanic      14895
    ## 4 1978  Senior American Indian or Native Alaskan       1137
    ## 5 1978  Senior         Asian or Pacific Islander       5378
    ## 6 1978  Senior                             Other          0

## CUNY ethnicity data in 1981

- From “Office of Institutional Research and Assessment 1981 - 1982 Data
  Book”
  - https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/
  - Table IV: Ethnic Composition of Undergraduate Students by
    Matriculation Status, Senior and Community Colleges: Expressed In
    Percentages, Fall 1969, 1970, 1972, 1974, 19768, 1978, 1980 and 1981
    (p.106)
    - Only proportions were reported in the report. The actual number of
      students were interpolated from the total number of matriculated
      students (see Table I: Trends in Enrollment Status By Sex Fall
      1978, 1979, 1980, 1981 (p.96))
  - The enrollment data for 1978 and 1979 differ from those previously
    reported in the Data Book since these years have been adjusted to
    include New York City Technical College and the College of Staten
    Island as senior colleges.
    - The labeling of the following ethnicity levels have been modified.
      - “Black” to “Black or African American”
      - “American Indian” to “American Indian or Native Alaskan”
      - “Oriental” to “Asian or Pacific Islander”

``` r
tempData.1981 = read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity1981.csv", sep = ",")
tempData.1981$Enrollment = as.integer(tempData.1981$EnrollmentP * tempData.1981$EnrollmentT / 100)
tempData.1981 = tempData.1981[tempData.1981$Year %in% c(1981),c("Year","Control","Ethnicity","Enrollment")]

tempData.1981[tempData.1981=="Black"] <- "Black or African American"
tempData.1981[tempData.1981=="American Indian"] <- "American Indian or Native Alaskan"
tempData.1981[tempData.1981=="Oriental"] <- "Asian or Pacific Islander"

head(tempData.1981)
```

    ##    Year Control                         Ethnicity Enrollment
    ## 13 1981  Senior                             White      51684
    ## 14 1981  Senior         Black or African American      23891
    ## 15 1981  Senior                          Hispanic      14822
    ## 16 1981  Senior American Indian or Native Alaskan        975
    ## 17 1981  Senior         Asian or Pacific Islander       6143
    ## 18 1981  Senior                             Other          0

## CUNY ethnicity data in 1992

- From “CUNY Student Data Book Fall 1992”
  - https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/
  - Table 34. Trends in Enrollment by Race/Ethnicity of CUNY
    Undergraduates: 1976 to 1992 (p.121)
  - Only proportions were reported in the report. The actual number of
    students were interpolated from the total number of matriculated
    students (see Table 10B. Trends in Undergraduate Enrolment by
    Colege: 1980 to 1992 (p.52))

``` r
tempData.1992 = read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity1992.csv", sep = ",")
tempData.1992$Enrollment = as.integer(tempData.1992$EnrollmentP * tempData.1992$EnrollmentT / 100)
tempData.1992 = tempData.1992[tempData.1992$Year %in% c(1982,1984,1986,1988,1989),c("Year","Control","Ethnicity","Enrollment")]

tempData.1992[tempData.1992=="Black"] <- "Black or African American"

head(tempData.1992)
```

    ##    Year   Control                         Ethnicity Enrollment
    ## 31 1982    Senior                             White      54999
    ## 32 1982    Senior         Black or African American      27499
    ## 33 1982    Senior                          Hispanic      16435
    ## 34 1982    Senior         Asian or Pacific Islander       7304
    ## 35 1982    Senior American Indian or Native Alaskan       1181
    ## 36 1982 Community                             White      16019

### CUNY ethnicity data between 1990 and 2022

- Data from CUNY’s Office of Institutional Research
  - <https://www.cuny.edu/about/administration/offices/oira/institutional/data/student-data-book-archive/>
  - <https://public.tableau.com/app/profile/oira.cuny/viz/StudentDataBook/Enrollment>
  - Tableau
    - Select year in “Select Year”
    - “Enrollment Drilldown”
    - For “Bottom Column Header”, choose “Degree Pursued Level”
    - For “Select Right Row Header”, choose “Ethnicity”
    - For filters, choose “Undergraduate” in “Class Level”
  - “Black” in the ethnicity category has been modified to “Black or
    African American”

``` r
tempData.2022 <- read.csv("data/CUNY_OIRA2023StudentDataBookEnrollmentByDegreeAndEthnicity.csv", sep = ",")
tempData.2022[tempData.2022=="Black"] <- "Black or African American"

head(tempData.2022)
```

    ##   Year   Control                         Ethnicity Enrollment
    ## 1 1990 Community American Indian or Native Alaskan        139
    ## 2 1990 Community         Asian or Pacific Islander       5018
    ## 3 1990 Community         Black or African American      20620
    ## 4 1990 Community                          Hispanic      17198
    ## 5 1990 Community                             White      17702
    ## 6 1990    Senior American Indian or Native Alaskan        275

### Plotting the data

- “Other” and “No Response” were removed

``` r
# creating data tables
tempData = bind_rows(tempData.1967, tempData.1969, tempData.1972, tempData.1975, tempData.1977, tempData.1980, tempData.1981, tempData.1992, tempData.2022)

xtabs(Enrollment ~ Ethnicity + Year + Control, data=tempData)[,,"Community"]
```

    ##                                    Year
    ## Ethnicity                            1967  1968  1969  1971  1972  1973  1974  1975  1976  1977  1978  1980
    ##   American Indian or Native Alaskan     0     0     0   144   220   179   198   137  1051   607   562  1326
    ##   Asian or Pacific Islander             0     0     0   864   883  1075  1322  1103  1793  2023  1499   353
    ##   Black or African American          2303  4885  6040 13972 17108 20195 20227 20767 20715 23945 17201 16756
    ##   Hispanic                            948  1379  1555  4129  6180  8484  8790  9658 11068 11938  8905 10699
    ##   No Response                         259   420  1244     0     0     0     0     0     0     0     0     0
    ##   Other                               813   677   907   144  1103  2031  2644  3173     0     0     0     0
    ##   White                             12906 16011 16176 28761 29691 27783 32919 34152 27208 28936 18701 15032
    ##                                    Year
    ## Ethnicity                            1981  1982  1984  1986  1988  1989  1990  1991  1992  1993  1994  1995
    ##   American Indian or Native Alaskan   492   721   908   867   116   122   139   123   112   105   141   140
    ##   Asian or Pacific Islander          1478  2060  2896  3180  4193  4580  5018  5631  5789  6174  7111  7249
    ##   Black or African American         16843 19367 21527 20930 20620 22049 20620 21253 22109 23119 31089 29233
    ##   Hispanic                          11064 13341 15393 16420 15960 16735 17198 17646 18206 19625 23628 23534
    ##   No Response                           0     0     0     0     0     0     0     0     0     0     0     0
    ##   Other                                 0     0     0     0     0     0     0     0     0     0     0     0
    ##   White                             14917 16019 16074 16420 17299 17529 17702 17846 17499 17992 19290 17986
    ##                                    Year
    ## Ethnicity                            1996  1997  1998  1999  2000  2001  2002  2003  2004  2005  2006  2007
    ##   American Indian or Native Alaskan   114   128   128   147   143   131   131   104   119   140   150   168
    ##   Asian or Pacific Islander          7461  7590  8104  8399  8865  9094  9496  9779 10054 10380 11265 12194
    ##   Black or African American         29201 27914 26951 26004 26067 26197 28185 28133 29431 29057 28789 28137
    ##   Hispanic                          24694 23820 23215 22087 22180 22157 23523 23671 24705 25351 26193 27814
    ##   No Response                           0     0     0     0     0     0     0     0     0     0     0     0
    ##   Other                                 0     0     0     0     0     0     0     0     0     0     0     0
    ##   White                             17391 16208 16248 15537 15469 15503 16973 17201 17719 17627 17769 18015
    ##                                    Year
    ## Ethnicity                            2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019
    ##   American Indian or Native Alaskan   194   313   363   317   329   326   372   401   412   399   411   385
    ##   Asian or Pacific Islander         13239 14405 14790 15357 15501 15588 16058 15922 15861 15530 15262 14688
    ##   Black or African American         29366 31978 32120 33522 32185 32137 33007 32251 31017 30002 29445 28632
    ##   Hispanic                          30048 33671 34927 37001 37126 38699 39569 39241 38707 38318 36516 33653
    ##   No Response                           0     0     0     0     0     0     0     0     0     0     0     0
    ##   Other                                 0     0     0     0     0     0     0     0     0     0     0     0
    ##   White                             17713 18910 18268 18793 17707 16973 16775 15567 14839 14324 13956 12983
    ##                                    Year
    ## Ethnicity                            2020  2021  2022
    ##   American Indian or Native Alaskan   373   333   278
    ##   Asian or Pacific Islander         13570 11840 10358
    ##   Black or African American         25790 23089 21162
    ##   Hispanic                          29708 24793 21606
    ##   No Response                           0     0     0
    ##   Other                                 0     0     0
    ##   White                             11520  9726  7995

``` r
xtabs(Enrollment ~ Ethnicity + Year + Control, data=tempData)[,,"Senior"]
```

    ##                                    Year
    ## Ethnicity                            1967  1968  1969  1971  1972  1973  1974  1975  1976  1977  1978  1980
    ##   American Indian or Native Alaskan     0     0     0   181   296   428   455   355   918   964  1137   888
    ##   Asian or Pacific Islander             0     0     0  1900  2374  2679  3419  3905  4684  4209  5378  5430
    ##   Black or African American          2285  4712  6179 13216 15929 19293 24736 30413 23145 19820 27617 27549
    ##   Hispanic                            975  1338  1872  4164  6727  8038 10486 11123 11939 10962 14895 15700
    ##   No Response                         468   872  1934     0     0     0     0     0     0     0     0     0
    ##   Other                              2268  1396  2371   452  2869  4287  6725  6863     0     0     0     0
    ##   White                             56560 49797 50059 70607 70742 72458 68167 65678 51158 51744 54408 49174
    ##                                    Year
    ## Ethnicity                            1981  1982  1984  1986  1988  1989  1990  1991  1992  1993  1994  1995
    ##   American Indian or Native Alaskan   975  1181  1135  1424   208   216   275   231   224   234   160   154
    ##   Asian or Pacific Islander          6143  7304  7434  9870 12222 12568 12947 13281 13794 14341 12589 12473
    ##   Black or African American         23891 27499 27671 28491 30086 31529 35438 34894 36143 36607 25718 25009
    ##   Hispanic                          14822 16435 18172 17908 18281 19610 20701 20833 21791 23082 19854 19698
    ##   No Response                           0     0     0     0     0     0     0     0     0     0     0     0
    ##   Other                                 0     0     0     0     0     0     0     0     0     0     0     0
    ##   White                             51684 54999 48837 44059 43562 44422 44127 42525 41114 40050 29930 29142
    ##                                    Year
    ## Ethnicity                            1996  1997  1998  1999  2000  2001  2002  2003  2004  2005  2006  2007
    ##   American Indian or Native Alaskan   130   135   142   133   135   128   154   144   157   158   158   153
    ##   Asian or Pacific Islander         12242 11905 11827 11860 12113 12807 13291 13984 14749 15580 16770 18200
    ##   Black or African American         24859 24946 24185 23937 23135 23100 23255 23153 23628 23997 24443 25016
    ##   Hispanic                          19440 19486 18816 18513 18340 18367 18781 19865 20934 21395 22394 23631
    ##   No Response                           0     0     0     0     0     0     0     0     0     0     0     0
    ##   Other                                 0     0     0     0     0     0     0     0     0     0     0     0
    ##   White                             28498 28391 27494 27130 26581 27142 28335 29647 30052 30522 31128 32053
    ##                                    Year
    ## Ethnicity                            2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019
    ##   American Indian or Native Alaskan   184   186   220   233   239   284   306   335   338   356   364   375
    ##   Asian or Pacific Islander         19976 21370 22856 24769 26075 26961 28113 29460 30266 31155 32158 32497
    ##   Black or African American         25242 25414 24678 25315 25034 24825 25444 26475 26608 26843 27164 27961
    ##   Hispanic                          25456 26472 26664 27154 27685 28083 29514 30973 32271 34078 35471 35699
    ##   No Response                           0     0     0     0     0     0     0     0     0     0     0     0
    ##   Other                                 0     0     0     0     0     0     0     0     0     0     0     0
    ##   White                             33324 34898 35651 37157 36825 35167 34631 32697 31156 30791 30222 29816
    ##                                    Year
    ## Ethnicity                            2020  2021  2022
    ##   American Indian or Native Alaskan   371   394   363
    ##   Asian or Pacific Islander         32913 31944 30764
    ##   Black or African American         27878 26826 24833
    ##   Hispanic                          35359 33819 31074
    ##   No Response                           0     0     0
    ##   Other                                 0     0     0
    ##   White                             30280 28690 25786

``` r
xtabs(Enrollment ~ Year + Control, data=tempData)
```

    ##       Control
    ## Year   Community Senior
    ##   1967     17229  62556
    ##   1968     23372  58115
    ##   1969     25922  62415
    ##   1971     48014  90520
    ##   1972     55185  98937
    ##   1973     59747 107183
    ##   1974     66100 113988
    ##   1975     68990 118337
    ##   1976     61835  91844
    ##   1977     67449  87699
    ##   1978     46868 103435
    ##   1980     44166  98741
    ##   1981     44794  97515
    ##   1982     51508 107418
    ##   1984     56798 103249
    ##   1986     57817 101752
    ##   1988     58188 104359
    ##   1989     61015 108345
    ##   1990     60677 113488
    ##   1991     62499 111764
    ##   1992     63715 113066
    ##   1993     67015 114314
    ##   1994     81259  88251
    ##   1995     78142  86476
    ##   1996     78861  85169
    ##   1997     75660  84863
    ##   1998     74646  82464
    ##   1999     72174  81573
    ##   2000     72724  80304
    ##   2001     73082  81544
    ##   2002     78308  83816
    ##   2003     78888  86793
    ##   2004     82028  89520
    ##   2005     82555  91652
    ##   2006     84166  94893
    ##   2007     86328  99053
    ##   2008     90560 104182
    ##   2009     99277 108340
    ##   2010    100468 110069
    ##   2011    104990 114628
    ##   2012    102848 115858
    ##   2013    103723 115320
    ##   2014    105781 118008
    ##   2015    103382 119940
    ##   2016    100836 120639
    ##   2017     98573 123223
    ##   2018     95590 125379
    ##   2019     90341 126348
    ##   2020     80961 126801
    ##   2021     69781 121673
    ##   2022     61399 112820

### Plotting the data for community colleges

- “Other” and “No Response” were removed

``` r
tempData = tempData[tempData$Ethnicity %notin% c("Other","No Response"),]
```

- Plotting diversity data at CUNY community colleges

``` r
CUNY.diversity = xtabs(Enrollment ~ Ethnicity + Year + Control, data=tempData)
CUNY.diveristy.2y = as.data.frame(CUNY.diversity[,,"Community"])
colnames(CUNY.diveristy.2y) <- c("Ethnicity","Year","Enrollment")

# plotting data
p <- ggplot(CUNY.diveristy.2y, aes(Year, Enrollment, group = Ethnicity)) + 
    geom_point(aes(color=Ethnicity), size=2.5) + 
  scale_shape_manual(values = c(0,1)) +
#  scale_color_brewer(palette = "Spectral") +
  geom_line(aes(color=Ethnicity)) + 
    geom_text(aes(label=paste(addComma(Enrollment),sep="")), position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, hjust=-0.01, size=2.75, data=CUNY.diveristy.2y[as.integer(CUNY.diveristy.2y$Year) %% 3 == 1,])

# using ggplot theme and modifying axis labels
p + theme_hc() + scale_fill_identity() + # scale_colour_hc() +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
  ggtitle(paste("Ethnicity of Matriculated Undergradutae Enrollments at CUNY Community Colleges between 1967 and 2022\n(Data from CUNY's Office of Institutional Research)")) +
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=10)))
```

<img src="RProcedureMLA2024CUNYMLEnrollmentAndDiversity_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

``` r
#  theme(legend.key.size = unit(1, 'cm'), #change legend key size
#  legend.key.height = unit(1, 'cm'), #change legend key height
#  legend.key.width = unit(1, 'cm'), #change legend key width
#  legend.title = element_text(size=12), #change legend title font size
#  legend.text = element_text(size=8)) #change legend text font size
ggsave("data/CUNYMLEnrollmentAndDiversityPlotEthnicity2Y1967_2022.pdf", width = 12, height = 8)
```

- Simplified plot for community colleges

``` r
tempData[tempData$Ethnicity %in% c("American Indian or Native Alaskan", "Asian or Pacific Islander", "Black or African American", "Hispanic"),"Ethnicity"] <- "Minority groups"
CUNY.diversity.simplified = xtabs(Enrollment ~ Ethnicity + Year + Control, data=tempData)
CUNY.diveristy.simplified.2y = as.data.frame(CUNY.diversity.simplified[,,"Community"])
colnames(CUNY.diveristy.simplified.2y) <- c("Ethnicity","Year","Enrollment")

# plotting data
p <- ggplot(CUNY.diveristy.simplified.2y, aes(Year, Enrollment, group = Ethnicity)) + 
    geom_point(aes(color=Ethnicity), size=2.5) + 
  scale_shape_manual(values = c(0,1)) +
#  scale_color_brewer(palette = "Spectral") +
  geom_line(aes(color=Ethnicity)) + 
    geom_text(aes(label=paste(addComma(Enrollment),sep="")), position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.75, data=CUNY.diveristy.simplified.2y[as.integer(CUNY.diveristy.simplified.2y$Year) %% 3 == 1,])

# using ggplot theme and modifying axis labels
p + theme_hc() + scale_fill_identity() + # scale_colour_hc() +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
  ggtitle(paste("Ethnicity of Matriculated Undergradutae Enrollments at CUNY Community Colleges between 1967 and 2022\n(Data from CUNY's Office of Institutional Research)")) +
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=10)))
```

<img src="RProcedureMLA2024CUNYMLEnrollmentAndDiversity_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

``` r
#  theme(legend.key.size = unit(1, 'cm'), #change legend key size
#  legend.key.height = unit(1, 'cm'), #change legend key height
#  legend.key.width = unit(1, 'cm'), #change legend key width
#  legend.title = element_text(size=12), #change legend title font size
#  legend.text = element_text(size=8)) #change legend text font size
ggsave("data/CUNYMLEnrollmentAndDiversityPlotEthnicity2Y1967_2022Simplified.pdf", width = 12, height = 8)
```

### Plotting the data for senior colleges

- Plotting diversity data at CUNY community colleges

``` r
# plotting data
CUNY.diveristy.4y = as.data.frame(CUNY.diversity[,,"Senior"])
colnames(CUNY.diveristy.4y) <- c("Ethnicity","Year","Enrollment")
p <- ggplot(CUNY.diveristy.4y, aes(Year, Enrollment, group = Ethnicity)) + 
    geom_point(aes(color=Ethnicity), size=2.5) + 
  scale_shape_manual(values = c(0,1)) +
#  scale_color_brewer(palette = "Spectral") +
  geom_line(aes(color=Ethnicity)) + 
    geom_text(aes(label=paste(addComma(Enrollment),sep="")), position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, hjust=-0.01, size=2.75, data=CUNY.diveristy.4y[as.integer(CUNY.diveristy.4y$Year) %% 3 == 1,])

# using ggplot theme and modifying axis labels
p + theme_hc() + scale_fill_identity() + # scale_colour_hc() +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
  ggtitle(paste("Ethnicity of Matriculated Undergradutae Enrollments at CUNY Senior Colleges between 1967 and 2022\n(Data from CUNY's Office of Institutional Research)")) +
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=10)))
```

<img src="RProcedureMLA2024CUNYMLEnrollmentAndDiversity_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

``` r
#  theme(legend.key.size = unit(1, 'cm'), #change legend key size
#  legend.key.height = unit(1, 'cm'), #change legend key height
#  legend.key.width = unit(1, 'cm'), #change legend key width
#  legend.title = element_text(size=12), #change legend title font size
#  legend.text = element_text(size=8)) #change legend text font size
ggsave("data/CUNYMLEnrollmentAndDiversityPlotEthnicity4Y1967_2022.pdf", width = 12, height = 8)
```

## Analyzing the MLA’s Enrollment Survey

- MLA’s Language Enrollment Database, 1958-2021
  (<http://apps.mla.org/flsurvey_search>)
- The data file “Historical-language-enrollments-1958-2021.xlsx” was
  obtained from the MLA’s website at
  <http://apps.mla.org/flsurvey_search>.
- Some campuses are listed twice (e.g., CityTech) because of the changes
  of the institutional name that took place between 1965 and 2021.

``` r
# importing data (Download the original data from https://apps.mla.org/flsurvey_search. Change xlsx to csv.)
thisData <- read.csv("data/Historical-language-enrollments-1958-2021.csv", sep = ",")
# filling empty "UNIV_NAME_HISTORY"
thisData[thisData$UNIV_NAME_HISTORY=="",c("UNIV_NAME_HISTORY")] <- thisData[thisData$UNIV_NAME_HISTORY=="",c("UNIV")]
thisData$SRVY_YEAR <- as.factor(thisData$SRVY_YEAR)
thisData$TERM <- as.factor(thisData$TERM)
thisData$YR.TERM <- as.factor(thisData$YR.TERM)
thisData$UNIV <- as.factor(thisData$UNIV)
thisData$UNIV_NAME_HISTORY <- as.factor(thisData$UNIV_NAME_HISTORY)
thisData$CAMPUS <- as.factor(thisData$CAMPUS)
thisData$NCES_ID <- as.factor(thisData$NCES_ID)
thisData$STATE <- as.factor(thisData$STATE)
thisData$STATE_ID <- as.factor(thisData$STATE_ID)
thisData$MLA.ICLEVEL <- as.factor(thisData$MLA.ICLEVEL)
# NOTE: From the 2021 data, the ICLEVEL is a three-level factor: 4Y, 2Y, and 2Y with some 4Y degrees
levels(thisData$MLA.ICLEVEL) = c("4 year","2 year","2 year")
thisData$LANG_CODE <- as.factor(thisData$LANG_CODE)
thisData$CITY <- as.factor(thisData$CITY)
thisData$LANGUAGE <- as.factor(thisData$LANGUAGE)
thisData$LANG_REGION <- as.factor(thisData$LANG_REGION)
thisData$OTHER_LANG <- as.factor(thisData$OTHER_LANG)
thisData$GEOGRAPHY_CODE <- as.factor(thisData$GEOGRAPHY_CODE)
thisData$N_RESP <- as.factor(thisData$N_RESP)
thisData$ZERO_ERL <- as.factor(thisData$ZERO_ERL)

# Between 1963 - 1972 many institutions did not report "UNDERGRAD_TOTAL" and "GRAD_TOTAL". We need to use "ALL_LEVEL_TOTAL" instead
# Note: From the 2021 data, UNDERGRAD_TOTAL is renamed  to UG.TOTAL (similar changes happened in other column names)
thisData[is.na(thisData$UG.TOTAL),c("UG.TOTAL")] <- thisData[is.na(thisData$UG.TOTAL),c("ALL.LEVEL.TOTAL")]

# Retrieving all data (for some reason, there is no enrollment data for some community colleges in 1972)
MLA <- drop.levels(thisData[thisData$YR.TERM %notin% c("1958 Fall", "1959 Fall", "1961 Fall", "1963 Fall", "1969 Summer", "1970 Fall", "1971 Summer", "1972 Fall", "2016 Summer", "2020 Fall"),],reorder=FALSE)

# selecting only CUNY
#MLA.CUNY <- drop.levels(MLA[grep("CUNY",MLA$UNIV),],reorder=FALSE)
MLA.CUNY <- drop.levels(MLA[MLA$UNIV %in% c("BARUCH C, CUNY","BOROUGH OF MANHATTAN COMM C, CUNY","BRONX COMM C, CUNY", "BROOKLYN C, CUNY", "C OF STATEN ISLAND, CUNY", "CITY C OF NEW YORK, CUNY", "HOSTOS COMM C, CUNY", "HUNTER C, CUNY", "JOHN JAY C OF CRIMINAL JUSTICE, CUNY", "KINGSBOROUGH COMM C, CUNY", "LAGUARDIA COMM C, CUNY", "LEHMAN C, CUNY", "MEDGAR EVERS C, CUNY", "NEW YORK CITY C OF TECH, CUNY", "QUEENS C, CUNY", "QUEENSBOROUGH COMM C, CUNY","YORK C, CUNY"),],reorder=FALSE)

levels(MLA.CUNY$UNIV) <- c("Baruch","BMCC", "BCC", "Brooklyn","CSI","City","Hostos C","Hunter","John Jay","Kingsborough C","LaGuardia C","Lehman","Medger Evers","CityTech","Queens","Queensborough C","York")

MLA.CUNY.e <- tapply(MLA.CUNY$UG.TOTAL, list(MLA.CUNY$UNIV, MLA.CUNY$YR.TERM, MLA.CUNY$LANGUAGE),sum, na.rm=TRUE)
MLA.CUNY.e3 <- tapply(MLA.CUNY$UG.TOTAL, list(MLA.CUNY$UNIV, MLA.CUNY$YR.TERM),sum, na.rm=TRUE)
```

- Plotting the modern language enrollment data at CUNY

``` r
# selecting community colleges and senior colleges
CUNYCCIndex = c("BMCC", "Hostos C", "BCC", "Kingsborough C", "LaGuardia C", "Queensborough C")
CUNYSCIndex = c("Baruch","Brooklyn","CSI", "City", "Hunter", "John Jay", "Lehman", "Medger Evers", "CityTech", "Queens", "York")
```

- Plotting the modern language enrollment data

``` r
tempData = melt(MLA.CUNY.e3)
colnames(tempData) <- c("Institution","Year","Enrollment")
tempData[tempData$Institution %in% CUNYCCIndex,"Control"] = "Community College"
tempData[tempData$Institution %in% CUNYSCIndex,"Control"] = "Senior College"

tempData[is.na(tempData$Enrollment),"Enrollment"] <- 0

tempData <- melt(tapply(tempData$Enrollment, list(tempData$Year,tempData$Control),sum, na.rm=TRUE))
colnames(tempData) <- c("Year","Control","Enrollment")

p <- ggplot(tempData, aes(Year, Enrollment, group = Control)) + 
  geom_point(aes(color=Control), size=2.5) + 
#  scale_shape_manual(values = c(0,1)) +
#  scale_color_brewer(palette = "PuOr") +
  geom_line(aes(color=Control)) + 
    geom_text(aes(label=paste(addComma(Enrollment),sep="")), 
        position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.75)

  # using ggplot theme and modifying axis labels
p + theme_hc() + scale_fill_identity() + # + scale_colour_hc() +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
    ggtitle(paste("Modern Language Enrollments at CUNY by Control between 1958 and 2021\n(Data from MLA's Language Enrollment Database, 1958–2021)")) +
    theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm")) +
    theme(axis.title.x = element_text(margin=margin(t=10)))
```

<img src="RProcedureMLA2024CUNYMLEnrollmentAndDiversity_files/figure-gfm/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

``` r
#  theme(legend.key.size = unit(1, 'cm'), #change legend key size
#    legend.key.height = unit(1, 'cm'), #change legend key height
#    legend.key.width = unit(1, 'cm'), #change legend key width
#    legend.title = element_text(size=12), #change legend title font size
#    legend.text = element_text(size=8)) #change legend text font size
ggsave(paste("data/CUNYMLEnrollmentMLAEnrollmentSurvey2023CCAndSC.pdf",sep=""), width = 11, height = 7)
```

``` r
# plotting data
tempData = melt(MLA.CUNY.e3[CUNYCCIndex,])
colnames(tempData) <- c("Institution","Year","Enrollment")
tempData[is.na(tempData$Enrollment),"Enrollment"] <- 0
p <- ggplot(tempData, aes(Year, Enrollment, group = Institution)) + 
  geom_point(aes(color=Institution), size=2.5) + 
#  scale_shape_manual(values = c(0,1)) +
#  scale_color_brewer(palette = "PuOr") +
  geom_line(aes(color=Institution)) + 
    geom_text(aes(label=paste(addComma(Enrollment),sep="")), 
        position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.75)

  # using ggplot theme and modifying axis labels
p + theme_hc() + scale_fill_identity() + # + scale_colour_hc() +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
    ggtitle(paste("Modern Language Enrollments at CUNY Commmunity Colleges bet. 1958 and 2021\n(Data from MLA's Language Enrollment Database, 1958–2021)")) +
    theme(plot.margin = unit(c(2,2,2,2), "cm")) +
    theme(axis.title.x = element_text(margin=margin(t=10)))
```

<img src="RProcedureMLA2024CUNYMLEnrollmentAndDiversity_files/figure-gfm/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

``` r
#  theme(legend.key.size = unit(1, 'cm'), #change legend key size
#    legend.key.height = unit(1, 'cm'), #change legend key height
#    legend.key.width = unit(1, 'cm'), #change legend key width
#    legend.title = element_text(size=12), #change legend title font size
#    legend.text = element_text(size=8)) #change legend text font size
ggsave(paste("data/CUNYMLEnrollmentMLAEnrollmentSurvey2023PlotCCAllLanguages.pdf",sep=""), width = 12, height = 9)
```
