porfolio 2
================

### 1. Load Dataset and Overview of data

\#for this porfolio, I aim to look at some Covid data.

``` r
COVID19 <- read.csv("~/Desktop/R lab/keyu-porfolio-2/COVID19_line_list_data.csv")
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
```

\#the dataset is imported from Kaggle:
<https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset/version/25>

``` r
glimpse(COVID19)
```

    ## Rows: 1,085
    ## Columns: 27
    ## $ id                    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
    ## $ case_in_country       <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ reporting.date        <chr> "1/20/2020", "1/20/2020", "1/21/2020", "1/21/202…
    ## $ X                     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ summary               <chr> "First confirmed imported COVID-19 pneumonia pat…
    ## $ location              <chr> "Shenzhen, Guangdong", "Shanghai", "Zhejiang", "…
    ## $ country               <chr> "China", "China", "China", "China", "China", "Ch…
    ## $ gender                <chr> "male", "female", "male", "female", "male", "fem…
    ## $ age                   <dbl> 66, 56, 46, 60, 58, 44, 34, 37, 39, 56, 18, 32, …
    ## $ symptom_onset         <chr> "01/03/20", "1/15/2020", "01/04/20", NA, NA, "1/…
    ## $ If_onset_approximated <int> 0, 0, 0, NA, NA, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, …
    ## $ hosp_visit_date       <chr> "01/11/20", "1/15/2020", "1/17/2020", "1/19/2020…
    ## $ exposure_start        <chr> "12/29/2019", NA, NA, NA, NA, NA, NA, "01/10/20"…
    ## $ exposure_end          <chr> "01/04/20", "01/12/20", "01/03/20", NA, NA, NA, …
    ## $ visiting.Wuhan        <int> 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, …
    ## $ from.Wuhan            <int> 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, …
    ## $ death                 <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"…
    ## $ recovered             <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"…
    ## $ symptom               <chr> "", "", "", "", "", "", "", "", "", "", "", "", …
    ## $ source                <chr> "Shenzhen Municipal Health Commission", "Officia…
    ## $ link                  <chr> "http://wjw.sz.gov.cn/wzx/202001/t20200120_18987…
    ## $ X.1                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ X.2                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ X.3                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ X.4                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ X.5                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    ## $ X.6                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

### 2. Are most cases reported from China？

\#due to the large population of China, and the fact that it experienced
the first outbreak of covid, I wonder if most cases reported were from
China.

``` r
COVID19$death_clean <- as.integer(COVID19$death != 0)
```

\#to do so, I filtered cases in China and cases not in China.

``` r
china_cases <- COVID19 %>% 
  filter(country == "China")
```

``` r
other_cases <- COVID19 %>% 
  filter(country != "China")
```

“as shown in the data, there are only 197 cases reported from China
whereas there are 888 cases reported outside of China.”

\#in order to present this research question visiaully, I used ggplot.

``` r
covid_china <- COVID19 %>% 
  mutate(country_china = if_else (country == "China", "China", "Other"))
```

``` r
covid_china %>%
  ggplot(covid_china,
         mapping = aes(x= country_china)) +
  geom_bar() +
  labs (
  x = "country_china",
  y = "count",
  title = "whether most covid cases are reported from China"
  )
```

![](porfolio-2_files/figure-gfm/china_plot-1.png)<!-- --> “as shown in
the graph, the majority of cases were not from China.”

### 3. Are most cases reported in China originally from Wuhan？

``` r
china_cases %>%
  ggplot(china_cases,
         mapping = aes(x= country,
                       fill = from.Wuhan)) +
  geom_bar() +
  labs (
  x = "cases in China",
  y = "count",
  title = "whether most covid cases in China are from Wuhan"
  )+
  facet_wrap(~from.Wuhan)
```

![](porfolio-2_files/figure-gfm/wuhan_plot-1.png)<!-- --> “as shown in
the diagram, covid cases not from wuhan (”0“) were greater than cases
from wuhan (”1“), suggesting that most covid cases in China were not
from Wuhan.”

\#\#\#4. What are the top 3 countries with most death cases?

``` r
COVID19 %>% 
  filter(death_clean > 0) %>% 
  group_by(country) %>%
  summarise(death_total = sum(death_clean)) %>%
  ggplot(aes(country)) + 
  geom_point(aes(y = death_total, color = "red")) +
coord_flip()
```

![](porfolio-2_files/figure-gfm/death_cases-1.png)<!-- --> “China seemed
to have the largest death total, followed by South Korea and Japan.”

\#\#\#5. What is the death rate in China?

``` r
sum(china_cases$death_clean) / 197 #number of cases in china
```

    ## [1] 0.1979695

“the death rate in China is about 20%.”

\#\#\#6. Which population is more likely to die in China?

``` r
dead = subset(china_cases, death_clean == 1)
alive = subset(china_cases, death_clean == 0)
mean(dead$age, na.rm = TRUE) #71.05
```

    ## [1] 71.05128

``` r
mean(alive$age, na.rm = TRUE) #43.30
```

    ## [1] 43.30464

“the mean age for people who died from covid is 71.1 whereas the mean
age for people who survived from covid is 43.3.”

``` r
t.test(dead$age, alive$age, alternative = "two.sided")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  dead$age and alive$age
    ## t = 12.257, df = 74.006, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  23.23621 32.25708
    ## sample estimates:
    ## mean of x mean of y 
    ##  71.05128  43.30464

“the p value is basically 0, indicating that the mean age between people
who died from covid and people who survived from covid is significantly
different. Hence, elder population is more likely to die during covid in
China than younger people”

\#\#\#7. Are there sex differences in the death rate of covid in China?

``` r
men = subset(china_cases, gender == "male")
women = subset(china_cases, gender == "female")
mean(men$death_clean, na.rm = TRUE) #21.7%
```

    ## [1] 0.2177419

``` r
mean(women$death_clean, na.rm = TRUE) #16.4%
```

    ## [1] 0.1643836

“The mean death rate for men (21.7%) is larger than the mean death rate
for women (16.4%). I used ggplot below to visually present this
information”

``` r
china_cases %>%
  filter(death_clean > 0) %>%
  ggplot(china_cases,
         mapping = aes(x= death_clean,
                       fill = gender)) +
  geom_bar() +
  labs (
  x = "death cases in China",
  y = "count",
  title = "whether there are sex differences in deaths rate"
  )+
  facet_wrap(~gender)
```

![](porfolio-2_files/figure-gfm/gender_plot-1.png)<!-- -->

``` r
t.test(men$death_clean, women$death_clean, alternative = "two.sided")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  men$death_clean and women$death_clean
    ## t = 0.92989, df = 163.91, p-value = 0.3538
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.05994323  0.16665997
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.2177419 0.1643836

“the p value is 0.35, suggesting that there is no significant
differences of death rates between men and women in China.”
