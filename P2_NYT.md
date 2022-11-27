P2\_New York Times
================
Jue Zhou
11/21/2019

## 

``` r
# define a function to create the url and use NYT Archive API to return the array of articles of a certain month
get_archive <- function(year, month){
  nyt_key <- getOption("nyt_key")
  apiurl <- str_c("https://api.nytimes.com/svc/archive/v1/",
                year,
                "/",
                month,
                ".",
                "json?",
                "api-key=",
                nyt_key)
  archive <- apiurl %>% 
    httr::GET() %>% 
    httr::content() %>% 
    as_tibble() 
}
```

``` r
# fetch the data for 2019.11
archive_2019 <- get_archive(2019,11)
archive_2019 <- archive_2019 %>% 
    unnest_longer(response) %>% 
    unnest_wider(response) %>% 
    select(snippet,keywords, pub_date, news_desk, section_name, word_count)
# unnest the list of keywords for each article
archive_2019clear <- archive_2019[-c(1), ] %>% 
    unnest_longer(keywords) %>% 
    unnest_wider(keywords) %>% 
    select(-c(4:6)) %>% 
    mutate(pub_date = str_sub(pub_date, 1, 10))
  
## deduplicate the data and transform to one line for each article 
article_2019 <- archive_2019clear %>% 
  distinct(snippet, .keep_all = TRUE) %>% 
  select(snippet,pub_date, news_desk, section_name, word_count)
```

``` r
# fetch the data for 1919.11
archive2 <- get_archive(1919,11)
archive_1919 <- archive2 %>% 
    unnest_longer(response) %>% 
    unnest_wider(response) %>% 
    select(snippet, keywords, pub_date, type_of_material, word_count)
archive_1919
```

    ## # A tibble: 10,185 x 5
    ##    snippet                 keywords  pub_date   type_of_material word_count
    ##    <chr>                   <list>    <chr>      <chr>                 <int>
    ##  1 <NA>                    <???>     <NA>       <NA>                     NA
    ##  2 On the intelligent ind… <list [1… 1919-11-0… Editorial               295
    ##  3 estimate of persons in… <list [2… 1919-11-0… Article                 119
    ##  4 Isaacs, Hy, slain foll… <list [3… 1919-11-0… Article                  99
    ##  5 Denmark places 16 vess… <list [4… 1919-11-0… Article                 189
    ##  6 Considerable activity … <list [1… 1919-11-0… Article                 400
    ##  7 WASHINGTON, Oct. 31.--… <list [4… 1919-11-0… Front Page              859
    ##  8 vs Jackson              <list [7… 1919-11-0… Article                 151
    ##  9 PRINCETON, N.J., Oct. … <list [6… 1919-11-0… Article                 224
    ## 10 Fined by French for di… <list [4… 1919-11-0… Article                 125
    ## # … with 10,175 more rows

``` r
archive_1919clear <- archive_1919[-c(1), ] %>% 
    unnest_longer(keywords) %>% 
    unnest_wider(keywords) %>% 
    select(snippet, name, value, pub_date, type_of_material, word_count) %>% 
    mutate(pub_date = str_sub(pub_date, 1, 10))
archive_1919clear
```

    ## # A tibble: 27,279 x 6
    ##    snippet             name   value    pub_date type_of_material word_count
    ##    <chr>               <chr>  <chr>    <chr>    <chr>                 <int>
    ##  1 On the intelligent… perso… MCCOOK,… 1919-11… Editorial               295
    ##  2 estimate of person… perso… ROOSEVE… 1919-11… Article                 119
    ##  3 estimate of person… subje… MEMORIA… 1919-11… Article                 119
    ##  4 Isaacs, Hy, slain … perso… ISAACS,… 1919-11… Article                  99
    ##  5 Isaacs, Hy, slain … gloca… NEW YOR… 1919-11… Article                  99
    ##  6 Isaacs, Hy, slain … subje… MURDERS  1919-11… Article                  99
    ##  7 Denmark places 16 … gloca… DENMARK  1919-11… Article                 189
    ##  8 Denmark places 16 … subje… COAL     1919-11… Article                 189
    ##  9 Denmark places 16 … subje… COAL     1919-11… Article                 189
    ## 10 Denmark places 16 … subje… SHIPPING 1919-11… Article                 189
    ## # … with 27,269 more rows

``` r
## deduplicate the data and transform to one line for each article 
article_1919 <- archive_1919clear %>% 
  distinct(snippet, .keep_all = TRUE) %>% 
  select(snippet,pub_date, type_of_material, word_count)
```

## 

``` r
article_2019 %>%
  group_by(section_name) %>%
  summarise(word_count = mean(word_count), number = n()) %>% 
  mutate(percent = number/sum(number)*100) %>% 
  rename("Section Name"="section_name",
         "Word Count"="word_count",
         "Number" = "number",
         "Percent(%)" = "percent") %>% 
  head() %>% 
  kable(caption = "Distribution of Sections, New York Times (2019.11)")
```

| Section Name | Word Count | Number | Percent(%) |
| :----------- | ---------: | -----: | ---------: |
| Admin        |    30.7500 |      4 |  0.1130582 |
| Arts         |   876.1648 |    273 |  7.7162239 |
| Books        |   930.2905 |    148 |  4.1831543 |
| Briefing     |  1135.8966 |     29 |  0.8196721 |
| Business Day |  1112.6637 |    226 |  6.3877897 |
| Climate      |   924.5385 |     26 |  0.7348785 |

Distribution of Sections, New York Times (2019.11)

``` r
article_1919 %>%
  group_by(type_of_material) %>%
  summarise(word_count = mean(word_count), number = n())
```

    ## # A tibble: 6 x 3
    ##   type_of_material      word_count number
    ##   <chr>                      <dbl>  <int>
    ## 1 Article                     503.   4074
    ## 2 Editorial                   610.    188
    ## 3 Front Page                 1073.    349
    ## 4 Letter                      724.     60
    ## 5 Marriage Announcement       453       3
    ## 6 Obituary                    344       2

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
