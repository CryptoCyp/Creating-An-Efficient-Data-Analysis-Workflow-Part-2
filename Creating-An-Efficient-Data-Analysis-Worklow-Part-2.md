Creating An Efficient Data Analysis Workflow Part 2
================
Eriz Tolay
9/12/2021

## Creating An Efficient Data Analysis Workflow Part 2

In this project, we are taking on the role of as an analyst for a book
company again. The company has provided us more data on some of its 2019
book sales, and it wants us to extract some usable knowledge from it. It
launched a new program encouraging customers to buy more books on July
1st, 2019, and it wants to know **if this new program was successful at
increasing sales and improving review quality.**

``` r
sales <- read_csv('sales2019.csv')
```

    ## Rows: 5000 Columns: 5

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): date, user_submitted_review, title, customer_type
    ## dbl (1): total_purchased

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
for (col in colnames(sales)) {
  paste0(col, 
         ", number of missing data rows: ", 
         is.na(sales[[col]]) %>% sum) %>% print
}
```

    ## [1] "date, number of missing data rows: 0"
    ## [1] "user_submitted_review, number of missing data rows: 885"
    ## [1] "title, number of missing data rows: 0"
    ## [1] "total_purchased, number of missing data rows: 718"
    ## [1] "customer_type, number of missing data rows: 0"

Looking at the data, we can see that user\_submitted\_review and
total\_purchased columns have missing some missing data.

We’re going to to remove any rows that have missing data in
**user\_submitted\_review.**

For total\_purchased, we’re going to use a slightly more sophisticated
approach. We are going to replace all of the NA values with an average
value that we calculate from the complete dataset.

``` r
new_sales <- sales %>%
  filter(!(is.na(user_submitted_review)))

dim(new_sales)
```

    ## [1] 4115    5

885 rows with missing data are now removed from our data.

Now, we will calculate the mean of the total\_purchased column, without
the missing values.

``` r
average_purchased <- new_sales %>%
  filter(!(is.na(total_purchased))) %>%
  pull(total_purchased) %>%
  mean() %>% print
```

    ## [1] 3.985561

Filling all of the missing values in **total\_purchased** with the
average value we calculated above.

``` r
new_sales <- new_sales %>%
  mutate(
    total_purchased =
      if_else(is.na(total_purchased), average_purchased, total_purchased)
  )
```

The **user\_submitted\_review** column contains reviews in the form of
sentences. Ultimately, we want to be able to classify reviews as either
positive or negative. This will allow us to count the number of negative
or positive reviews in the analysis part of the workflow. On this
screen, we’ll perform the cleaning and processing necessary to turn each
of the review sentences into the classifications we want.

``` r
new_sales %>% pull(user_submitted_review) %>% unique
```

    ## [1] "it was okay"                         
    ## [2] "Awesome!"                            
    ## [3] "Hated it"                            
    ## [4] "Never read a better book"            
    ## [5] "OK"                                  
    ## [6] "The author's other books were better"
    ## [7] "A lot of material was not needed"    
    ## [8] "Would not recommend"                 
    ## [9] "I learned a lot"

``` r
# Creates a function that takes in a sentence and returns a value indicating if the review is positive or not.

is_positive <- function(review) {
  review_positive = case_when(
  str_detect(review, "Awesome") ~ TRUE,
  str_detect(review, "OK") ~ TRUE,
  str_detect(review, "Never") ~ TRUE,
  str_detect(review, "a lot") ~ TRUE,
  TRUE ~ FALSE # The review did not contain any of the above phrases
  )
}
```

``` r
new_sales <- new_sales %>% 
  mutate(
    positive_reviews = unlist(map(user_submitted_review, is_positive))
  )
```

With the review data and order quantities processed into a usable form,
we can finally make a move towards answering the main question of the
analysis, **Was the new book program effective in increasing book
sales?** The program started on July 1st, 2019, and the data we have
contains all of the sales for 2019. There are still some preparatory
steps we need to take before performing the analysis, so we’ll do these
first before conducting the analysis.

First, the dates are currently represented in string form. These must be
properly formatted before we can make any comparisons based on date and
time.

Second, we need a clear way to distinguish between sales that happen
before the program starts and those that happen after. We need to
distinguish between these two groups so that we can use what we’ve
learned to more easily calculate the summary values we want from the
data.

And finally, this analysis should be put into a neat form.

``` r
# Formatting the dates
new_sales <- new_sales %>%
  mutate(
    date = mdy(date)
  )
```

``` r
# Create a new grouping column to help distinguish between sales that happen before July 1, 2019 and sales that happen after this date.

new_sales <- new_sales %>%
  mutate(
    beforejul2019 = if_else(date < "2019-07-01", TRUE, FALSE)
  )
```

``` r
new_sales_summary <-  new_sales %>%
  group_by(beforejul2019) %>%
  summarise(
    sum_purchases = sum(total_purchased)
  ) %>% print()
```

    ## # A tibble: 2 × 2
    ##   beforejul2019 sum_purchases
    ##   <lgl>                 <dbl>
    ## 1 FALSE                 8190.
    ## 2 TRUE                  8211.

As it’s clear from table above, it seems like the new program was not
really effective in increasing book sales.

``` r
new_sales_summary <-  new_sales %>%
  group_by(beforejul2019, title) %>%
  summarise(
    sum_purchases = sum(total_purchased)
  ) %>% print()
```

    ## `summarise()` has grouped output by 'beforejul2019'. You can override using the `.groups` argument.

    ## # A tibble: 12 × 3
    ## # Groups:   beforejul2019 [2]
    ##    beforejul2019 title                              sum_purchases
    ##    <lgl>         <chr>                                      <dbl>
    ##  1 FALSE         Fundamentals of R For Beginners            2832.
    ##  2 FALSE         R For Dummies                              2779.
    ##  3 FALSE         R Made Easy                                  24 
    ##  4 FALSE         R vs Python: An Essay                      1172.
    ##  5 FALSE         Secrets Of R For Advanced Students         1154.
    ##  6 FALSE         Top 10 Mistakes R Beginners Make            228.
    ##  7 TRUE          Fundamentals of R For Beginners            3093.
    ##  8 TRUE          R For Dummies                              2626.
    ##  9 TRUE          R Made Easy                                  15 
    ## 10 TRUE          R vs Python: An Essay                      1271.
    ## 11 TRUE          Secrets Of R For Advanced Students          965.
    ## 12 TRUE          Top 10 Mistakes R Beginners Make            241.

It turns out that certain books actually got more popular after the
program started! R For Dummies and Secrets of R For Advanced Students
got more popular.

``` r
new_sales_summary <-  new_sales %>%
  group_by(beforejul2019, customer_type) %>%
  summarise(
    sum_purchases = sum(total_purchased)
  ) %>% print()
```

    ## `summarise()` has grouped output by 'beforejul2019'. You can override using the `.groups` argument.

    ## # A tibble: 4 × 3
    ## # Groups:   beforejul2019 [2]
    ##   beforejul2019 customer_type sum_purchases
    ##   <lgl>         <chr>                 <dbl>
    ## 1 FALSE         Business              5742.
    ## 2 FALSE         Individual            2448.
    ## 3 TRUE          Business              5612.
    ## 4 TRUE          Individual            2599.

Here, we can see that Business customers purchased slightly more books
after the program was introduced. There is still no improvement for
individual customers.

The last question that we need to answer is, **Did review scores improve
as a result of the program?**

``` r
new_sales_summary <-  new_sales %>%
  group_by(beforejul2019) %>%
  summarise(
    sum_purchases = sum(total_purchased),
    positive_reviews = sum(positive_reviews)
  ) %>% print()
```

    ## # A tibble: 2 × 3
    ##   beforejul2019 sum_purchases positive_reviews
    ##   <lgl>                 <dbl>            <int>
    ## 1 FALSE                 8190.              903
    ## 2 TRUE                  8211.              900

There’s slightly more reviews before the program, but this difference
seems negligible.
