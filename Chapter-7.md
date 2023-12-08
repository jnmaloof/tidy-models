---
title: "Chapter 7"
author: "Julin Maloof"
date: "2023-12-08"
output: 
  html_document: 
    keep_md: yes
---




```r
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.1.1 ──
```

```
## ✔ broom        1.0.5     ✔ recipes      1.0.8
## ✔ dials        1.2.0     ✔ rsample      1.2.0
## ✔ dplyr        1.1.4     ✔ tibble       3.2.1
## ✔ ggplot2      3.4.4     ✔ tidyr        1.3.0
## ✔ infer        1.0.5     ✔ tune         1.1.2
## ✔ modeldata    1.2.0     ✔ workflows    1.1.3
## ✔ parsnip      1.1.1     ✔ workflowsets 1.0.1
## ✔ purrr        1.0.2     ✔ yardstick    1.2.0
```

```
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ purrr::discard() masks scales::discard()
## ✖ dplyr::filter()  masks stats::filter()
## ✖ dplyr::lag()     masks stats::lag()
## ✖ recipes::step()  masks stats::step()
## • Search for functions across packages at https://www.tidymodels.org/find/
```

```r
library(multilevelmod)
library(workflowsets)
tidymodels_prefer()
load("ames_test_train.Rdata")
```

# Chapter 7: A Model Workflow

include pre and post-processing steps in addition to the model fit in a workflow.

## 7.2 Workflow Basics

Create the model

```r
lm_model <- linear_reg() %>%
  set_engine("lm")
```

Now create a workflow

```r
lm_wflow <- workflow() %>%
  add_model(lm_model)

lm_wflow
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: None
## Model: linear_reg()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear Regression Model Specification (regression)
## 
## Computational engine: lm
```

What about a pre-processor?

If very simple, can just be a formula:


```r
lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Formula
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Sale_Price ~ Longitude + Latitude
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear Regression Model Specification (regression)
## 
## Computational engine: lm
```

Workslows have a fit method that can be used to create the model


```r
lm_fit <- fit(lm_wflow, ames_train)
lm_fit
```

```
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Formula
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Sale_Price ~ Longitude + Latitude
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## 
## Call:
## stats::lm(formula = ..y ~ ., data = data)
## 
## Coefficients:
## (Intercept)    Longitude     Latitude  
##    -309.471       -2.064        2.889
```

Can predict from this:

```r
predict(lm_fit, ames_test %>% slice(1:3))
```

```
## # A tibble: 3 × 1
##   .pred
##   <dbl>
## 1  5.21
## 2  5.27
## 3  5.28
```

can update model or pre-processor

```r
lm_fit %>% update_formula(Sale_Price ~ Longitude)
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Formula
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Sale_Price ~ Longitude
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear Regression Model Specification (regression)
## 
## Computational engine: lm
```
Would need to add data and refit

## 7.3 Adding Raw Variables to the Workflow

This is an alternative to using a formula


```r
lm_wflow <- lm_wflow %>%
  remove_formula() %>%
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))
lm_wflow
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: Sale_Price
## Predictors: c(Longitude, Latitude)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear Regression Model Specification (regression)
## 
## Computational engine: lm
```


```r
fit(lm_wflow, ames_train)
```

```
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: Sale_Price
## Predictors: c(Longitude, Latitude)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## 
## Call:
## stats::lm(formula = ..y ~ ., data = data)
## 
## Coefficients:
## (Intercept)    Longitude     Latitude  
##    -309.471       -2.064        2.889
```
## 7.4 How does a `workflow()` use the formula?

This is complicated because different packages do different transformations of the formula...

workflow attempts to do whatever the underlying model would do.

I am a little confused about whether or not workflow will create dummy variables for us when a package expects the user to do so (e.g. xgboost).  Maybe this will be come clear as we continue.

### 7.4.1 Special formulas and inline functions

Here we specify the formula when we specify the model...

Seems a little strange to me that we need to add variables and also have a formula later.


```r
multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_workflow <- 
  workflow() %>% 
  # Pass the data along as-is: 
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevel_spec, 
            # This formula is given to the model
            formula = distance ~ Sex + (age | Subject))

multilevel_fit <- fit(multilevel_workflow, data = nlme::Orthodont)
multilevel_fit
```

```
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: distance
## Predictors: c(Sex, age, Subject)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear mixed model fit by REML ['lmerMod']
## Formula: distance ~ Sex + (age | Subject)
##    Data: data
## REML criterion at convergence: 471.1635
## Random effects:
##  Groups   Name        Std.Dev. Corr 
##  Subject  (Intercept) 7.3912        
##           age         0.6943   -0.97
##  Residual             1.3100        
## Number of obs: 108, groups:  Subject, 27
## Fixed Effects:
## (Intercept)    SexFemale  
##      24.517       -2.145
```

Also works when you have functions in your formula:

```r
library(censored)
```

```
## Loading required package: survival
```

```r
parametric_spec <- survival_reg()

parametric_workflow <- 
  workflow() %>% 
  add_variables(outcome = c(fustat, futime), predictors = c(age, rx)) %>% 
  add_model(parametric_spec, 
            formula = Surv(futime, fustat) ~ age + strata(rx))

parametric_fit <- fit(parametric_workflow, data = ovarian)
parametric_fit
```

```
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: survival_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: c(fustat, futime)
## Predictors: c(age, rx)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Call:
## survival::survreg(formula = Surv(futime, fustat) ~ age + strata(rx), 
##     data = data, model = TRUE)
## 
## Coefficients:
## (Intercept)         age 
##  12.8734120  -0.1033569 
## 
## Scale:
##      rx=1      rx=2 
## 0.7695509 0.4703602 
## 
## Loglik(model)= -89.4   Loglik(intercept only)= -97.1
## 	Chisq= 15.36 on 1 degrees of freedom, p= 8.88e-05 
## n= 26
```

## 7.5 Creating Multiple Workflows at once

Use a workflow set if you want to try a bunch of different formulas, etc.

Example: multiple formulas for Ames data set Sales as a function of location


```r
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)
```

Now create the workflow set

```r
location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models
```

```
## # A workflow set/tibble: 4 × 4
##   wflow_id        info             option    result    
##   <chr>           <list>           <list>    <list>    
## 1 longitude_lm    <tibble [1 × 4]> <opts[0]> <list [0]>
## 2 latitude_lm     <tibble [1 × 4]> <opts[0]> <list [0]>
## 3 coords_lm       <tibble [1 × 4]> <opts[0]> <list [0]>
## 4 neighborhood_lm <tibble [1 × 4]> <opts[0]> <list [0]>
```



```r
location_models$info[[1]]
```

```
## # A tibble: 1 × 4
##   workflow   preproc model      comment
##   <list>     <chr>   <chr>      <chr>  
## 1 <workflow> formula linear_reg ""
```

```r
extract_workflow(location_models, id = "coords_lm")
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Formula
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Sale_Price ~ Longitude + Latitude
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear Regression Model Specification (regression)
## 
## Computational engine: lm
```
fit each one.  A little ugly...(although apparently we will see something better in Chapter 11)

```r
location_models <-
   location_models %>%
   mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
```

```
## # A workflow set/tibble: 4 × 5
##   wflow_id        info             option    result     fit       
##   <chr>           <list>           <list>    <list>     <list>    
## 1 longitude_lm    <tibble [1 × 4]> <opts[0]> <list [0]> <workflow>
## 2 latitude_lm     <tibble [1 × 4]> <opts[0]> <list [0]> <workflow>
## 3 coords_lm       <tibble [1 × 4]> <opts[0]> <list [0]> <workflow>
## 4 neighborhood_lm <tibble [1 × 4]> <opts[0]> <list [0]> <workflow>
```


```r
location_models$fit[[1]]
```

```
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Formula
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Sale_Price ~ Longitude
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## 
## Call:
## stats::lm(formula = ..y ~ ., data = data)
## 
## Coefficients:
## (Intercept)    Longitude  
##    -181.831       -1.997
```

## 7.6 Evaluating the test set...


```r
final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res
```

```
## # Resampling results
## # Manual resampling 
## # A tibble: 1 × 6
##   splits             id               .metrics .notes   .predictions .workflow 
##   <list>             <chr>            <list>   <list>   <list>       <list>    
## 1 <split [2344/586]> train/test split <tibble> <tibble> <tibble>     <workflow>
```


```r
fitted_lm_wflow <- extract_workflow(final_lm_res)

collect_metrics(final_lm_res)
```

```
## # A tibble: 2 × 4
##   .metric .estimator .estimate .config             
##   <chr>   <chr>          <dbl> <chr>               
## 1 rmse    standard       0.162 Preprocessor1_Model1
## 2 rsq     standard       0.163 Preprocessor1_Model1
```

```r
collect_predictions(final_lm_res) %>% slice(1:5)
```

```
## # A tibble: 5 × 5
##   id               .pred  .row Sale_Price .config             
##   <chr>            <dbl> <int>      <dbl> <chr>               
## 1 train/test split  5.21     4       5.39 Preprocessor1_Model1
## 2 train/test split  5.27    12       5.27 Preprocessor1_Model1
## 3 train/test split  5.28    13       5.26 Preprocessor1_Model1
## 4 train/test split  5.27    15       5.33 Preprocessor1_Model1
## 5 train/test split  5.24    19       5.15 Preprocessor1_Model1
```

