--- 
title: "Portfolio Stat 764"
author: "Gabriel Hintz"
date: "2024-05-09"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib]
# url: your book url like https://bookdown.org/yihui/bookdown
# cover-image: path to the social sharing image like images/cover.jpg
description: |
  Portfolio STAT764 Spring 2024
biblio-style: apalike
csl: chicago-fullnote-bibliography.csl
---


<!--chapter:end:index.Rmd-->

# Journals

## Day 1

In the class lectured on January 18th I learned about the quality of the department of statistics of KSU and how it can impact our professional lives in the future. Having an own department of statistics provide us a whole opportunity to go deeper and develop our statistical skills. Also, the main book recommended by the professor seems to be great to support us throughout the semester. Lastly, something I hadn’t realized before, basically all data can be considered as spatio-temporal due to the fact it is collected in a certain time and location.

As it was the introductory class, there is nothing I struggled to understand.

## Day 2

In the class lectured on January 25th we mostly talked about an example of spatio-temporal analysis to a very simple dataset collected by the professor. We tested three different models: a) a 10th degree polynomial model; b) a generalized additive model using a non-linear, and smooth relationship employing a Gaussian process; and c) a decision tree. Besides that, there were some take away messages: 

- The size of the dataset is not a limiting factor to employ statistical analysis. 

- In research, we are unable to collect data without any error, which imply in adding another source of variability in our analysis: the error. What happens is that sometimes we don’t care about the dimension of the error, but it will always be there. 

- The distribution of the error is crucial when doing predictions if you want to look at the interval or distribution of the most likely prediction value. 

- Overfitting x underfitting is a complex discussion and needs to be carefully interpreted.

Something that I kind of struggled a little bit and need to study more is about distributions and how to write out the model properly.

## Day 3

Questions to be covered in class:

• How to know if a model is good/reliable?

• How to improve our model?

• What is overfitting?

Implementation of a process model to your truth location not to your recorded location, and that is when the hierarchical statistical methods come up.

In space time statistics it is almost impossible to replicate things. However, in the marathon example, one could run using a second/third GPS devices or run the marathon again and then create replications.

Data + assumptions = predictions/inferences/forecasts. The assumptions are mandatory!

mob function in r: instead of flat lines as in regression tree, there is an actual statistical model for each ‘line’.

In the marathon example we fit a quadratic model.

Spatio-temporal has adopted Bayesian hierarchical models due to the fact the traditional methods are not as good as expected. Linear model does pretty good to estimate but not as good to measure uncertainty.

The more complex model is usually the best prediction wise but not as good for inference (as it happened for speed in the marathon example). That means that sometimes depending on the question that needs to be answered, one can overcomplicate things.

## Day 4 

Be careful when removing outliers or making modifications on the data.

Try to avoid to apply statistics on statistics, rather model the original raw data instead.

Collect information to help on building the model assumptions/changing them.

Prediction vs forecast. Prediction is restricted to the range of the data you have. Forescast you can go further and extrapolate over the range of your data. Hindcast follows the same rules as forecast but towards the past instead of future.

Certain methods are very good for prediction, such as machine learning methods. When one try to move it to forecasts or hindcasts usually they become not as good because they are probably missing assumptions.

Alternative is to save some data to test and validate different models which will probably result in less complex models.

Things we are interested on: predictions, forecast, hindcast and statistical inferences. The last one can be observable or unobservable (a slope, for example).

Reliably quantify and communicate uncertainty.

Adjustment of assumptions can help to narrow prediction intervals.

When we talk about mathematical equations I can picture the real mean of it if I understand every piece of the formula. Looking at the normal distribution equation kind of scares at the first moment and does not tell me much how to apply or how to interpret it. I need to go deeper on it.

## Day 5

The variance (second moment) can also be modeled.

Expected value and mean are actually different things.

Different distributions have different moments.

Least squares only provide estimates of the parameters, not able to generate a p value or generate fake data.

It is simply a mathematical model.

Moments of a distribution: expected value and variance. If you calculate the mean it would be the estimate of the expected value. However, even though mean and expected can be the same value, it is important to highlight that conceptually they are different things.

We might treat time as either continuous(differential equations) or discrete (difference equations), depending on the situation.

I don’t think I fully understood the concepts of difference equations and differential equations. I did understood the first one is treating time as discrete values and the second one as continuous values. However, the formula and its application was not 100% clear.

## Day 6

Spatio-temporal data: any data that was collected on a given time and location. Almost all data is spatiotemporal, there is something happening in a given time and space.

Hierarchical models examples: mixed models, kriging, most Bayesian models.

Hierarchical models can be divided into: Empirical model and Bayesian model. The difference is that the second one includes the parameter model, whereas the first one uses the maximum likelihood.

Framework Bayesian hierarchical model: Data model, process model and parameter model (this one is not present in the empirical framework). y is the data we wish we had. Also known as priors.

It assumes all data is collected with error, and there is a probability distribution for this error. It may be very small or huge.

Outputs: Posterior distribution of the parameters Posterior predictive distribution z is the column matrix with the observed data. y would be the true value for the variable measured in z or the priors.

To be honest, I don’t think there is any doubt about this class, everything was clear. I will just need to study more about Bayesian.

## Day 7

Whooping cranes is a good example for both prediction and inference.

If the goal is prediction, probably machine learning will be the best choice. 

If the goal is inference, probably regression and anova can pretty much meet the goals. If we want both we need to find something in between.

I) clarify what you want, what is your goal.
II) write out a model you think is suitable.
III) google and look for packages/papers have already used the same methodology.

The real meaning of the p on the data model was unclear to me. As I am not familiar with Bayesian
I struggled a little bit to understand all the hierarchical sequence of the model but I think at the end I
understood, and as we move forward it will make more sense for me.
I could not take notes about the ${\theta_D}$ in the model description.

## Day 8

In the whooping crane example p =${\theta_D}$, which means the probability distribution.


‘rejection’ of parameters: if the simulated ‘fake’ data fits well to the model we keep it, if it doesn’t we reject
it.

The result will be a posterior prediction distribution for y

Not happy with the posterior distribution: either collect more data or adjust your assumptions.

I did not understand the real meaning of the blue line when you showed the posterior distribution of the parameters. Was that the mean of the percentages? Not clear for me what that means.

I meant to stay longer for office hours but I had an appointment. You mentioned in class that it is very hard (almost impossible?!) to measure uncertainty without using Bayesian. However, what if we use a traditional
approach (let’s say a linear model) and then bootstrap to calculate confidence intervals? Are not those intervals an uncertainty measurement?

Regarding the final project, joining someone from the Stat department is mandatory or just a recommendation?

## Day 9

Example: extreme precipitation in Kansas

Goal: mapping rainfall and predict rainfall. Inference on where was the highest precipitation amount.

Careful when deleting missing data, they might be information and change the meaning of your analysis.

Currently, sf package in R is the most reliable one for spatial analysis and to manage shapefiles.

When we talked about methodologies to deal with missing data I could not follow since I am not aware of methodologies to deal with missing data in the analysis.

## Day 10

Case Deletion missing data methodology assumption: NAs are not related to the response variable.

Grabbing the nearest weather station is basically KNN approach, which is a machine learning technique.

Think about how check and validate the model and/or model selection.

It was not clear for me the wrap up of the class about Gaussian processes, as well as the multivariate normal distribution notation.

## Day 11

Gaussian process is one of the most useful distribution in stat.

There are distributions for spatial points or even contours.

Gaussian assumes multivariate normal distribution.

Correlation matrix - correlation between all covariates.

Multivariate Normal Distribution you do not need to deal with dependence.

Random block effect assumption will be just like the compound symmetry matrix.

AR(1) dependence of observations in a given time. Basically a correlation matrix for time-series data.

ACF: dependence of observed neighbors

Correlation function: mathematical function that describes how correlated are random variables.

In the correlation function d describes the distance whereas the ϕ describes the ‘growth rate’.

Very good class, however when you showed both the eta calculated from the correlation matrix and the AR(1) lines I got confused about how they differenciate themselves.

## Day 12

The model above was applied to the extreme precipitation example in the analysis 2 of the script, more specifically the model using the gls function.

In spatial stats we kind of switch from replication to smoothness.

I struggled today to understand the gls model and the nugget term.

## Day 13

## Day 14

Before starting the workshop we discussed about alternatives to explore the elevation activity, such as kriging, regression tree and gradient boosting machine learning methods.

## Day 15

Read chapter 6 and technical note 1.1 on pag 13 - Wikle et al. (2019).

Memorization of some values of the data is important before analyzing.

There is a problem on it, the assumption is saying the precipitation can be negative and infinite when it assumes a normal distribution.

Classic kriging doesn’t work for large datasets.

Statistical analysis 3 - kriging with two error terms using low-rank approximation (aka modern kriging for big data).

The model 3 the same as the 2 but using low-rank approximation.

## Day 16

Jacob and I were discussing about the Tweedie distribution today to fit a model to explain precipitation.

We struggled to fit bayesian model using the tweedie distribution. Do you know if it is possible to do it in brms package? or just using Stan or Jags?

## Day 17

Chapter 6 proposes a way to test the ‘best’ distribution.

MAE is a proper metric for a normal distribution data.

Some cases calibration is more important the prediction accuracy.

If the prediction interval is not covering ~95% of the data points it is a sign the assumptions of the model have problems.

## Day 18

Checking calibration and model performance is crucial for a good fitting.

We struggled to understand the Tweedie distribution to model precipitation in our project. Especially on the coding part.

## Day 19

Tutorial

Bird Cherry-oat Aphid dataset description

Activity 3 we will have to do similar spatial prediction to the class example and use the land cover info for it. Check code.

Poisson is a good distribution for counting.

## Day 20

Regarding the class project, Jacob and I discussed about the models we will fit to our data and the distributions we will use. Our reference model will be a KNN model and we will try to fit a hierarchical model using both zero-inflated and hurdle distributions to model rainfall.

We discussed about the basis function you used in the last class and we were not sure why you used an exponential function. Also, I tried to understand the models that were fitted in the Enders example, but I am not sure if I fully understood all of them.

## Day 21

Activity 3

Goal: accurately predict (map) the abundance and probability of BYDV infection at times and locations where data was not collected.

Here we will make the assumption that the collected data is the true data.

The exponential in the process model is the link function.

In the example, ηs is the only one with a distribution assumption behind it since we are smothing it through the s function. In this case we are assuming a multivariate normal distribution.

In the example we are interested on the second coefficient that portraits the increase of aphids as the grassland percentage increases.

The second grassland % in the zero inflated poisson basically say as far as it is negative that as you increase the abundance it is more likely to be out of zero.

The use of AIC allows you to test the best model without sacrifizing any portion of your data for model testing.

Concurvity is an advanced approach of multicollinearity.

If the random effect is penalizing your model according to the concurvity you can either ackowledge the limitations of the model or remove the random effect.

## Day 22

## Day 23

AIC: model checking when you don’t want to sacrifize any data for testing.

Concurvity is very similar to colinearity. Measure how the covariate relates to the spatial covariate or any other random effect.

Variogram: check dependence of the the residuals. Ideally there will be now shape on the distribution of the points, following a linear flat line.

## Day 24

Jacob and I worked on the project. We discussed the final adjustments of the analysis and talked about the main goals for the writing.

## Day 25

Earthquake example

Location and time are the random variables.

y = z 

Assumptiom: measured data is pretty close to the real data.

## Day 26

Jacob and I worked on the class project. We discussed about calibration and uncertainty with Aidan. We were struggling to have a good calibration to our data but then we realized that with our data set it would be very hard to achieve a reasonable calibration. Regarding the uncertainty, we talked to Aidan and then we decided to average daily uncertainty for each location of our predictions.

## Day 27

Uncertainty in forecasts using GAM functions are hugely wide.

The partial differential equations were quite challenging.

## Day 28

Jacob and way worked on last adjustments of the final project before sending to the peer review.


<!--chapter:end:01-Journals.Rmd-->

# Activity 1




```r

url <- 'https://www.dropbox.com/scl/fi/2mufv5tlloz06k5ncwyx8/Afternoon_Walk.gpx?rlkey=6jzq31fonrs95glnfibi7lscp&dl=1'

st_layers(url)
data <- st_read(url, layer = "track_points")
```

## Plot/map your movement data. I would recommend using R and/or Google earth as I demonstrated in class.


```r
coords <- as.data.frame(st_coordinates(data))
coords$time <- data$time

leaflet(coords) %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  addCircleMarkers(~X, ~Y, color = ~time, radius = 2, fillOpacity = .8, group = "Data Points") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "World Imagery"),
    overlayGroups = c("Data Points"),
    options = layersControlOptions(collapsed = FALSE)
  )
```


```{=html}
<div class="leaflet html-widget html-fill-item" id="htmlwidget-90f5dd032d678b4f4bc9" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-90f5dd032d678b4f4bc9">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,"OSM (default)",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addProviderTiles","args":["Esri.WorldImagery",null,"World Imagery",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircleMarkers","args":[[39.193263,39.193272,39.193282,39.193282,39.193267,39.193254,39.193329,39.193345,39.193323,39.193265,39.193258,39.193281,39.1933,39.193316,39.19334,39.19335,39.193351,39.193373,39.193386,39.193433,39.193364,39.193296,39.193228,39.193246,39.193355,39.193343,39.193338,39.193341,39.19335,39.19337,39.193378,39.193386,39.193402,39.193414,39.193422,39.193429,39.193436,39.193447,39.193462,39.193479,39.193492,39.193512,39.193523,39.193534,39.193559,39.193567,39.193583,39.193598,39.193626,39.193636,39.193664,39.193676,39.193697,39.193705,39.193713,39.193728,39.19374,39.193764,39.193771,39.193778,39.193788,39.193801,39.193812,39.19382,39.193835,39.193845,39.193859,39.193872,39.193884,39.19389,39.1939,39.193923,39.193932,39.19394,39.193961,39.19397,39.193979,39.193993,39.194012,39.194024,39.194047,39.194055,39.194074,39.194086,39.194107,39.194115,39.194136,39.194153,39.194163,39.194173,39.194194,39.194203,39.194227,39.194234,39.194253,39.194262,39.194281,39.194291,39.194313,39.194317,39.194321,39.194328,39.19432,39.194328,39.194319,39.194319,39.194321,39.194319,39.194317,39.194317,39.194316,39.194316,39.19432,39.194319,39.194316,39.194316,39.194316,39.194317,39.194319,39.194318,39.194318,39.194319,39.194326,39.194326,39.194326,39.19433,39.194339,39.194349,39.194351,39.194353,39.194357,39.194358,39.19436,39.194367,39.194369,39.194373,39.194371,39.194378,39.194379,39.194383,39.194384,39.194384,39.194388,39.19439,39.194392,39.194394,39.194396,39.194397,39.194399,39.194406,39.194415,39.194419,39.194416,39.194413,39.194412,39.194405,39.19438,39.194353,39.194344,39.194319,39.194312,39.194301,39.194302,39.194303,39.194308,39.194312,39.194316,39.19432,39.194325,39.194333,39.194355,39.194364,39.194372,39.194388,39.194403,39.194406,39.194397,39.194389,39.194389,39.194386,39.194385,39.194388,39.19439,39.194387,39.194382,39.194376,39.194373,39.194373,39.194386,39.194395,39.194391,39.194391,39.194391,39.19439,39.194388,39.194388,39.194388,39.194388,39.194388,39.194388,39.194388,39.194388,39.194376,39.194399,39.194393,39.194408,39.194404,39.194371,39.194375,39.19439,39.194387,39.194388,39.194386,39.194381,39.194386,39.194395,39.194394,39.194396,39.194397,39.194394,39.194393,39.194384,39.194369,39.194361,39.194334,39.194286,39.194227,39.194158,39.194081,39.194001,39.193923,39.193848,39.193776,39.193709,39.193636,39.193566,39.193499,39.193432,39.193367,39.193302,39.19323,39.193147,39.193074,39.193022,39.192983,39.192952,39.192955,39.192958,39.192971,39.192974,39.192979,39.192982,39.192988,39.192996,39.193007,39.193013,39.193016,39.19302,39.193021,39.193029,39.193036,39.193052,39.193069,39.19309,39.193114,39.193138,39.193167,39.193196,39.193223,39.193249,39.193276,39.193309,39.19335,39.193386,39.193427,39.193473,39.193513,39.193555,39.193595,39.193641,39.193688,39.193734,39.193778,39.193825,39.193872,39.193916,39.193959,39.194007,39.194055,39.194101,39.194148,39.194193,39.19423,39.194268,39.194294,39.194313,39.19433,39.194345,39.194348,39.194358,39.194368,39.194376,39.194388,39.194397,39.194406,39.194419,39.194423,39.194436,39.19444,39.194447,39.194452,39.19445,39.19445,39.19445,39.194449,39.19445,39.194453,39.194457,39.194477,39.194486,39.194508,39.194534,39.194565,39.194599,39.194633,39.194672,39.194704,39.194742,39.194782,39.19482,39.194856,39.194891,39.194925,39.194957,39.194988,39.195016,39.195042,39.195066,39.195086,39.195109,39.195125,39.195143,39.195155,39.195163,39.195168,39.195175,39.19518,39.195184,39.195187,39.195192,39.195191,39.195188,39.195183,39.195181,39.195177,39.195171,39.195164,39.195158,39.195155,39.195155,39.195153,39.195152,39.195152,39.195149,39.195148,39.195146,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.195142,39.19514,39.195138,39.19513,39.19511,39.195059,39.194981,39.194887,39.194771,39.194646,39.194525,39.194399,39.194271,39.194146,39.194024,39.193902,39.193782,39.193667,39.193554,39.193438,39.193341,39.193227,39.193116,39.193016,39.192897,39.192777,39.192664,39.192554,39.192446,39.192338,39.192233,39.192129,39.192013,39.191901,39.191786,39.191668,39.191554,39.191452,39.191352,39.191254,39.191154,39.191055,39.190953,39.190854,39.190754,39.190655,39.190564,39.190467,39.190371,39.190287,39.190204,39.190036,39.189947,39.189861,39.189768,39.189674,39.189581,39.189491,39.1894,39.18931,39.189209,39.189105,39.189007,39.188907,39.188808,39.188718,39.188623,39.188532,39.188443,39.188362,39.188289,39.188217,39.188146,39.188073,39.187993,39.187916,39.187844,39.187792,39.187753,39.18772,39.187713,39.187706,39.187695,39.187682,39.187664,39.187654,39.187643,39.187605,39.187553,39.187477,39.187407,39.187328,39.187246,39.187152,39.187062,39.186975,39.186902,39.186842,39.186793,39.186765,39.186755,39.186758,39.18676,39.186765,39.186763,39.186757,39.186744,39.186736,39.186737,39.186734,39.186732,39.186733,39.186732,39.186736,39.186734,39.186737,39.186739,39.186742,39.186741,39.18674,39.186738,39.18674,39.186738,39.186737,39.186734,39.186734,39.186732,39.18673,39.18673,39.186726,39.186724,39.186721,39.186721,39.186721,39.186721,39.18672,39.18672,39.18672,39.18672,39.186719,39.186719,39.186719,39.186719,39.186718,39.186718,39.186718,39.186718,39.186717,39.186717,39.186717,39.186717,39.186716,39.186716,39.186716,39.186716,39.186715,39.186715,39.186715,39.186715,39.186714,39.186714,39.186714,39.186713,39.186713,39.186713,39.186713,39.186712,39.186712,39.186712,39.186712,39.186711,39.186711,39.186711,39.186711,39.186714,39.186713,39.186714,39.186713,39.186712,39.186713,39.186716,39.186721,39.186728,39.186729,39.186724,39.186721,39.18672,39.186718,39.186716,39.186715,39.186713,39.18671,39.186709,39.186709,39.18671,39.18671,39.18671,39.186712,39.186712,39.18671,39.186708,39.186707,39.186706,39.186705,39.186706,39.186704,39.186702,39.1867,39.186697,39.186689,39.186674,39.18667,39.186671,39.186664,39.186666,39.186667,39.186666,39.186665,39.186666,39.186644,39.186594,39.186522,39.186442,39.186351,39.186242,39.186129,39.186015,39.185903,39.185792,39.18568,39.185568,39.185456,39.185338,39.185221,39.185098,39.184977,39.184868,39.184761,39.184648,39.184538,39.184418,39.1843,39.184183,39.184063,39.18395,39.183831,39.183717,39.183596,39.183485,39.183367,39.183252,39.183152,39.183038,39.182931,39.182815,39.182683,39.182576,39.182461,39.182351,39.182252,39.182155,39.182061,39.181972,39.181898,39.181829,39.181776,39.181743,39.181726,39.181714,39.181705,39.181683,39.181665,39.18165,39.181642,39.181632,39.181621,39.181611,39.181601,39.181563,39.181532,39.181519,39.18152,39.181524,39.181523,39.181516,39.181508,39.181495,39.181465,39.181438,39.181398,39.181344,39.181293,39.18125,39.18121,39.181173,39.181148,39.181133,39.181136,39.181136,39.181132,39.181129,39.181129,39.181124,39.181118,39.181115,39.181103,39.181102,39.181099,39.181097,39.181097,39.181094,39.181094,39.181093,39.181093,39.181094,39.181091,39.181087,39.181081,39.181094,39.181094],[-96.584378,-96.584383,-96.58464499999999,-96.584667,-96.584695,-96.58465700000001,-96.584772,-96.584818,-96.584801,-96.584695,-96.584687,-96.584722,-96.584749,-96.584773,-96.58479699999999,-96.584879,-96.58492200000001,-96.58528099999999,-96.58525899999999,-96.585362,-96.58548399999999,-96.58560199999999,-96.58571999999999,-96.585652,-96.585447,-96.585472,-96.585489,-96.585505,-96.585514,-96.585515,-96.585516,-96.585516,-96.585511,-96.585504,-96.585483,-96.585472,-96.58546200000001,-96.585449,-96.58544000000001,-96.58543400000001,-96.585431,-96.585421,-96.585415,-96.585408,-96.585393,-96.58538799999999,-96.58537800000001,-96.58537,-96.585357,-96.585353,-96.585328,-96.585307,-96.585308,-96.585311,-96.58531499999999,-96.58531600000001,-96.585306,-96.585309,-96.585306,-96.585302,-96.585311,-96.585311,-96.5853,-96.58530500000001,-96.58530500000001,-96.585311,-96.58532,-96.58533,-96.585335,-96.585342,-96.585341,-96.58533799999999,-96.585337,-96.585336,-96.58532700000001,-96.58532700000001,-96.585328,-96.58532599999999,-96.58532700000001,-96.58532200000001,-96.58532,-96.585308,-96.585301,-96.58530399999999,-96.585307,-96.5853,-96.585301,-96.585301,-96.5853,-96.58529799999999,-96.585297,-96.585296,-96.585296,-96.585303,-96.585306,-96.585301,-96.585297,-96.585295,-96.585301,-96.58531600000001,-96.585353,-96.58539,-96.58540000000001,-96.585436,-96.585477,-96.585494,-96.585509,-96.585531,-96.585559,-96.585572,-96.585601,-96.585613,-96.58563700000001,-96.58565299999999,-96.58567499999999,-96.585696,-96.585717,-96.585736,-96.585758,-96.585781,-96.585801,-96.585823,-96.585848,-96.58586,-96.58587199999999,-96.58588899999999,-96.58590599999999,-96.585932,-96.585943,-96.585955,-96.585981,-96.58599100000001,-96.586001,-96.586012,-96.58603599999999,-96.586044,-96.58607000000001,-96.586079,-96.586105,-96.58611399999999,-96.586139,-96.586152,-96.58617700000001,-96.58618800000001,-96.586214,-96.586225,-96.586257,-96.586268,-96.58627799999999,-96.586287,-96.58631,-96.58631699999999,-96.58633399999999,-96.586354,-96.586366,-96.586388,-96.586405,-96.586434,-96.586445,-96.58646899999999,-96.586478,-96.586494,-96.58650400000001,-96.586519,-96.58654300000001,-96.586558,-96.586572,-96.58659299999999,-96.586614,-96.586635,-96.586637,-96.586634,-96.586631,-96.58663799999999,-96.58663900000001,-96.586664,-96.586682,-96.58668299999999,-96.586687,-96.58669,-96.58670499999999,-96.586716,-96.58671699999999,-96.58672199999999,-96.58672300000001,-96.586715,-96.586721,-96.58672199999999,-96.58671699999999,-96.586718,-96.586721,-96.586721,-96.586721,-96.586718,-96.586718,-96.586718,-96.586718,-96.586718,-96.586709,-96.586709,-96.586709,-96.586709,-96.58668400000001,-96.586662,-96.586665,-96.586626,-96.586573,-96.58649699999999,-96.58642500000001,-96.586337,-96.586276,-96.58620999999999,-96.586146,-96.586083,-96.58601400000001,-96.58593999999999,-96.585868,-96.58580000000001,-96.585734,-96.585672,-96.585618,-96.585583,-96.585548,-96.585536,-96.58551300000001,-96.58550200000001,-96.585504,-96.585503,-96.585497,-96.585486,-96.58548,-96.585477,-96.585475,-96.585477,-96.58547900000001,-96.585483,-96.58547799999999,-96.585476,-96.58547299999999,-96.585469,-96.585469,-96.585469,-96.585464,-96.585455,-96.58541700000001,-96.585362,-96.585294,-96.585204,-96.585093,-96.584996,-96.584897,-96.5848,-96.584701,-96.584599,-96.584515,-96.58445500000001,-96.584416,-96.58437000000001,-96.584352,-96.584305,-96.584243,-96.584172,-96.584091,-96.584005,-96.583929,-96.58384599999999,-96.583763,-96.58368,-96.583602,-96.583524,-96.58344200000001,-96.583365,-96.583277,-96.58318800000001,-96.583096,-96.583,-96.582908,-96.58281100000001,-96.58271499999999,-96.58260900000001,-96.58250099999999,-96.582396,-96.58229,-96.58218100000001,-96.58207299999999,-96.581965,-96.581856,-96.581754,-96.58165099999999,-96.581548,-96.581446,-96.581338,-96.581253,-96.581177,-96.581115,-96.58107200000001,-96.581023,-96.58099,-96.58098099999999,-96.580963,-96.580946,-96.580929,-96.58090799999999,-96.58089200000001,-96.580872,-96.58084100000001,-96.580831,-96.580797,-96.580786,-96.58077299999999,-96.580772,-96.580776,-96.58077400000001,-96.58077299999999,-96.58077400000001,-96.580769,-96.58076199999999,-96.580752,-96.58071099999999,-96.580692,-96.580642,-96.580579,-96.58050799999999,-96.58043600000001,-96.580349,-96.580262,-96.580168,-96.58008100000001,-96.579987,-96.579887,-96.579787,-96.579683,-96.579577,-96.57946800000001,-96.579356,-96.579245,-96.579134,-96.57902199999999,-96.578914,-96.578805,-96.578698,-96.57858899999999,-96.578474,-96.578352,-96.578238,-96.578121,-96.578005,-96.577893,-96.577789,-96.577684,-96.577581,-96.57748100000001,-96.57737899999999,-96.577277,-96.577174,-96.577068,-96.57696799999999,-96.57687300000001,-96.576778,-96.57668,-96.576604,-96.57654599999999,-96.576509,-96.576505,-96.576504,-96.576509,-96.57651199999999,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576517,-96.576539,-96.576515,-96.57648,-96.576435,-96.57639399999999,-96.57638799999999,-96.57639500000001,-96.57639500000001,-96.57639,-96.576391,-96.57638900000001,-96.576384,-96.57637699999999,-96.576385,-96.576392,-96.576392,-96.57639399999999,-96.576393,-96.576397,-96.57640499999999,-96.57640499999999,-96.57640600000001,-96.576409,-96.576404,-96.576409,-96.576413,-96.57641599999999,-96.57641700000001,-96.576429,-96.576432,-96.57643,-96.576425,-96.576425,-96.57642199999999,-96.576427,-96.57642199999999,-96.57643,-96.576435,-96.57643899999999,-96.576447,-96.57644500000001,-96.576454,-96.576452,-96.576452,-96.57645100000001,-96.576453,-96.57645599999999,-96.57646,-96.576458,-96.576459,-96.576454,-96.57646200000001,-96.576466,-96.576472,-96.57647299999999,-96.576472,-96.576476,-96.57648399999999,-96.576489,-96.57649499999999,-96.5765,-96.576503,-96.57650099999999,-96.576499,-96.576491,-96.576475,-96.576459,-96.576444,-96.57643899999999,-96.576435,-96.576437,-96.57642800000001,-96.576425,-96.57641700000001,-96.576418,-96.57642199999999,-96.576426,-96.576431,-96.576432,-96.576431,-96.57643299999999,-96.576427,-96.576424,-96.57642300000001,-96.576421,-96.576419,-96.57641700000001,-96.57641700000001,-96.576419,-96.57642800000001,-96.576455,-96.576476,-96.576482,-96.576489,-96.57648399999999,-96.57647799999999,-96.576441,-96.57637200000001,-96.57626399999999,-96.576137,-96.576016,-96.57588699999999,-96.575761,-96.575637,-96.57552,-96.575405,-96.57529100000001,-96.575187,-96.57510000000001,-96.575019,-96.57492999999999,-96.574822,-96.57472799999999,-96.574642,-96.57454300000001,-96.574426,-96.574308,-96.574202,-96.574096,-96.573994,-96.573896,-96.573806,-96.57373699999999,-96.573674,-96.573623,-96.573582,-96.573537,-96.57352400000001,-96.57350700000001,-96.573486,-96.57346800000001,-96.57346699999999,-96.573466,-96.573465,-96.573464,-96.57346200000001,-96.57346099999999,-96.57346,-96.573459,-96.573458,-96.573457,-96.573455,-96.573454,-96.573453,-96.573452,-96.57345100000001,-96.57344999999999,-96.573448,-96.573447,-96.573446,-96.57344500000001,-96.57344399999999,-96.573443,-96.573441,-96.57344000000001,-96.57343899999999,-96.573438,-96.573437,-96.573436,-96.57343400000001,-96.57343299999999,-96.573432,-96.573431,-96.57343,-96.57342800000001,-96.573427,-96.573426,-96.573425,-96.573424,-96.57342300000001,-96.573421,-96.57342,-96.57336599999999,-96.573291,-96.573198,-96.57309100000001,-96.57297,-96.572846,-96.572716,-96.572581,-96.57244300000001,-96.57230300000001,-96.572152,-96.571997,-96.571843,-96.571686,-96.57152600000001,-96.57137,-96.571213,-96.571061,-96.570915,-96.570767,-96.57061899999999,-96.570471,-96.570234,-96.570127,-96.57001,-96.56985899999999,-96.56970699999999,-96.569546,-96.569382,-96.569204,-96.569028,-96.56885800000001,-96.568684,-96.56850799999999,-96.568343,-96.568175,-96.568,-96.567837,-96.56767000000001,-96.567514,-96.567357,-96.5672,-96.567063,-96.566946,-96.566846,-96.56677000000001,-96.56671,-96.566667,-96.56667,-96.566675,-96.566677,-96.566676,-96.56667899999999,-96.566676,-96.566671,-96.566667,-96.566666,-96.56666300000001,-96.566666,-96.566672,-96.566672,-96.566672,-96.566678,-96.56667899999999,-96.566683,-96.56669100000001,-96.566695,-96.566694,-96.566697,-96.566698,-96.56670099999999,-96.56670699999999,-96.566711,-96.56671799999999,-96.566723,-96.566726,-96.566728,-96.566733,-96.56673600000001,-96.56674599999999,-96.56674700000001,-96.566751,-96.566756,-96.56675799999999,-96.56676299999999,-96.566767,-96.566771,-96.56677500000001,-96.566774,-96.566776,-96.566779,-96.566783,-96.56677999999999,-96.56677999999999,-96.56677999999999,-96.566778,-96.566782,-96.56677999999999,-96.566779,-96.566778,-96.566776,-96.56677500000001,-96.566773,-96.566772,-96.566771,-96.56676400000001,-96.56673499999999,-96.566664,-96.566568,-96.566475,-96.566378,-96.566293,-96.566222,-96.566176,-96.56615499999999,-96.566147,-96.56613400000001,-96.566142,-96.566157,-96.566168,-96.566176,-96.566193,-96.566228,-96.566267,-96.566276,-96.566282,-96.566289,-96.566289,-96.566292,-96.56629599999999,-96.56629700000001,-96.56629,-96.566294,-96.566283,-96.566275,-96.56626900000001,-96.566272,-96.56626900000001,-96.566271,-96.56626199999999,-96.56627,-96.566265,-96.56626300000001,-96.566265,-96.566271,-96.56629,-96.56629700000001],2,null,"Data Points",{"interactive":true,"className":"","stroke":true,"color":["2024-01-29T23:46:22Z","2024-01-29T23:46:23Z","2024-01-29T23:46:34Z","2024-01-29T23:46:36Z","2024-01-29T23:46:40Z","2024-01-29T23:46:41Z","2024-01-29T23:46:47Z","2024-01-29T23:46:49Z","2024-01-29T23:46:53Z","2024-01-29T23:46:55Z","2024-01-29T23:46:57Z","2024-01-29T23:46:59Z","2024-01-29T23:47:00Z","2024-01-29T23:47:02Z","2024-01-29T23:47:05Z","2024-01-29T23:47:09Z","2024-01-29T23:47:10Z","2024-01-29T23:47:15Z","2024-01-29T23:47:16Z","2024-01-29T23:47:18Z","2024-01-29T23:47:19Z","2024-01-29T23:47:20Z","2024-01-29T23:47:21Z","2024-01-29T23:47:22Z","2024-01-29T23:47:23Z","2024-01-29T23:47:24Z","2024-01-29T23:47:25Z","2024-01-29T23:47:26Z","2024-01-29T23:47:27Z","2024-01-29T23:47:28Z","2024-01-29T23:47:29Z","2024-01-29T23:47:30Z","2024-01-29T23:47:31Z","2024-01-29T23:47:32Z","2024-01-29T23:47:33Z","2024-01-29T23:47:34Z","2024-01-29T23:47:35Z","2024-01-29T23:47:36Z","2024-01-29T23:47:37Z","2024-01-29T23:47:38Z","2024-01-29T23:47:39Z","2024-01-29T23:47:40Z","2024-01-29T23:47:41Z","2024-01-29T23:47:42Z","2024-01-29T23:47:43Z","2024-01-29T23:47:44Z","2024-01-29T23:47:45Z","2024-01-29T23:47:46Z","2024-01-29T23:47:47Z","2024-01-29T23:47:48Z","2024-01-29T23:47:49Z","2024-01-29T23:47:50Z","2024-01-29T23:47:51Z","2024-01-29T23:47:52Z","2024-01-29T23:47:53Z","2024-01-29T23:47:54Z","2024-01-29T23:47:55Z","2024-01-29T23:47:56Z","2024-01-29T23:47:57Z","2024-01-29T23:47:58Z","2024-01-29T23:47:59Z","2024-01-29T23:48:00Z","2024-01-29T23:48:01Z","2024-01-29T23:48:02Z","2024-01-29T23:48:03Z","2024-01-29T23:48:04Z","2024-01-29T23:48:05Z","2024-01-29T23:48:06Z","2024-01-29T23:48:07Z","2024-01-29T23:48:08Z","2024-01-29T23:48:09Z","2024-01-29T23:48:10Z","2024-01-29T23:48:11Z","2024-01-29T23:48:12Z","2024-01-29T23:48:13Z","2024-01-29T23:48:14Z","2024-01-29T23:48:15Z","2024-01-29T23:48:16Z","2024-01-29T23:48:17Z","2024-01-29T23:48:18Z","2024-01-29T23:48:19Z","2024-01-29T23:48:20Z","2024-01-29T23:48:21Z","2024-01-29T23:48:22Z","2024-01-29T23:48:23Z","2024-01-29T23:48:24Z","2024-01-29T23:48:25Z","2024-01-29T23:48:26Z","2024-01-29T23:48:27Z","2024-01-29T23:48:28Z","2024-01-29T23:48:29Z","2024-01-29T23:48:30Z","2024-01-29T23:48:31Z","2024-01-29T23:48:32Z","2024-01-29T23:48:33Z","2024-01-29T23:48:34Z","2024-01-29T23:48:35Z","2024-01-29T23:48:36Z","2024-01-29T23:48:37Z","2024-01-29T23:48:38Z","2024-01-29T23:48:39Z","2024-01-29T23:48:40Z","2024-01-29T23:48:41Z","2024-01-29T23:48:42Z","2024-01-29T23:48:43Z","2024-01-29T23:48:44Z","2024-01-29T23:48:45Z","2024-01-29T23:48:46Z","2024-01-29T23:48:47Z","2024-01-29T23:48:48Z","2024-01-29T23:48:49Z","2024-01-29T23:48:50Z","2024-01-29T23:48:51Z","2024-01-29T23:48:52Z","2024-01-29T23:48:53Z","2024-01-29T23:48:54Z","2024-01-29T23:48:55Z","2024-01-29T23:48:56Z","2024-01-29T23:48:57Z","2024-01-29T23:48:58Z","2024-01-29T23:48:59Z","2024-01-29T23:49:00Z","2024-01-29T23:49:01Z","2024-01-29T23:49:02Z","2024-01-29T23:49:03Z","2024-01-29T23:49:04Z","2024-01-29T23:49:05Z","2024-01-29T23:49:06Z","2024-01-29T23:49:07Z","2024-01-29T23:49:08Z","2024-01-29T23:49:09Z","2024-01-29T23:49:10Z","2024-01-29T23:49:11Z","2024-01-29T23:49:12Z","2024-01-29T23:49:13Z","2024-01-29T23:49:14Z","2024-01-29T23:49:15Z","2024-01-29T23:49:16Z","2024-01-29T23:49:17Z","2024-01-29T23:49:18Z","2024-01-29T23:49:19Z","2024-01-29T23:49:20Z","2024-01-29T23:49:21Z","2024-01-29T23:49:22Z","2024-01-29T23:49:23Z","2024-01-29T23:49:24Z","2024-01-29T23:49:25Z","2024-01-29T23:49:26Z","2024-01-29T23:49:27Z","2024-01-29T23:49:28Z","2024-01-29T23:49:29Z","2024-01-29T23:49:30Z","2024-01-29T23:49:31Z","2024-01-29T23:49:32Z","2024-01-29T23:49:33Z","2024-01-29T23:49:34Z","2024-01-29T23:49:35Z","2024-01-29T23:49:36Z","2024-01-29T23:49:37Z","2024-01-29T23:49:38Z","2024-01-29T23:49:39Z","2024-01-29T23:49:40Z","2024-01-29T23:49:41Z","2024-01-29T23:49:42Z","2024-01-29T23:49:43Z","2024-01-29T23:49:44Z","2024-01-29T23:49:45Z","2024-01-29T23:49:46Z","2024-01-29T23:49:47Z","2024-01-29T23:49:48Z","2024-01-29T23:49:49Z","2024-01-29T23:49:50Z","2024-01-29T23:49:51Z","2024-01-29T23:49:52Z","2024-01-29T23:49:53Z","2024-01-29T23:49:54Z","2024-01-29T23:49:55Z","2024-01-29T23:49:56Z","2024-01-29T23:49:57Z","2024-01-29T23:49:58Z","2024-01-29T23:49:59Z","2024-01-29T23:50:00Z","2024-01-29T23:50:01Z","2024-01-29T23:50:02Z","2024-01-29T23:50:03Z","2024-01-29T23:50:04Z","2024-01-29T23:50:05Z","2024-01-29T23:50:06Z","2024-01-29T23:50:07Z","2024-01-29T23:50:08Z","2024-01-29T23:50:09Z","2024-01-29T23:50:10Z","2024-01-29T23:50:11Z","2024-01-29T23:50:12Z","2024-01-29T23:50:13Z","2024-01-29T23:50:14Z","2024-01-29T23:50:15Z","2024-01-29T23:50:16Z","2024-01-29T23:50:17Z","2024-01-29T23:50:18Z","2024-01-29T23:50:19Z","2024-01-29T23:50:20Z","2024-01-29T23:50:21Z","2024-01-29T23:50:22Z","2024-01-29T23:50:23Z","2024-01-29T23:50:24Z","2024-01-29T23:50:25Z","2024-01-29T23:50:26Z","2024-01-29T23:50:27Z","2024-01-29T23:50:28Z","2024-01-29T23:50:29Z","2024-01-29T23:50:30Z","2024-01-29T23:50:31Z","2024-01-29T23:50:32Z","2024-01-29T23:50:33Z","2024-01-29T23:50:34Z","2024-01-29T23:50:35Z","2024-01-29T23:50:36Z","2024-01-29T23:50:37Z","2024-01-29T23:50:38Z","2024-01-29T23:50:39Z","2024-01-29T23:50:40Z","2024-01-29T23:50:41Z","2024-01-29T23:50:42Z","2024-01-29T23:50:43Z","2024-01-29T23:50:44Z","2024-01-29T23:50:45Z","2024-01-29T23:50:46Z","2024-01-29T23:50:47Z","2024-01-29T23:50:48Z","2024-01-29T23:50:49Z","2024-01-29T23:50:50Z","2024-01-29T23:50:51Z","2024-01-29T23:50:52Z","2024-01-29T23:50:53Z","2024-01-29T23:50:54Z","2024-01-29T23:50:55Z","2024-01-29T23:50:56Z","2024-01-29T23:50:57Z","2024-01-29T23:50:58Z","2024-01-29T23:50:59Z","2024-01-29T23:51:00Z","2024-01-29T23:51:01Z","2024-01-29T23:51:02Z","2024-01-29T23:51:03Z","2024-01-29T23:51:04Z","2024-01-29T23:51:05Z","2024-01-29T23:51:06Z","2024-01-29T23:51:07Z","2024-01-29T23:51:08Z","2024-01-29T23:51:09Z","2024-01-29T23:51:10Z","2024-01-29T23:51:11Z","2024-01-29T23:51:12Z","2024-01-29T23:51:13Z","2024-01-29T23:51:14Z","2024-01-29T23:51:15Z","2024-01-29T23:51:16Z","2024-01-29T23:51:17Z","2024-01-29T23:51:18Z","2024-01-29T23:51:19Z","2024-01-29T23:51:20Z","2024-01-29T23:51:21Z","2024-01-29T23:51:22Z","2024-01-29T23:51:23Z","2024-01-29T23:51:24Z","2024-01-29T23:51:25Z","2024-01-29T23:51:26Z","2024-01-29T23:51:27Z","2024-01-29T23:51:28Z","2024-01-29T23:51:29Z","2024-01-29T23:51:30Z","2024-01-29T23:51:31Z","2024-01-29T23:51:32Z","2024-01-29T23:51:33Z","2024-01-29T23:51:34Z","2024-01-29T23:51:35Z","2024-01-29T23:51:36Z","2024-01-29T23:51:37Z","2024-01-29T23:51:38Z","2024-01-29T23:51:39Z","2024-01-29T23:51:40Z","2024-01-29T23:51:41Z","2024-01-29T23:51:42Z","2024-01-29T23:51:43Z","2024-01-29T23:51:44Z","2024-01-29T23:51:45Z","2024-01-29T23:51:46Z","2024-01-29T23:51:47Z","2024-01-29T23:51:48Z","2024-01-29T23:51:49Z","2024-01-29T23:51:50Z","2024-01-29T23:51:51Z","2024-01-29T23:51:52Z","2024-01-29T23:51:53Z","2024-01-29T23:51:54Z","2024-01-29T23:51:55Z","2024-01-29T23:51:56Z","2024-01-29T23:51:57Z","2024-01-29T23:51:58Z","2024-01-29T23:51:59Z","2024-01-29T23:52:00Z","2024-01-29T23:52:01Z","2024-01-29T23:52:02Z","2024-01-29T23:52:03Z","2024-01-29T23:52:04Z","2024-01-29T23:52:05Z","2024-01-29T23:52:06Z","2024-01-29T23:52:07Z","2024-01-29T23:52:08Z","2024-01-29T23:52:09Z","2024-01-29T23:52:10Z","2024-01-29T23:52:11Z","2024-01-29T23:52:12Z","2024-01-29T23:52:13Z","2024-01-29T23:52:14Z","2024-01-29T23:52:15Z","2024-01-29T23:52:16Z","2024-01-29T23:52:17Z","2024-01-29T23:52:18Z","2024-01-29T23:52:19Z","2024-01-29T23:52:20Z","2024-01-29T23:52:21Z","2024-01-29T23:52:22Z","2024-01-29T23:52:23Z","2024-01-29T23:52:24Z","2024-01-29T23:52:25Z","2024-01-29T23:52:26Z","2024-01-29T23:52:27Z","2024-01-29T23:52:28Z","2024-01-29T23:52:29Z","2024-01-29T23:52:30Z","2024-01-29T23:52:31Z","2024-01-29T23:52:32Z","2024-01-29T23:52:33Z","2024-01-29T23:52:34Z","2024-01-29T23:52:35Z","2024-01-29T23:52:36Z","2024-01-29T23:52:37Z","2024-01-29T23:52:38Z","2024-01-29T23:52:39Z","2024-01-29T23:52:40Z","2024-01-29T23:52:41Z","2024-01-29T23:52:42Z","2024-01-29T23:52:43Z","2024-01-29T23:52:44Z","2024-01-29T23:52:45Z","2024-01-29T23:52:46Z","2024-01-29T23:52:47Z","2024-01-29T23:52:48Z","2024-01-29T23:52:49Z","2024-01-29T23:52:50Z","2024-01-29T23:52:51Z","2024-01-29T23:52:52Z","2024-01-29T23:52:53Z","2024-01-29T23:52:54Z","2024-01-29T23:52:55Z","2024-01-29T23:52:56Z","2024-01-29T23:52:57Z","2024-01-29T23:52:58Z","2024-01-29T23:52:59Z","2024-01-29T23:53:00Z","2024-01-29T23:53:01Z","2024-01-29T23:53:02Z","2024-01-29T23:53:03Z","2024-01-29T23:53:04Z","2024-01-29T23:53:05Z","2024-01-29T23:53:06Z","2024-01-29T23:53:07Z","2024-01-29T23:53:08Z","2024-01-29T23:53:09Z","2024-01-29T23:53:10Z","2024-01-29T23:53:11Z","2024-01-29T23:53:12Z","2024-01-29T23:53:13Z","2024-01-29T23:53:14Z","2024-01-29T23:53:15Z","2024-01-29T23:53:16Z","2024-01-29T23:53:17Z","2024-01-29T23:53:18Z","2024-01-29T23:53:19Z","2024-01-29T23:53:20Z","2024-01-29T23:53:21Z","2024-01-29T23:53:22Z","2024-01-29T23:53:23Z","2024-01-29T23:53:24Z","2024-01-29T23:53:25Z","2024-01-29T23:53:26Z","2024-01-29T23:53:27Z","2024-01-29T23:53:28Z","2024-01-29T23:53:29Z","2024-01-29T23:53:30Z","2024-01-29T23:53:31Z","2024-01-29T23:53:32Z","2024-01-29T23:53:33Z","2024-01-29T23:53:34Z","2024-01-29T23:53:35Z","2024-01-29T23:53:36Z","2024-01-29T23:53:37Z","2024-01-29T23:53:38Z","2024-01-29T23:53:39Z","2024-01-29T23:53:40Z","2024-01-29T23:53:41Z","2024-01-29T23:53:42Z","2024-01-29T23:53:43Z","2024-01-29T23:53:45Z","2024-01-29T23:53:46Z","2024-01-29T23:53:47Z","2024-01-29T23:53:48Z","2024-01-29T23:53:49Z","2024-01-29T23:53:50Z","2024-01-29T23:53:51Z","2024-01-29T23:53:52Z","2024-01-29T23:53:53Z","2024-01-29T23:53:54Z","2024-01-29T23:53:55Z","2024-01-29T23:53:56Z","2024-01-29T23:53:57Z","2024-01-29T23:53:58Z","2024-01-29T23:53:59Z","2024-01-29T23:54:00Z","2024-01-29T23:54:01Z","2024-01-29T23:54:02Z","2024-01-29T23:54:03Z","2024-01-29T23:54:04Z","2024-01-29T23:54:05Z","2024-01-29T23:54:06Z","2024-01-29T23:54:07Z","2024-01-29T23:54:08Z","2024-01-29T23:54:09Z","2024-01-29T23:54:10Z","2024-01-29T23:54:11Z","2024-01-29T23:54:12Z","2024-01-29T23:54:13Z","2024-01-29T23:54:14Z","2024-01-29T23:54:15Z","2024-01-29T23:54:16Z","2024-01-29T23:54:17Z","2024-01-29T23:54:18Z","2024-01-29T23:54:19Z","2024-01-29T23:54:20Z","2024-01-29T23:54:21Z","2024-01-29T23:54:22Z","2024-01-29T23:54:23Z","2024-01-29T23:54:24Z","2024-01-29T23:54:25Z","2024-01-29T23:54:26Z","2024-01-29T23:54:27Z","2024-01-29T23:54:28Z","2024-01-29T23:54:29Z","2024-01-29T23:54:30Z","2024-01-29T23:54:31Z","2024-01-29T23:54:32Z","2024-01-29T23:54:33Z","2024-01-29T23:54:34Z","2024-01-29T23:54:35Z","2024-01-29T23:54:36Z","2024-01-29T23:54:37Z","2024-01-29T23:54:38Z","2024-01-29T23:54:39Z","2024-01-29T23:54:40Z","2024-01-29T23:54:41Z","2024-01-29T23:54:42Z","2024-01-29T23:54:43Z","2024-01-29T23:54:44Z","2024-01-29T23:54:45Z","2024-01-29T23:54:46Z","2024-01-29T23:54:47Z","2024-01-29T23:54:48Z","2024-01-29T23:54:49Z","2024-01-29T23:54:50Z","2024-01-29T23:54:51Z","2024-01-29T23:54:52Z","2024-01-29T23:54:53Z","2024-01-29T23:54:54Z","2024-01-29T23:54:55Z","2024-01-29T23:54:56Z","2024-01-29T23:54:57Z","2024-01-29T23:54:58Z","2024-01-29T23:54:59Z","2024-01-29T23:55:00Z","2024-01-29T23:55:01Z","2024-01-29T23:55:02Z","2024-01-29T23:55:03Z","2024-01-29T23:55:04Z","2024-01-29T23:55:05Z","2024-01-29T23:55:06Z","2024-01-29T23:55:07Z","2024-01-29T23:55:08Z","2024-01-29T23:55:09Z","2024-01-29T23:55:10Z","2024-01-29T23:55:11Z","2024-01-29T23:55:12Z","2024-01-29T23:55:13Z","2024-01-29T23:55:14Z","2024-01-29T23:55:15Z","2024-01-29T23:55:16Z","2024-01-29T23:55:17Z","2024-01-29T23:55:18Z","2024-01-29T23:55:19Z","2024-01-29T23:55:20Z","2024-01-29T23:55:21Z","2024-01-29T23:55:22Z","2024-01-29T23:55:23Z","2024-01-29T23:55:24Z","2024-01-29T23:55:25Z","2024-01-29T23:55:26Z","2024-01-29T23:55:27Z","2024-01-29T23:55:28Z","2024-01-29T23:55:29Z","2024-01-29T23:55:30Z","2024-01-29T23:55:31Z","2024-01-29T23:55:32Z","2024-01-29T23:55:33Z","2024-01-29T23:55:34Z","2024-01-29T23:55:35Z","2024-01-29T23:55:36Z","2024-01-29T23:55:37Z","2024-01-29T23:55:38Z","2024-01-29T23:55:39Z","2024-01-29T23:55:40Z","2024-01-29T23:55:41Z","2024-01-29T23:55:42Z","2024-01-29T23:55:43Z","2024-01-29T23:55:44Z","2024-01-29T23:55:45Z","2024-01-29T23:55:46Z","2024-01-29T23:55:47Z","2024-01-29T23:55:48Z","2024-01-29T23:55:49Z","2024-01-29T23:55:51Z","2024-01-29T23:55:52Z","2024-01-29T23:55:53Z","2024-01-29T23:55:54Z","2024-01-29T23:55:55Z","2024-01-29T23:55:56Z","2024-01-29T23:55:57Z","2024-01-29T23:55:58Z","2024-01-29T23:55:59Z","2024-01-29T23:56:00Z","2024-01-29T23:56:01Z","2024-01-29T23:56:02Z","2024-01-29T23:56:03Z","2024-01-29T23:56:04Z","2024-01-29T23:56:05Z","2024-01-29T23:56:06Z","2024-01-29T23:56:07Z","2024-01-29T23:56:08Z","2024-01-29T23:56:09Z","2024-01-29T23:56:10Z","2024-01-29T23:56:11Z","2024-01-29T23:56:12Z","2024-01-29T23:56:13Z","2024-01-29T23:56:14Z","2024-01-29T23:56:15Z","2024-01-29T23:56:16Z","2024-01-29T23:56:17Z","2024-01-29T23:56:18Z","2024-01-29T23:56:19Z","2024-01-29T23:56:20Z","2024-01-29T23:56:21Z","2024-01-29T23:56:22Z","2024-01-29T23:56:23Z","2024-01-29T23:56:24Z","2024-01-29T23:56:25Z","2024-01-29T23:56:26Z","2024-01-29T23:56:27Z","2024-01-29T23:56:28Z","2024-01-29T23:56:29Z","2024-01-29T23:56:30Z","2024-01-29T23:56:31Z","2024-01-29T23:56:32Z","2024-01-29T23:56:33Z","2024-01-29T23:56:34Z","2024-01-29T23:56:35Z","2024-01-29T23:56:36Z","2024-01-29T23:56:37Z","2024-01-29T23:56:38Z","2024-01-29T23:56:39Z","2024-01-29T23:56:40Z","2024-01-29T23:56:41Z","2024-01-29T23:56:42Z","2024-01-29T23:56:43Z","2024-01-29T23:56:44Z","2024-01-29T23:56:45Z","2024-01-29T23:56:46Z","2024-01-29T23:56:47Z","2024-01-29T23:56:48Z","2024-01-29T23:56:49Z","2024-01-29T23:56:50Z","2024-01-29T23:56:51Z","2024-01-29T23:56:52Z","2024-01-29T23:56:53Z","2024-01-29T23:56:54Z","2024-01-29T23:56:55Z","2024-01-29T23:56:56Z","2024-01-29T23:56:57Z","2024-01-29T23:56:58Z","2024-01-29T23:56:59Z","2024-01-29T23:57:00Z","2024-01-29T23:57:01Z","2024-01-29T23:57:02Z","2024-01-29T23:57:03Z","2024-01-29T23:57:04Z","2024-01-29T23:57:05Z","2024-01-29T23:57:06Z","2024-01-29T23:57:07Z","2024-01-29T23:57:08Z","2024-01-29T23:57:09Z","2024-01-29T23:57:10Z","2024-01-29T23:57:11Z","2024-01-29T23:57:12Z","2024-01-29T23:57:13Z","2024-01-29T23:57:14Z","2024-01-29T23:57:15Z","2024-01-29T23:57:16Z","2024-01-29T23:57:17Z","2024-01-29T23:57:18Z","2024-01-29T23:57:19Z","2024-01-29T23:57:20Z","2024-01-29T23:57:21Z","2024-01-29T23:57:22Z","2024-01-29T23:57:23Z","2024-01-29T23:57:24Z","2024-01-29T23:57:25Z","2024-01-29T23:57:26Z","2024-01-29T23:57:27Z","2024-01-29T23:57:28Z","2024-01-29T23:57:29Z","2024-01-29T23:57:30Z","2024-01-29T23:57:31Z","2024-01-29T23:57:32Z","2024-01-29T23:57:33Z","2024-01-29T23:57:34Z","2024-01-29T23:57:35Z","2024-01-29T23:57:36Z","2024-01-29T23:57:37Z","2024-01-29T23:57:38Z","2024-01-29T23:57:39Z","2024-01-29T23:57:40Z","2024-01-29T23:57:41Z","2024-01-29T23:57:42Z","2024-01-29T23:57:43Z","2024-01-29T23:57:44Z","2024-01-29T23:57:45Z","2024-01-29T23:57:46Z","2024-01-29T23:57:47Z","2024-01-29T23:57:48Z","2024-01-29T23:57:49Z","2024-01-29T23:57:50Z","2024-01-29T23:57:51Z","2024-01-29T23:57:52Z","2024-01-29T23:57:53Z","2024-01-29T23:57:54Z","2024-01-29T23:57:55Z","2024-01-29T23:57:57Z","2024-01-29T23:57:58Z","2024-01-29T23:57:59Z","2024-01-29T23:58:00Z","2024-01-29T23:58:01Z","2024-01-29T23:58:02Z","2024-01-29T23:58:03Z","2024-01-29T23:58:04Z","2024-01-29T23:58:05Z","2024-01-29T23:58:06Z","2024-01-29T23:58:07Z","2024-01-29T23:58:08Z","2024-01-29T23:58:09Z","2024-01-29T23:58:10Z","2024-01-29T23:58:11Z","2024-01-29T23:58:12Z","2024-01-29T23:58:13Z","2024-01-29T23:58:14Z","2024-01-29T23:58:15Z","2024-01-29T23:58:16Z","2024-01-29T23:58:17Z","2024-01-29T23:58:18Z","2024-01-29T23:58:19Z","2024-01-29T23:58:20Z","2024-01-29T23:58:21Z","2024-01-29T23:58:22Z","2024-01-29T23:58:23Z","2024-01-29T23:58:24Z","2024-01-29T23:58:25Z","2024-01-29T23:58:26Z","2024-01-29T23:58:27Z","2024-01-29T23:58:28Z","2024-01-29T23:58:29Z","2024-01-29T23:58:30Z","2024-01-29T23:58:31Z","2024-01-29T23:58:32Z","2024-01-29T23:58:33Z","2024-01-29T23:58:34Z","2024-01-29T23:58:35Z","2024-01-29T23:58:36Z","2024-01-29T23:58:37Z","2024-01-29T23:58:38Z","2024-01-29T23:58:39Z","2024-01-29T23:58:40Z","2024-01-29T23:58:41Z","2024-01-29T23:58:42Z","2024-01-29T23:58:43Z"],"weight":5,"opacity":0.5,"fill":true,"fillColor":["2024-01-29T23:46:22Z","2024-01-29T23:46:23Z","2024-01-29T23:46:34Z","2024-01-29T23:46:36Z","2024-01-29T23:46:40Z","2024-01-29T23:46:41Z","2024-01-29T23:46:47Z","2024-01-29T23:46:49Z","2024-01-29T23:46:53Z","2024-01-29T23:46:55Z","2024-01-29T23:46:57Z","2024-01-29T23:46:59Z","2024-01-29T23:47:00Z","2024-01-29T23:47:02Z","2024-01-29T23:47:05Z","2024-01-29T23:47:09Z","2024-01-29T23:47:10Z","2024-01-29T23:47:15Z","2024-01-29T23:47:16Z","2024-01-29T23:47:18Z","2024-01-29T23:47:19Z","2024-01-29T23:47:20Z","2024-01-29T23:47:21Z","2024-01-29T23:47:22Z","2024-01-29T23:47:23Z","2024-01-29T23:47:24Z","2024-01-29T23:47:25Z","2024-01-29T23:47:26Z","2024-01-29T23:47:27Z","2024-01-29T23:47:28Z","2024-01-29T23:47:29Z","2024-01-29T23:47:30Z","2024-01-29T23:47:31Z","2024-01-29T23:47:32Z","2024-01-29T23:47:33Z","2024-01-29T23:47:34Z","2024-01-29T23:47:35Z","2024-01-29T23:47:36Z","2024-01-29T23:47:37Z","2024-01-29T23:47:38Z","2024-01-29T23:47:39Z","2024-01-29T23:47:40Z","2024-01-29T23:47:41Z","2024-01-29T23:47:42Z","2024-01-29T23:47:43Z","2024-01-29T23:47:44Z","2024-01-29T23:47:45Z","2024-01-29T23:47:46Z","2024-01-29T23:47:47Z","2024-01-29T23:47:48Z","2024-01-29T23:47:49Z","2024-01-29T23:47:50Z","2024-01-29T23:47:51Z","2024-01-29T23:47:52Z","2024-01-29T23:47:53Z","2024-01-29T23:47:54Z","2024-01-29T23:47:55Z","2024-01-29T23:47:56Z","2024-01-29T23:47:57Z","2024-01-29T23:47:58Z","2024-01-29T23:47:59Z","2024-01-29T23:48:00Z","2024-01-29T23:48:01Z","2024-01-29T23:48:02Z","2024-01-29T23:48:03Z","2024-01-29T23:48:04Z","2024-01-29T23:48:05Z","2024-01-29T23:48:06Z","2024-01-29T23:48:07Z","2024-01-29T23:48:08Z","2024-01-29T23:48:09Z","2024-01-29T23:48:10Z","2024-01-29T23:48:11Z","2024-01-29T23:48:12Z","2024-01-29T23:48:13Z","2024-01-29T23:48:14Z","2024-01-29T23:48:15Z","2024-01-29T23:48:16Z","2024-01-29T23:48:17Z","2024-01-29T23:48:18Z","2024-01-29T23:48:19Z","2024-01-29T23:48:20Z","2024-01-29T23:48:21Z","2024-01-29T23:48:22Z","2024-01-29T23:48:23Z","2024-01-29T23:48:24Z","2024-01-29T23:48:25Z","2024-01-29T23:48:26Z","2024-01-29T23:48:27Z","2024-01-29T23:48:28Z","2024-01-29T23:48:29Z","2024-01-29T23:48:30Z","2024-01-29T23:48:31Z","2024-01-29T23:48:32Z","2024-01-29T23:48:33Z","2024-01-29T23:48:34Z","2024-01-29T23:48:35Z","2024-01-29T23:48:36Z","2024-01-29T23:48:37Z","2024-01-29T23:48:38Z","2024-01-29T23:48:39Z","2024-01-29T23:48:40Z","2024-01-29T23:48:41Z","2024-01-29T23:48:42Z","2024-01-29T23:48:43Z","2024-01-29T23:48:44Z","2024-01-29T23:48:45Z","2024-01-29T23:48:46Z","2024-01-29T23:48:47Z","2024-01-29T23:48:48Z","2024-01-29T23:48:49Z","2024-01-29T23:48:50Z","2024-01-29T23:48:51Z","2024-01-29T23:48:52Z","2024-01-29T23:48:53Z","2024-01-29T23:48:54Z","2024-01-29T23:48:55Z","2024-01-29T23:48:56Z","2024-01-29T23:48:57Z","2024-01-29T23:48:58Z","2024-01-29T23:48:59Z","2024-01-29T23:49:00Z","2024-01-29T23:49:01Z","2024-01-29T23:49:02Z","2024-01-29T23:49:03Z","2024-01-29T23:49:04Z","2024-01-29T23:49:05Z","2024-01-29T23:49:06Z","2024-01-29T23:49:07Z","2024-01-29T23:49:08Z","2024-01-29T23:49:09Z","2024-01-29T23:49:10Z","2024-01-29T23:49:11Z","2024-01-29T23:49:12Z","2024-01-29T23:49:13Z","2024-01-29T23:49:14Z","2024-01-29T23:49:15Z","2024-01-29T23:49:16Z","2024-01-29T23:49:17Z","2024-01-29T23:49:18Z","2024-01-29T23:49:19Z","2024-01-29T23:49:20Z","2024-01-29T23:49:21Z","2024-01-29T23:49:22Z","2024-01-29T23:49:23Z","2024-01-29T23:49:24Z","2024-01-29T23:49:25Z","2024-01-29T23:49:26Z","2024-01-29T23:49:27Z","2024-01-29T23:49:28Z","2024-01-29T23:49:29Z","2024-01-29T23:49:30Z","2024-01-29T23:49:31Z","2024-01-29T23:49:32Z","2024-01-29T23:49:33Z","2024-01-29T23:49:34Z","2024-01-29T23:49:35Z","2024-01-29T23:49:36Z","2024-01-29T23:49:37Z","2024-01-29T23:49:38Z","2024-01-29T23:49:39Z","2024-01-29T23:49:40Z","2024-01-29T23:49:41Z","2024-01-29T23:49:42Z","2024-01-29T23:49:43Z","2024-01-29T23:49:44Z","2024-01-29T23:49:45Z","2024-01-29T23:49:46Z","2024-01-29T23:49:47Z","2024-01-29T23:49:48Z","2024-01-29T23:49:49Z","2024-01-29T23:49:50Z","2024-01-29T23:49:51Z","2024-01-29T23:49:52Z","2024-01-29T23:49:53Z","2024-01-29T23:49:54Z","2024-01-29T23:49:55Z","2024-01-29T23:49:56Z","2024-01-29T23:49:57Z","2024-01-29T23:49:58Z","2024-01-29T23:49:59Z","2024-01-29T23:50:00Z","2024-01-29T23:50:01Z","2024-01-29T23:50:02Z","2024-01-29T23:50:03Z","2024-01-29T23:50:04Z","2024-01-29T23:50:05Z","2024-01-29T23:50:06Z","2024-01-29T23:50:07Z","2024-01-29T23:50:08Z","2024-01-29T23:50:09Z","2024-01-29T23:50:10Z","2024-01-29T23:50:11Z","2024-01-29T23:50:12Z","2024-01-29T23:50:13Z","2024-01-29T23:50:14Z","2024-01-29T23:50:15Z","2024-01-29T23:50:16Z","2024-01-29T23:50:17Z","2024-01-29T23:50:18Z","2024-01-29T23:50:19Z","2024-01-29T23:50:20Z","2024-01-29T23:50:21Z","2024-01-29T23:50:22Z","2024-01-29T23:50:23Z","2024-01-29T23:50:24Z","2024-01-29T23:50:25Z","2024-01-29T23:50:26Z","2024-01-29T23:50:27Z","2024-01-29T23:50:28Z","2024-01-29T23:50:29Z","2024-01-29T23:50:30Z","2024-01-29T23:50:31Z","2024-01-29T23:50:32Z","2024-01-29T23:50:33Z","2024-01-29T23:50:34Z","2024-01-29T23:50:35Z","2024-01-29T23:50:36Z","2024-01-29T23:50:37Z","2024-01-29T23:50:38Z","2024-01-29T23:50:39Z","2024-01-29T23:50:40Z","2024-01-29T23:50:41Z","2024-01-29T23:50:42Z","2024-01-29T23:50:43Z","2024-01-29T23:50:44Z","2024-01-29T23:50:45Z","2024-01-29T23:50:46Z","2024-01-29T23:50:47Z","2024-01-29T23:50:48Z","2024-01-29T23:50:49Z","2024-01-29T23:50:50Z","2024-01-29T23:50:51Z","2024-01-29T23:50:52Z","2024-01-29T23:50:53Z","2024-01-29T23:50:54Z","2024-01-29T23:50:55Z","2024-01-29T23:50:56Z","2024-01-29T23:50:57Z","2024-01-29T23:50:58Z","2024-01-29T23:50:59Z","2024-01-29T23:51:00Z","2024-01-29T23:51:01Z","2024-01-29T23:51:02Z","2024-01-29T23:51:03Z","2024-01-29T23:51:04Z","2024-01-29T23:51:05Z","2024-01-29T23:51:06Z","2024-01-29T23:51:07Z","2024-01-29T23:51:08Z","2024-01-29T23:51:09Z","2024-01-29T23:51:10Z","2024-01-29T23:51:11Z","2024-01-29T23:51:12Z","2024-01-29T23:51:13Z","2024-01-29T23:51:14Z","2024-01-29T23:51:15Z","2024-01-29T23:51:16Z","2024-01-29T23:51:17Z","2024-01-29T23:51:18Z","2024-01-29T23:51:19Z","2024-01-29T23:51:20Z","2024-01-29T23:51:21Z","2024-01-29T23:51:22Z","2024-01-29T23:51:23Z","2024-01-29T23:51:24Z","2024-01-29T23:51:25Z","2024-01-29T23:51:26Z","2024-01-29T23:51:27Z","2024-01-29T23:51:28Z","2024-01-29T23:51:29Z","2024-01-29T23:51:30Z","2024-01-29T23:51:31Z","2024-01-29T23:51:32Z","2024-01-29T23:51:33Z","2024-01-29T23:51:34Z","2024-01-29T23:51:35Z","2024-01-29T23:51:36Z","2024-01-29T23:51:37Z","2024-01-29T23:51:38Z","2024-01-29T23:51:39Z","2024-01-29T23:51:40Z","2024-01-29T23:51:41Z","2024-01-29T23:51:42Z","2024-01-29T23:51:43Z","2024-01-29T23:51:44Z","2024-01-29T23:51:45Z","2024-01-29T23:51:46Z","2024-01-29T23:51:47Z","2024-01-29T23:51:48Z","2024-01-29T23:51:49Z","2024-01-29T23:51:50Z","2024-01-29T23:51:51Z","2024-01-29T23:51:52Z","2024-01-29T23:51:53Z","2024-01-29T23:51:54Z","2024-01-29T23:51:55Z","2024-01-29T23:51:56Z","2024-01-29T23:51:57Z","2024-01-29T23:51:58Z","2024-01-29T23:51:59Z","2024-01-29T23:52:00Z","2024-01-29T23:52:01Z","2024-01-29T23:52:02Z","2024-01-29T23:52:03Z","2024-01-29T23:52:04Z","2024-01-29T23:52:05Z","2024-01-29T23:52:06Z","2024-01-29T23:52:07Z","2024-01-29T23:52:08Z","2024-01-29T23:52:09Z","2024-01-29T23:52:10Z","2024-01-29T23:52:11Z","2024-01-29T23:52:12Z","2024-01-29T23:52:13Z","2024-01-29T23:52:14Z","2024-01-29T23:52:15Z","2024-01-29T23:52:16Z","2024-01-29T23:52:17Z","2024-01-29T23:52:18Z","2024-01-29T23:52:19Z","2024-01-29T23:52:20Z","2024-01-29T23:52:21Z","2024-01-29T23:52:22Z","2024-01-29T23:52:23Z","2024-01-29T23:52:24Z","2024-01-29T23:52:25Z","2024-01-29T23:52:26Z","2024-01-29T23:52:27Z","2024-01-29T23:52:28Z","2024-01-29T23:52:29Z","2024-01-29T23:52:30Z","2024-01-29T23:52:31Z","2024-01-29T23:52:32Z","2024-01-29T23:52:33Z","2024-01-29T23:52:34Z","2024-01-29T23:52:35Z","2024-01-29T23:52:36Z","2024-01-29T23:52:37Z","2024-01-29T23:52:38Z","2024-01-29T23:52:39Z","2024-01-29T23:52:40Z","2024-01-29T23:52:41Z","2024-01-29T23:52:42Z","2024-01-29T23:52:43Z","2024-01-29T23:52:44Z","2024-01-29T23:52:45Z","2024-01-29T23:52:46Z","2024-01-29T23:52:47Z","2024-01-29T23:52:48Z","2024-01-29T23:52:49Z","2024-01-29T23:52:50Z","2024-01-29T23:52:51Z","2024-01-29T23:52:52Z","2024-01-29T23:52:53Z","2024-01-29T23:52:54Z","2024-01-29T23:52:55Z","2024-01-29T23:52:56Z","2024-01-29T23:52:57Z","2024-01-29T23:52:58Z","2024-01-29T23:52:59Z","2024-01-29T23:53:00Z","2024-01-29T23:53:01Z","2024-01-29T23:53:02Z","2024-01-29T23:53:03Z","2024-01-29T23:53:04Z","2024-01-29T23:53:05Z","2024-01-29T23:53:06Z","2024-01-29T23:53:07Z","2024-01-29T23:53:08Z","2024-01-29T23:53:09Z","2024-01-29T23:53:10Z","2024-01-29T23:53:11Z","2024-01-29T23:53:12Z","2024-01-29T23:53:13Z","2024-01-29T23:53:14Z","2024-01-29T23:53:15Z","2024-01-29T23:53:16Z","2024-01-29T23:53:17Z","2024-01-29T23:53:18Z","2024-01-29T23:53:19Z","2024-01-29T23:53:20Z","2024-01-29T23:53:21Z","2024-01-29T23:53:22Z","2024-01-29T23:53:23Z","2024-01-29T23:53:24Z","2024-01-29T23:53:25Z","2024-01-29T23:53:26Z","2024-01-29T23:53:27Z","2024-01-29T23:53:28Z","2024-01-29T23:53:29Z","2024-01-29T23:53:30Z","2024-01-29T23:53:31Z","2024-01-29T23:53:32Z","2024-01-29T23:53:33Z","2024-01-29T23:53:34Z","2024-01-29T23:53:35Z","2024-01-29T23:53:36Z","2024-01-29T23:53:37Z","2024-01-29T23:53:38Z","2024-01-29T23:53:39Z","2024-01-29T23:53:40Z","2024-01-29T23:53:41Z","2024-01-29T23:53:42Z","2024-01-29T23:53:43Z","2024-01-29T23:53:45Z","2024-01-29T23:53:46Z","2024-01-29T23:53:47Z","2024-01-29T23:53:48Z","2024-01-29T23:53:49Z","2024-01-29T23:53:50Z","2024-01-29T23:53:51Z","2024-01-29T23:53:52Z","2024-01-29T23:53:53Z","2024-01-29T23:53:54Z","2024-01-29T23:53:55Z","2024-01-29T23:53:56Z","2024-01-29T23:53:57Z","2024-01-29T23:53:58Z","2024-01-29T23:53:59Z","2024-01-29T23:54:00Z","2024-01-29T23:54:01Z","2024-01-29T23:54:02Z","2024-01-29T23:54:03Z","2024-01-29T23:54:04Z","2024-01-29T23:54:05Z","2024-01-29T23:54:06Z","2024-01-29T23:54:07Z","2024-01-29T23:54:08Z","2024-01-29T23:54:09Z","2024-01-29T23:54:10Z","2024-01-29T23:54:11Z","2024-01-29T23:54:12Z","2024-01-29T23:54:13Z","2024-01-29T23:54:14Z","2024-01-29T23:54:15Z","2024-01-29T23:54:16Z","2024-01-29T23:54:17Z","2024-01-29T23:54:18Z","2024-01-29T23:54:19Z","2024-01-29T23:54:20Z","2024-01-29T23:54:21Z","2024-01-29T23:54:22Z","2024-01-29T23:54:23Z","2024-01-29T23:54:24Z","2024-01-29T23:54:25Z","2024-01-29T23:54:26Z","2024-01-29T23:54:27Z","2024-01-29T23:54:28Z","2024-01-29T23:54:29Z","2024-01-29T23:54:30Z","2024-01-29T23:54:31Z","2024-01-29T23:54:32Z","2024-01-29T23:54:33Z","2024-01-29T23:54:34Z","2024-01-29T23:54:35Z","2024-01-29T23:54:36Z","2024-01-29T23:54:37Z","2024-01-29T23:54:38Z","2024-01-29T23:54:39Z","2024-01-29T23:54:40Z","2024-01-29T23:54:41Z","2024-01-29T23:54:42Z","2024-01-29T23:54:43Z","2024-01-29T23:54:44Z","2024-01-29T23:54:45Z","2024-01-29T23:54:46Z","2024-01-29T23:54:47Z","2024-01-29T23:54:48Z","2024-01-29T23:54:49Z","2024-01-29T23:54:50Z","2024-01-29T23:54:51Z","2024-01-29T23:54:52Z","2024-01-29T23:54:53Z","2024-01-29T23:54:54Z","2024-01-29T23:54:55Z","2024-01-29T23:54:56Z","2024-01-29T23:54:57Z","2024-01-29T23:54:58Z","2024-01-29T23:54:59Z","2024-01-29T23:55:00Z","2024-01-29T23:55:01Z","2024-01-29T23:55:02Z","2024-01-29T23:55:03Z","2024-01-29T23:55:04Z","2024-01-29T23:55:05Z","2024-01-29T23:55:06Z","2024-01-29T23:55:07Z","2024-01-29T23:55:08Z","2024-01-29T23:55:09Z","2024-01-29T23:55:10Z","2024-01-29T23:55:11Z","2024-01-29T23:55:12Z","2024-01-29T23:55:13Z","2024-01-29T23:55:14Z","2024-01-29T23:55:15Z","2024-01-29T23:55:16Z","2024-01-29T23:55:17Z","2024-01-29T23:55:18Z","2024-01-29T23:55:19Z","2024-01-29T23:55:20Z","2024-01-29T23:55:21Z","2024-01-29T23:55:22Z","2024-01-29T23:55:23Z","2024-01-29T23:55:24Z","2024-01-29T23:55:25Z","2024-01-29T23:55:26Z","2024-01-29T23:55:27Z","2024-01-29T23:55:28Z","2024-01-29T23:55:29Z","2024-01-29T23:55:30Z","2024-01-29T23:55:31Z","2024-01-29T23:55:32Z","2024-01-29T23:55:33Z","2024-01-29T23:55:34Z","2024-01-29T23:55:35Z","2024-01-29T23:55:36Z","2024-01-29T23:55:37Z","2024-01-29T23:55:38Z","2024-01-29T23:55:39Z","2024-01-29T23:55:40Z","2024-01-29T23:55:41Z","2024-01-29T23:55:42Z","2024-01-29T23:55:43Z","2024-01-29T23:55:44Z","2024-01-29T23:55:45Z","2024-01-29T23:55:46Z","2024-01-29T23:55:47Z","2024-01-29T23:55:48Z","2024-01-29T23:55:49Z","2024-01-29T23:55:51Z","2024-01-29T23:55:52Z","2024-01-29T23:55:53Z","2024-01-29T23:55:54Z","2024-01-29T23:55:55Z","2024-01-29T23:55:56Z","2024-01-29T23:55:57Z","2024-01-29T23:55:58Z","2024-01-29T23:55:59Z","2024-01-29T23:56:00Z","2024-01-29T23:56:01Z","2024-01-29T23:56:02Z","2024-01-29T23:56:03Z","2024-01-29T23:56:04Z","2024-01-29T23:56:05Z","2024-01-29T23:56:06Z","2024-01-29T23:56:07Z","2024-01-29T23:56:08Z","2024-01-29T23:56:09Z","2024-01-29T23:56:10Z","2024-01-29T23:56:11Z","2024-01-29T23:56:12Z","2024-01-29T23:56:13Z","2024-01-29T23:56:14Z","2024-01-29T23:56:15Z","2024-01-29T23:56:16Z","2024-01-29T23:56:17Z","2024-01-29T23:56:18Z","2024-01-29T23:56:19Z","2024-01-29T23:56:20Z","2024-01-29T23:56:21Z","2024-01-29T23:56:22Z","2024-01-29T23:56:23Z","2024-01-29T23:56:24Z","2024-01-29T23:56:25Z","2024-01-29T23:56:26Z","2024-01-29T23:56:27Z","2024-01-29T23:56:28Z","2024-01-29T23:56:29Z","2024-01-29T23:56:30Z","2024-01-29T23:56:31Z","2024-01-29T23:56:32Z","2024-01-29T23:56:33Z","2024-01-29T23:56:34Z","2024-01-29T23:56:35Z","2024-01-29T23:56:36Z","2024-01-29T23:56:37Z","2024-01-29T23:56:38Z","2024-01-29T23:56:39Z","2024-01-29T23:56:40Z","2024-01-29T23:56:41Z","2024-01-29T23:56:42Z","2024-01-29T23:56:43Z","2024-01-29T23:56:44Z","2024-01-29T23:56:45Z","2024-01-29T23:56:46Z","2024-01-29T23:56:47Z","2024-01-29T23:56:48Z","2024-01-29T23:56:49Z","2024-01-29T23:56:50Z","2024-01-29T23:56:51Z","2024-01-29T23:56:52Z","2024-01-29T23:56:53Z","2024-01-29T23:56:54Z","2024-01-29T23:56:55Z","2024-01-29T23:56:56Z","2024-01-29T23:56:57Z","2024-01-29T23:56:58Z","2024-01-29T23:56:59Z","2024-01-29T23:57:00Z","2024-01-29T23:57:01Z","2024-01-29T23:57:02Z","2024-01-29T23:57:03Z","2024-01-29T23:57:04Z","2024-01-29T23:57:05Z","2024-01-29T23:57:06Z","2024-01-29T23:57:07Z","2024-01-29T23:57:08Z","2024-01-29T23:57:09Z","2024-01-29T23:57:10Z","2024-01-29T23:57:11Z","2024-01-29T23:57:12Z","2024-01-29T23:57:13Z","2024-01-29T23:57:14Z","2024-01-29T23:57:15Z","2024-01-29T23:57:16Z","2024-01-29T23:57:17Z","2024-01-29T23:57:18Z","2024-01-29T23:57:19Z","2024-01-29T23:57:20Z","2024-01-29T23:57:21Z","2024-01-29T23:57:22Z","2024-01-29T23:57:23Z","2024-01-29T23:57:24Z","2024-01-29T23:57:25Z","2024-01-29T23:57:26Z","2024-01-29T23:57:27Z","2024-01-29T23:57:28Z","2024-01-29T23:57:29Z","2024-01-29T23:57:30Z","2024-01-29T23:57:31Z","2024-01-29T23:57:32Z","2024-01-29T23:57:33Z","2024-01-29T23:57:34Z","2024-01-29T23:57:35Z","2024-01-29T23:57:36Z","2024-01-29T23:57:37Z","2024-01-29T23:57:38Z","2024-01-29T23:57:39Z","2024-01-29T23:57:40Z","2024-01-29T23:57:41Z","2024-01-29T23:57:42Z","2024-01-29T23:57:43Z","2024-01-29T23:57:44Z","2024-01-29T23:57:45Z","2024-01-29T23:57:46Z","2024-01-29T23:57:47Z","2024-01-29T23:57:48Z","2024-01-29T23:57:49Z","2024-01-29T23:57:50Z","2024-01-29T23:57:51Z","2024-01-29T23:57:52Z","2024-01-29T23:57:53Z","2024-01-29T23:57:54Z","2024-01-29T23:57:55Z","2024-01-29T23:57:57Z","2024-01-29T23:57:58Z","2024-01-29T23:57:59Z","2024-01-29T23:58:00Z","2024-01-29T23:58:01Z","2024-01-29T23:58:02Z","2024-01-29T23:58:03Z","2024-01-29T23:58:04Z","2024-01-29T23:58:05Z","2024-01-29T23:58:06Z","2024-01-29T23:58:07Z","2024-01-29T23:58:08Z","2024-01-29T23:58:09Z","2024-01-29T23:58:10Z","2024-01-29T23:58:11Z","2024-01-29T23:58:12Z","2024-01-29T23:58:13Z","2024-01-29T23:58:14Z","2024-01-29T23:58:15Z","2024-01-29T23:58:16Z","2024-01-29T23:58:17Z","2024-01-29T23:58:18Z","2024-01-29T23:58:19Z","2024-01-29T23:58:20Z","2024-01-29T23:58:21Z","2024-01-29T23:58:22Z","2024-01-29T23:58:23Z","2024-01-29T23:58:24Z","2024-01-29T23:58:25Z","2024-01-29T23:58:26Z","2024-01-29T23:58:27Z","2024-01-29T23:58:28Z","2024-01-29T23:58:29Z","2024-01-29T23:58:30Z","2024-01-29T23:58:31Z","2024-01-29T23:58:32Z","2024-01-29T23:58:33Z","2024-01-29T23:58:34Z","2024-01-29T23:58:35Z","2024-01-29T23:58:36Z","2024-01-29T23:58:37Z","2024-01-29T23:58:38Z","2024-01-29T23:58:39Z","2024-01-29T23:58:40Z","2024-01-29T23:58:41Z","2024-01-29T23:58:42Z","2024-01-29T23:58:43Z"],"fillOpacity":0.8},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLayersControl","args":[["OSM (default)","World Imagery"],"Data Points",{"collapsed":false,"autoZIndex":true,"position":"topright"}]}],"limits":{"lat":[39.181081,39.195192],"lng":[-96.58672300000001,-96.56613400000001]}},"evals":[],"jsHooks":[]}</script>
```


## Explore your movement data. For example, are there any unique features of your data (e.g., a large change in location)? Do your data contain location error? Really try to explore your data as best as possible using the plots/maps you made in 3.

*The data actually is pretty good. It just had some problems in the very beginning when I was inside the building and probably the carrier service was not as good.*


```r
time <- as_datetime(coords$time) - as_datetime(coords$time)[1]
ele <- data$ele

df <- data.frame(long = coords[,1], 
                 lat = coords[,2],
                 time = time,
                 ele = ele)

df %>% pivot_longer(cols = c(long,lat,ele)) %>% 
  ggplot()+
  geom_point(aes(time,value), size = 2, shape = 21, fill = 'steelblue', alpha = .8)+
  facet_wrap(~name, scales = 'free')
```

<img src="02-Activity_1_files/figure-html/unnamed-chunk-3-1.png" width="960" />

## Fit a statistical or machine learning model to your movement data. Obtain predictions of your location on a fine time scale so that the estimates resemble a continuous trajectory.


```r
# Fit models polynomial regression and random forest.

# Longitude
m1_long <- lm(long ~ poly(time,degree=10,raw=TRUE),data=df)
summary(m1_long)
m2_long <- randomForest(long ~ time, data = df)
summary(m2_long)

# Latitude
m1_lat <- lm(lat ~ poly(time,degree=10,raw=TRUE),data=df)
summary(m1_lat)
m2_lat <- randomForest(lat ~ time, data = df)
summary(m2_lat)

# Elevation
m1_ele <- lm(ele ~ poly(time,degree=10,raw=TRUE),data=df)
summary(m1_lat)
m2_ele <- randomForest(ele ~ time, data = df)
summary(m2_lat)
```

## Plot/map your estimated trajectory from 5. Explore your estimated trajectory as best as possible using the plots/maps. Note any unique features or shortcomings of your model.


```r
df.pred = data.frame(time = seq(0,as.integer(max(df$time))))

df.pred$long.m1.hat = predict(m1_long, newdata = df.pred)
df.pred$long.m2.hat = predict(m2_long, newdata = df.pred)

df.pred$lat.m1.hat = predict(m1_lat, newdata = df.pred)
df.pred$lat.m2.hat = predict(m2_lat, newdata = df.pred)

df.pred$ele.m1.hat = predict(m1_ele, newdata = df.pred)
df.pred$ele.m2.hat = predict(m2_ele, newdata = df.pred)

p1 <- ggplot()+
  geom_point(data = df, aes(time, long), size = 3)+
  geom_line(data = df.pred, aes(time, long.m1.hat), color = "gold", size = 1)+
  geom_line(data = df.pred, aes(time, long.m2.hat), color = "red4", size = 1)

p2 <- ggplot()+
  geom_point(data = df, aes(time, lat), size = 3)+
  geom_line(data = df.pred, aes(time, lat.m1.hat), color = "gold", size = 1)+
  geom_line(data = df.pred, aes(time, lat.m2.hat), color = "red4", size = 1)

p3 <- ggplot()+
  geom_point(data = df, aes(time, ele), size = 3)+
  geom_line(data = df.pred, aes(time, ele.m1.hat), color = "gold", size = 1)+
  geom_line(data = df.pred, aes(time, ele.m2.hat), color = "red4", size = 1)


ggarrange(p1, p2, p3, nrow = 1)
```

<img src="02-Activity_1_files/figure-html/unnamed-chunk-5-1.png" width="672" />

```r
# Create data frame for plotting
df.pred2 <- df.pred %>% pivot_longer(cols = c(long.m1.hat,long.m2.hat), values_to = 'longitude', names_to = 'model') %>% 
  pivot_longer(cols = c(lat.m1.hat, lat.m2.hat), values_to = 'latitude', names_to = 'model2') %>% 
  mutate(model = substr(model, 6,7),
         model2 = substr(model2, 5,6)) %>% 
  filter(model == model2) %>% dplyr::select(-c(ele.m1.hat, ele.m2.hat, model2))

# Visualize models

color_palette <- colorFactor(palette = "Set1", domain = df.pred2$model)

leaflet(df.pred2) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(
    ~longitude,
    ~latitude,
    fillColor = ~color_palette(model), 
    color = ~'black', # This will set the border color the same as the fill color
    radius = 3,
    weight = 1,
    fillOpacity = 0.8,
    stroke = TRUE, # Set to TRUE to have borders on the circles
    group = "Data Points"
  ) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "World Imagery"),
    overlayGroups = c("Data Points"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = color_palette,
    values = ~model,
    title = "Model",
    opacity = 1.0
  )
```


```{=html}
<div class="leaflet html-widget html-fill-item" id="htmlwidget-065151fdc8d5f1d45309" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-065151fdc8d5f1d45309">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,"OSM (default)",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addProviderTiles","args":["Esri.WorldImagery",null,"World Imagery",{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircleMarkers","args":[[39.19160972573604,39.19327104899997,39.19187561668809,39.19327295853331,39.19212212965346,39.19327295853331,39.19235017310437,39.19327295853331,39.19256062599188,39.19327295853331,39.1927543384381,39.19327295853331,39.19293213241677,39.19327295853331,39.19309480242201,39.19327505073334,39.1932431161255,39.19327550223333,39.19337781502236,39.19327550223333,39.19349961506566,39.19327548223334,39.19360920728991,39.19327536253334,39.19370725842339,39.19327536253334,39.1937944114899,39.19327536253334,39.19387128639944,39.19327561830001,39.19393848052876,39.19327571210001,39.19399656929098,39.19327504240001,39.19404610669533,39.19327042883335,39.19408762589648,39.19327042883335,39.19412163973382,39.19326661053339,39.19414864126102,39.19326948176671,39.19416910426554,39.19327029453338,39.19418348377872,39.19327819706672,39.19419221657617,39.19330622976671,39.1941957216689,39.19331687690001,39.19419440078501,39.1933178483,39.19418863884235,39.19332123416667,39.19417880441218,39.19332146150001,39.19416525017369,39.1933214695,39.19414831336,39.19332145750001,39.19412831619535,39.19331803273334,39.19410556632369,39.19330914310001,39.19408035722895,39.19330716486667,39.1940529686469,39.19328632586665,39.1940236669688,39.19327966416666,39.19399270563709,39.19327892119999,39.19396032553284,39.19327960423333,39.19392675535558,39.19328513166667,39.19389221199521,39.19329195526671,39.19385690089634,39.19329646306674,39.19382101641509,39.19330318013342,39.1937847421683,39.19331926096675,39.19374825137572,39.19333179383337,39.1937117071946,39.19333550770005,39.19367526304748,39.19334409076669,39.19363906294276,39.19334661710002,39.19360324178844,39.19335003823334,39.19356792569903,39.19335153486667,39.19353323229577,39.19335196920001,39.19349927100019,39.19335562056668,39.19346614332115,39.19335590976668,39.19343394313556,39.19337087056667,39.19340275696262,39.19337570693332,39.19337266423197,39.19337882953332,39.19334373754569,39.19337918646665,39.19331604293414,39.19338322989999,39.19328964010602,39.19338761310001,39.19326458269243,39.19337169973334,39.19324091848529,39.1933069042334,39.19321868967,39.1932662297334,39.19319793305262,39.19326827703339,39.19317868028133,39.19332527006663,39.19316095806276,39.19333989490003,39.19314478837281,39.19334017246668,39.19313018866234,39.19334052246668,39.19311717205753,39.19334670160003,39.19310574755547,39.19336707460009,39.19309592021445,39.19337525190011,39.19308769133951,39.19338307806675,39.19308105866319,39.19340118010007,39.19307601652145,39.1934143451,39.19307255602487,39.19342208243329,39.19307066522556,39.19342843469997,39.19307032927912,39.19343339003331,39.19307153060254,39.19344310589999,39.19307424902753,39.19345840723332,39.19307846194967,39.19347747156669,39.19308414447334,39.19349184583336,39.19309126955245,39.19350894296671,39.19309980812719,39.19351929930003,39.19310972925671,39.19352881636668,39.19312100024797,39.19355467303333,39.19313358678069,39.19356714009999,39.1931474530285,39.19358049743331,39.19316256177644,39.1935958060666,39.19317887453469,39.19362150316659,39.19319635164901,39.19363318563327,39.1932149524073,39.19365876999999,39.19323463514291,39.19367119793333,39.19325535733462,39.19369271016669,39.19327707570324,39.19370248843336,39.19329974630487,39.19370887686669,39.19332332462115,39.19372168459998,39.19334776564624,39.19373518683332,39.19337302397074,39.19376224636665,39.19339905386273,39.19377190069999,39.19342580934575,39.19377679696667,39.19345324427385,39.19378425896667,39.19348131240392,39.19379820616664,39.19350996746529,39.19380892489999,39.19353916322637,39.19381665519999,39.19356885355892,39.19383236559998,39.1935989924995,39.19384425356666,39.19362953430849,39.19385598190006,39.19366043352656,39.19386941700004,39.19369164502878,39.19387961466664,39.19372312407615,39.19388765926666,39.19375482636504,39.19389712346663,39.19378670807411,39.19392014663332,39.1938187259092,39.1939334028333,39.19385083714588,39.19394073213329,39.19388299966993,39.19395913293329,39.19391517201573,39.19396884779997,39.19394731340254,39.1939757386333,39.19397938376898,39.1939876668667,39.19401134380525,39.19400593840001,39.19404315498379,39.19402263503336,39.19407477958785,39.1940440692667,39.1941061807383,39.19405437870001,39.19413732241874,39.19406930870001,39.1941681694989,39.19408319883334,39.19419868775615,39.19410110269998,39.19422884389569,39.19411194763332,39.19425860556895,39.19413141959999,39.19428794139034,39.19415109683334,39.19431682095284,39.19416055926667,39.19434521484176,39.19416872926667,39.1943730946472,39.19418847756669,39.19440043297518,39.19420269829999,39.19442720345732,39.19422328323335,39.19445338075928,39.19423478123337,39.19447894058779,39.1942441836,39.19450385969663,39.19425269796669,39.19452811589122,39.19427025343335,39.19455168803216,39.19428662866662,39.19457455603755,39.19430604870001,39.19459670088427,39.19431657363338,39.19461810460813,39.19432145836672,39.19463875030313,39.19432363140003,39.19465862211945,39.19432297286668,39.19467770526073,39.19432335636667,39.19469598598032,39.19432084529997,39.19471345157661,39.1943200485333,39.19473009038744,39.19431998319999,39.19474589178374,39.19431939266664,39.19476084616241,39.19431758543335,39.19477494493822,39.19431703526669,39.19478818053515,39.1943167177333,39.1948005463769,39.19431677943329,39.19481203687675,39.19431829166665,39.19482264742668,39.19431853403331,39.1948323743859,39.19431695989993,39.19484121506878,39.19431645976658,39.19484916773214,39.19431641983324,39.19485623156193,39.19431680099998,39.19486240665956,39.19431801049995,39.19486769402746,39.19431829000002,39.1948720955543,39.19431840560001,39.19487561399978,39.19431923140002,39.19487825297878,39.19432448346683,39.19488001694535,39.19432691643355,39.19488091117604,39.19432740636687,39.19488094175311,39.19432984853341,39.19488011554706,39.19433627820004,39.19487844019923,39.19434732540005,39.19487592410376,39.19435146230001,39.19487257638934,39.19435292233334,39.1948684069008,39.19435618406671,39.19486342618032,39.19435711806673,39.19485764544844,39.19435876996673,39.1948510765849,39.19436594353339,39.19484373210922,39.1943691687667,39.19483562516114,39.19437088636668,39.19482676948089,39.19437158633333,39.19481717938933,39.19437614750006,39.19480686976792,39.19437853423339,39.19479585603862,39.19438197263334,39.19478415414372,39.19438369100001,39.19477178052551,39.19438430330002,39.19475875210595,39.19438724116672,39.19474508626632,39.19438996536672,39.19473080082673,39.19439170170005,39.1947159140257,39.19439356740001,39.19470044449982,39.19439552576664,39.19468441126315,39.1943965476666,39.19466783368689,39.19439858589993,39.19465073147903,39.19440558339993,39.194633124664,39.19441234726668,39.19461503356241,39.1944141905,39.19459647877084,39.19441420513333,39.19457748114178,39.19441286176669,39.19455806176356,39.1944117770667,39.19453824194061,39.1944053123,39.19451804317342,39.19438335450021,39.19449748713917,39.19435932140007,39.19447659567202,39.19434587969992,39.19445539074387,39.19432401319995,39.1944338944451,39.19431258179999,39.19441212896559,39.19430624296663,39.19439011657582,39.19430523046664,39.19436787960824,39.19430539326666,39.19434544043882,39.1943075840333,39.19432282146868,39.19431051516666,39.19430004510624,39.1943148954667,39.19427713374919,39.19431992570006,39.19425410976695,39.19432407196673,39.19423099548338,39.19433125643334,39.19420781315948,39.19435107739999,39.1941845849767,39.19436163000006,39.19416133302015,39.19436847223344,39.19413807926237,39.19438750570003,39.19411484554704,39.19439752376664,39.19409165357337,39.19439882466663,39.19406852488036,39.1943977259333,39.19404548083165,39.19439099926669,39.19402254260046,39.1943883976334,39.19399973115492,39.19438693789996,39.19397706724371,39.19438662966662,39.19395457138188,39.1943872418333,39.19393226383711,39.19438789270001,39.19391016461623,39.19438715019997,39.19388829345205,39.19438258760002,39.19386666979042,39.19437785976664,39.19384531277783,39.19437625193331,39.19382424124907,39.19437638559997,39.19380347371538,39.19438161876653,39.19378302835292,39.19439039320001,39.19376292299144,39.19439124400019,39.19374317510346,39.19439102986691,39.19372380179361,39.19439092733356,39.19370481978851,39.19439032026683,39.19368624542675,39.19438873993332,39.19366809464932,39.19438814069991,39.1936503829905,39.19438804786657,39.19363312556889,39.19438804386657,39.19361633707877,39.19438800906657,39.19360003178214,39.19438798186657,39.19358422350058,39.19438798886657,39.19356892560792,39.19438714889989,39.193554151023,39.19438193243317,39.19353991220279,39.19439337643331,39.19352622113598,39.19439747093335,39.19351308933671,39.19440138396668,39.19350052783891,39.19440013183333,39.19348854719069,39.19438018683327,39.19347715744932,39.19437607759989,39.19346636817631,39.19438421313333,39.19345618843313,39.19438643919997,39.19344662677694,39.19438657673328,39.19343769125687,39.19438605499992,39.19342938941062,39.19438468253331,39.19342172826125,39.19438568943329,39.19341471431451,39.19439211223332,39.19340835355631,39.19439425723335,39.1934026514506,39.19439493090003,39.19339761293759,39.1943949872667,39.19339324243234,39.19439413320002,39.19338954382348,39.19439298863337,39.19338652047244,39.1943859916667,39.19338417521293,39.19436932076672,39.19338251035074,39.19435792580004,39.19338152766383,39.19433636853331,39.19338122840273,39.19427926600001,39.1933816132913,39.19422825886667,39.19338268252769,39.19415771169992,39.19338443578567,39.1940665543333,39.19338687221631,39.19399588149997,39.19338999044978,39.19394987376666,39.19339378859758,39.19388347596666,39.19339826425508,39.19380289099995,39.1934034145042,39.19371505616659,39.19340923591648,39.19363096243329,39.19341572455636,39.19357914456669,39.19342287598483,39.19352295623343,39.19343068526317,39.19346141986675,39.1934391469572,39.19340290510001,39.19344825514149,39.19332904099998,39.19345800340411,39.19325549666671,39.19346838485145,39.19317662493341,39.19347939211333,39.19309755676671,39.19349101734835,39.19303977853324,39.19350325224954,39.19299097826678,39.19351608805015,39.19296487656671,39.19352951552965,39.19296131286664,39.19354352502017,39.19296192186665,39.19355810641283,39.19296888926672,39.19357324916455,39.19297301326672,39.1935889423049,39.19297750073336,39.19360517444337,39.1929805894,39.19362193377651,39.19298591943335,39.19363920809558,39.19299294906669,39.19365698479426,39.19300478933339,39.19367525087645,39.19301318080002,39.1936939929645,39.1930168205,39.19371319730734,39.19301933193333,39.19373284978895,39.19302090723332,39.19375293593697,39.19302845879994,39.19377344093146,39.19303563089996,39.1937943496137,39.19305310163335,39.19381564649545,39.19307072380008,39.19383731576796,39.19309142986675,39.1938593413114,39.19311487619996,39.19388170670435,39.19313279303329,39.19390439523338,39.19316015906666,39.1939273899028,39.19319022563334,39.19395067344449,39.19321533609999,39.19397422832788,39.19324288346664,39.19399803677006,39.19327782316661,39.19402208074586,39.19330802113328,39.19404634199827,39.19334618916665,39.19407080204864,39.19338460736667,39.19409544220733,39.19341661553334,39.19412024358406,39.19346131106668,39.19414518709868,39.19349797410003,39.1941702534918,39.19354581606667,39.19419542333549,39.19358418630004,39.19422067704421,39.19363485423334,39.19424599488559,39.19369218239999,39.19427135699143,39.19372645360001,39.19429674336859,39.19376780610003,39.19432213391008,39.19381124160002,39.19434750840611,39.19385268296669,39.19437284655505,39.19390392150004,39.19439812797466,39.19395050813338,39.19442333221316,39.19400251603334,39.1944484387604,39.19404380740004,39.19447342705896,39.19409237446668,39.19449827651533,39.19413588143335,39.19452296651102,39.19417829439996,39.19454747641384,39.19421112949996,39.19457178558881,39.19424887766665,39.19459587340945,39.1942760372667,39.19461971926889,39.19430425800002,39.19464330259083,39.19432217999999,39.19466660284063,39.19434066606667,39.19468959953637,39.1943460147667,39.19471227225974,39.1943553868334,39.19473460066698,39.19436580803334,39.19475656449982,39.1943734857333,39.19477814359617,39.19438402439998,39.19479931790107,39.1943932582333,39.19482006747713,39.19440246489999,39.19484037251541,39.1944139866333,39.19486021334579,39.19442033393332,39.19487957044755,39.19443162713331,39.1948984244597,39.19443762273328,39.19491675619136,39.19444526066674,39.19493454663188,39.19445025483327,39.19495177696109,39.19445033513339,39.19496842855916,39.19445011740012,39.19498448301665,39.19445011016678,39.1949999221443,39.19444994146676,39.19501472798263,39.19445025620011,39.19502888281171,39.19445309500011,39.19504236916038,39.19445792233338,39.1950551698158,39.1944771199333,39.19506726783256,39.19448952699999,39.19507864654177,39.19450594610007,39.19508928955997,39.1945352720667,39.19509918079804,39.19457349050001,39.1951083044697,39.19460195036668,39.19511664510019,39.19463345350002,39.19512418753452,39.19466089960001,39.19513091694567,39.19468381179997,39.19513681884272,39.19471944813328,39.19514187907869,39.1947766877333,39.19514608385827,39.19482665086669,39.1951494197454,39.19485348770002,39.19515187367061,39.19488333786666,39.19515343293827,39.19491666399997,39.19515408523365,39.19494152836663,39.1951538186297,39.19496549263331,39.19515262159383,39.19500414246667,39.19515048299425,39.19504138806662,39.19514739210651,39.19506186043329,39.19514333861937,39.19507831559994,39.19513831264076,39.1950979773333,39.19513230470368,39.19512156453332,39.19512530577152,39.19513780740004,39.19511730724345,39.19514861750004,39.19510830095955,39.19515960426667,39.19509827920579,39.1951658128333,39.19508723471854,39.19517313386667,39.19507516068929,39.19517925130003,39.19506205076876,39.19518387926669,39.19504789907107,39.19518643023338,39.19503270017755,39.19518799170001,39.1950164491404,39.19518807510001,39.19499914148608,39.19518761223335,39.19498077321854,39.19518394979993,39.19496134082215,39.1951817483999,39.19494084126443,39.19517790956662,39.19491927199874,39.19517131529996,39.19489663096633,39.19516609506667,39.19487291659857,39.195160382,39.19484812781879,39.19515658103335,39.19482226404381,39.19515516170003,39.19479532518532,39.19515325086674,39.19476731165119,39.19515228226675,39.19473822434617,39.19515178563341,39.19470806467264,39.195149909,39.19467683453116,39.1951486604333,39.19464453632058,39.19514678613323,39.19461117293798,39.19514329583343,39.19457674777851,39.19514212733353,39.19454126473487,39.1951420160002,39.19450472819647,39.1951420000002,39.19446714304866,39.1951420000002,39.19442851467138,39.1951420000002,39.19438884893768,39.1951420000002,39.19434815221224,39.1951420000002,39.19430643134923,39.1951420000002,39.19426369369027,39.1951420000002,39.19421994706209,39.1951420000002,39.19417519977378,39.1951420000002,39.19412946061406,39.1951420000002,39.19408273884811,39.1951420000002,39.1940350442143,39.1951419960002,39.19398638692054,39.19514198400019,39.19393677764065,39.19514175326682,39.19388622751016,39.19513953613318,39.1938347481222,39.1951373832664,39.19378235152294,39.1951287117666,39.19372905020697,39.1951075332001,39.19367485711226,39.19505850253335,39.19361978561512,39.1949782771,39.1935638495247,39.19490325256666,39.19350706307746,39.19474945133334,39.19344944093141,39.19462240986667,39.1933909981599,39.19452639350001,39.19333175024552,39.19444043750003,39.1932717130736,39.19436344603337,39.19321090292542,39.19421266273335,39.19314933647146,39.19402261883334,39.19308703076418,39.19389066813336,39.19302400323081,39.19378896676668,39.19296027166579,39.19369592676667,39.192895854223,39.19359381666668,39.19283076940793,39.19348375593334,39.1927650360695,39.19338223946668,39.19269867339186,39.19324042153334,39.19263170088576,39.19314072150001,39.19256413837992,39.19306123646666,39.19249600601224,39.19299454733331,39.19242732422054,39.1929344219,39.19235811373351,39.19284822073334,39.19228839556132,39.19272672239995,39.19221819098588,39.19257130326664,39.19214752155114,39.19243423916665,39.19207640905319,39.1923349922,39.19200487553027,39.19225561446665,39.19193294325227,39.19215824996662,39.19186063471066,39.19204702006665,39.19178797260755,39.19192920719998,39.19171497984539,39.19181537383333,39.19164167951574,39.1916965166,39.19156809488862,39.19158422179999,39.19149424940121,39.19148200086666,39.1914201666464,39.1913855555,39.19134587036185,39.19129188760002,39.19127138441797,39.19119141733339,39.19119673280678,39.19108285656669,39.19112193962972,39.19096464653335,39.19104702908601,39.19085505160002,39.1909720254606,39.19077883803332,39.19089695311217,39.19067737383332,39.19082183646091,39.19059963613334,39.19074669997617,39.19049217763337,39.19067156816411,39.19040243206668,39.19059646555547,39.19029860323336,39.19052141669258,39.1902125627,39.19044644611746,39.19004993836668,39.19037157835828,39.18996033526669,39.19029683791745,39.18990491583337,39.19022224925816,39.18982871286671,39.19014783679196,39.18971318200001,39.19007362486563,39.18958649813337,39.18999963774836,39.18947854806668,39.18992589961864,39.18940121296668,39.18985243455128,39.1893305539667,39.18977926650442,39.18925210783338,39.18970641930638,39.18914675983334,39.18963391664256,39.18902204063333,39.18956178204245,39.18891940019999,39.1894900388664,39.18883049843334,39.18941871029248,39.18875161223335,39.18934781930344,39.18866384433331,39.18927738867358,39.18856812213329,39.18920744095578,39.18845997186661,39.1891379984684,39.18837652273329,39.18906908328186,39.18828995503331,39.18900071720638,39.18821677593336,39.18893292177832,39.1881595726,39.18886571824783,39.18810247543331,39.18879912756555,39.18804442486666,39.18873317036989,39.18796021880004,39.1886678669746,39.18787125730005,39.18860323735583,39.18781829966672,39.18853930113947,39.18777188143336,39.18847607758886,39.18772844746668,39.18841358559234,39.1877137475,39.18835184365081,39.18770762320001,39.18829086986535,39.18769859683334,39.1882306819258,39.18768482046666,39.18817129709779,39.18766539709996,39.18811273221153,39.18765371909996,39.18805500365003,39.18764090656661,39.18799812733704,39.18760938879993,39.18794211872606,39.18756284456665,39.18788699278861,39.18748287600001,39.18783276400357,39.18739246330001,39.18777944634539,39.18730587193335,39.18772705327404,39.18724482733336,39.18767559772386,39.18717868380004,39.18762509209316,39.18712055516669,39.18757554823384,39.18703420853333,39.18752697744132,39.18693785203328,39.18747939044491,39.18687392906661,39.18743279739746,39.18681780830011,39.18738720786652,39.18677738473338,39.18734263082425,39.18676171216666,39.18729907463948,39.1867594551666,39.18725654706777,39.18676039636664,39.18721505524343,39.18676181226665,39.18717460567107,39.18676163089999,39.18713520421745,39.18675844076659,39.18709685610351,39.18674735889993,39.18705956589719,39.1867382211999,39.18702333750567,39.18673619693323,39.18698817416859,39.18673424756668,39.18695407845148,39.18673314699996,39.1869210522387,39.18673306956664,39.18688909672798,39.18673312219997,39.18685821242421,39.18673443523323,39.18682839913396,39.18673488153327,39.18679965596033,39.18673676086664,39.18677198129793,39.18673895886671,39.18674537282838,39.18674023753328,39.18671982751589,39.18674031359993,39.18669534160402,39.18674002643327,39.18667191061116,39.18673908563339,39.18664952932817,39.18673904993339,39.18662819181495,39.18673836543341,39.18660789139855,39.18673740426672,39.18658862067052,39.18673460106672,39.18657037148574,39.18673348483336,39.18655313496078,39.18673210476663,39.18653690147255,39.18673075699998,39.18652166065891,39.18673027059997,39.18650740141751,39.18672719153319,39.18649411190628,39.18672480679976,39.18648177954454,39.18672183483308,39.18647039101391,39.1867210655331,39.18645993225928,39.18672094386643,39.18645038849154,39.18672086506644,39.18644174419006,39.18672028163329,39.18643398310455,39.18672007003336,39.18642708825951,39.18672004510004,39.18642104195708,39.1867199700667,39.18641582578203,39.18671928746666,39.18641142060574,39.18671902376663,39.1864078065918,39.18671897203331,39.18640496320139,39.18671887006668,39.18640286919955,39.18671828260026,39.18640150266155,39.18671804816703,39.18640084098018,39.18671800466704,39.18640086087307,39.18671793050036,39.18640153839108,39.1867173244669,39.18640284892654,39.18671707176685,39.18640476722278,39.18671699870016,39.18640726738253,39.18671696270017,39.18641032287974,39.18671689796683,39.18641390656888,39.18671625013345,39.18641799069655,39.18671601996677,39.18642254691322,39.18671598620011,39.18642754628532,39.18671591303342,39.18643295930777,39.18671529066659,39.18643875591786,39.1867150375332,39.18644490550786,39.1867150134332,39.18645137694108,39.18671487459986,39.18645813856514,39.18671428583316,39.18646515822846,39.1867140182998,39.18647240329614,39.18671388059978,39.18647984066607,39.186713256633,39.18648743678621,39.18671301086629,39.18649515767251,39.18671297326629,39.18650296892729,39.18671289596631,39.18651083575766,39.18671221553326,39.18651872299488,39.18671200226668,39.18652659511473,39.18671196403336,39.18653441625776,39.18671188160001,39.18654215025057,39.18671129619997,39.18654976062741,39.18671111503328,39.18655721065225,39.18671112973328,39.18656446334177,39.18671134649993,39.18657148148903,39.18671290406652,39.18657822768722,39.18671328086647,39.1865846643541,39.18671333809981,39.18659075375785,39.18671311713317,39.18659645804151,39.18671289209992,39.18660173925092,39.18671316339992,39.18660655936044,39.18671599380005,39.18661088030082,39.18672134323332,39.18661466398719,39.18672495296669,39.18661787234844,39.18672518016667,39.18662046735501,39.18672451029997,39.18662241105028,39.18672157419999,39.18662366557989,39.18672011836667,39.18662419322295,39.18671842146674,39.1866239564239,39.18671678813334,39.18662291782491,39.18671580489995,39.18662104029687,39.18671460419989,39.18661828697491,39.1867109013,39.18661462129133,39.18670964829999,39.18661000700932,39.1867095069,39.18660440825836,39.18670983150012,39.18659778957104,39.18670997333349,39.18659011591644,39.18671007460015,39.18658135273982,39.18671103456671,39.18657146599668,39.18671123570001,39.18656042219333,39.18671075286669,39.18654818842386,39.18670832180004,39.18653473240821,39.18670700810014,39.18652002253277,39.18670587130013,39.18650402788967,39.18670538493343,39.18648671831724,39.18670522486678,39.186468064441,39.18670408066669,39.18644803771476,39.18670204833336,39.18642661046312,39.18670010570006,39.18640375592285,39.1866977799,39.18637944828651,39.18669062496657,39.18635366274689,39.18667753159996,39.18632637553795,39.18667190063326,39.18629756398218,39.18667008826661,39.18626720653349,39.1866663234668,39.18623528282331,39.18666593246672,39.18620177370656,39.18666605500002,39.18616666130618,39.18666588996667,39.18612992906186,39.18666534143332,39.18609156177479,39.18666282423331,39.18605154565734,39.18664057300001,39.1860098683791,39.18658541263336,39.18596651911565,39.18651903120005,39.18592148859721,39.18640424576668,39.18587476915775,39.18632101386666,39.18582635478326,39.18626007766667,39.18577624116312,39.18618354500001,39.18572442573838,39.18608171076666,39.18567090775237,39.18592088096667,39.18561568830314,39.18577588830004,39.18555877039066,39.18568026103339,39.18550015897252,39.18558547646668,39.18543986101199,39.18548438996665,39.18537788553039,39.18538496179997,39.18531424366129,39.18526638053329,39.18524894869784,39.18511915743331,39.18518201615159,39.18497279710002,39.18511346379964,39.18487501103333,39.18504331174017,39.18479445526669,39.18497158244503,39.18470182139998,39.18489830081246,39.18462025883331,39.18482349422025,39.18448119466667,39.18474719257925,39.18428208373337,39.18466942838727,39.18414415506665,39.18459023678237,39.18406781659997,39.18450965559587,39.18398448929998,39.18442772540728,39.18387529439999,39.18434448959619,39.18376357430002,39.18425999439789,39.18364437216667,39.18417428895667,39.18352292013333,39.18408742537768,39.18338625163332,39.183999458782,39.18326439203338,39.18391044736069,39.18315815923338,39.18382045242781,39.18305922273332,39.18372953847283,39.18295171609996,39.18363777321441,39.1828429631333,39.18354522765424,39.18273599906664,39.1834519761281,39.18263650416667,39.18335809636065,39.18251925180004,39.18326366951494,39.18236921016672,39.18316878024805,39.18225657603336,39.18307351676029,39.1821650570667,39.18297797084733,39.18209368370002,39.18288223795284,39.1820176116667,39.18278641721695,39.18195527273333,39.18269061152851,39.18186286853328,39.18259492757447,39.18179551833327,39.18249947588939,39.18175434873327,39.18240437090425,39.1817350459,39.18230973099686,39.18172139703336,39.18221567853817,39.18171295803337,39.1821223399395,39.18169107316668,39.18202984570117,39.18167031070004,39.18193833045667,39.18165713713339,39.18184793302206,39.18164806986672,39.18175879643518,39.18164000766667,39.18167106800644,39.1816337227,39.18158489935725,39.18162512566668,39.1815004464637,39.18161502756667,39.18141786970149,39.18160380146665,39.18133733388458,39.18157331973326,39.18125900830439,39.18153929889996,39.18118306677209,39.18152352210002,39.18110968765285,39.18152096796668,39.18103905390711,39.18152182136664,39.18097135312493,39.18152181506664,39.18090677756086,39.18151775900006,39.18084552416906,39.18150902683336,39.18078779463573,39.18149692516668,39.18073379541036,39.18146531400003,39.18068373773676,39.18143348773331,39.18063783768329,39.18139178203334,39.18059631616765,39.18133794613336,39.18055939898804,39.18130096620002,39.18052731684307,39.18127647043334,39.18050030536028,39.18123930003338,39.18047860511646,39.18119222103347,39.18046246165662,39.18115910003328,39.18045212551647,39.18113866653328,39.18044785223753,39.18113561189992,39.18044990238224,39.18113513023326,39.18045854155287,39.18113320536667,39.18047404039623,39.18113033596672,39.18049667462418,39.18112918080007,39.18052672501105,39.18112457440002,39.18056447740901,39.18111997210001,39.18061022275037,39.18111649186667,39.18066425704959,39.18110591620001,39.18072688140413,39.18110182613334,39.18079840199297,39.18109989856669,39.18087913007605,39.18109836206675,39.18096938198391,39.18109746916674,39.18106947911224,39.18109484043325,39.18117974791133,39.18109370259987,39.18130051987569,39.18109332113321,39.18143213152146,39.18109325479988,39.18157492437899,39.18109303756655,39.1817292449638,39.18109098476666,39.18189544475916,39.18108867096669,39.18207388018718,39.18108777783336,39.18226491258494,39.18109015076657,39.18246890816673,39.18109053299988],[-96.58440502773641,-96.58442696303339,-96.58439235504227,-96.58442704303341,-96.58438327675877,-96.58442704303341,-96.58437755864844,-96.58442704303341,-96.58437497563101,-96.58442704303341,-96.58437531154433,-96.58442704303341,-96.58437835890985,-96.58442704303341,-96.58438391870224,-96.58457138486668,-96.58439180012326,-96.58463273080005,-96.5844018203798,-96.58463273080005,-96.5844138044658,-96.58465665386669,-96.58442758494853,-96.5846605207,-96.58444300175859,-96.5846605207,-96.58445990198398,-96.58466212230002,-96.58447813966808,-96.58466986523327,-96.58449757561134,-96.58467158423326,-96.58451807717685,-96.58467347793324,-96.58453951809967,-96.58467557373324,-96.58456177829972,-96.58467557373324,-96.58458474369843,-96.5846772152332,-96.58460830603896,-96.5846830013332,-96.58463236271004,-96.5846850978332,-96.58465681657304,-96.58469721549994,-96.58468157579276,-96.58475424073342,-96.58470655367159,-96.58477281713338,-96.58473166848681,-96.58477628173338,-96.58475684333159,-96.58478107370006,-96.58478200595883,-96.58478396570005,-96.58480708862851,-96.58478414530006,-96.58483202795821,-96.58478471190003,-96.58485676477649,-96.58478248646666,-96.58488124397981,-96.58476953273322,-96.5849054143921,-96.58476601539988,-96.58492922862736,-96.58473339036649,-96.58495264295578,-96.58472386916652,-96.58497561717185,-96.58472122833317,-96.58499811446622,-96.58472294603321,-96.58502010129973,-96.58473329973327,-96.58504154728087,-96.58474595596664,-96.58506242504546,-96.58475134176663,-96.58508271013909,-96.5847562877666,-96.58510238090285,-96.58476536573328,-96.58512141836086,-96.58476895783332,-96.585139806111,-96.58478613589995,-96.58515753021784,-96.58480993060004,-96.58517457910843,-96.5848185231001,-96.58519094347015,-96.58488484696683,-96.5852066161515,-96.58490202786685,-96.58522159206483,-96.58490822276684,-96.58523586809179,-96.58494732683353,-96.58524944299097,-96.58495375243352,-96.58526231730788,-96.58519816893332,-96.5852744932871,-96.58526168989995,-96.58528597478671,-96.58531098669992,-96.58529676719513,-96.58531389633326,-96.58530687734971,-96.58531572333327,-96.58531631345784,-96.58537967613323,-96.5853250850199,-96.5855025478,-96.58533320275427,-96.58559902419984,-96.58534067852464,-96.58562084889984,-96.58534752526903,-96.58561509089982,-96.58535375693064,-96.58550380063312,-96.58535938839134,-96.58547182733308,-96.58536443540598,-96.58548482763301,-96.5853689145396,-96.58550177183338,-96.58537284310586,-96.58551077786662,-96.58537623910726,-96.5855127530666,-96.58537912117761,-96.58551305153321,-96.58538150852574,-96.58551302966654,-96.58538342088107,-96.58551122943319,-96.58538487844109,-96.58550516800008,-96.5853859018202,-96.58548561486674,-96.58538651200018,-96.58547375316658,-96.58538673028249,-96.58546222349996,-96.58538657824201,-96.5854491319666,-96.58538607768219,-96.58544138370003,-96.5853852505919,-96.58543590086668,-96.58538411910365,-96.58543305766669,-96.58538270545334,-96.58542468723351,-96.58538103194138,-96.5854171109668,-96.58537912089515,-96.58541113930009,-96.58537699463309,-96.58539577353332,-96.58537467542982,-96.58538609673334,-96.58537218548273,-96.58537834396682,-96.5853695468799,-96.58537202573345,-96.58536678156929,-96.58536372836699,-96.58536391132904,-96.58535861276697,-96.58536095773921,-96.58533495243383,-96.58535794215449,-96.58531489083352,-96.58535488567829,-96.5853105789668,-96.58535180913783,-96.58531110276675,-96.58534873306039,-96.58531253996665,-96.58534567765085,-96.5853127472332,-96.58534266276982,-96.58530886556676,-96.58533970791362,-96.58530753620011,-96.58533683219436,-96.58530676270013,-96.58533405432179,-96.58530545033334,-96.58533139258584,-96.58530810636687,-96.58532886483999,-96.58530873743358,-96.58532648848596,-96.58530485169997,-96.58532428045892,-96.58530477593341,-96.58532225721392,-96.58530524373344,-96.58532043471303,-96.58530940940021,-96.58531882841345,-96.58531868843296,-96.58531745325632,-96.58532863753349,-96.58531632365664,-96.58533434483374,-96.58531545349356,-96.58533841660011,-96.58531485610197,-96.58533897629999,-96.58531454426441,-96.58533785056659,-96.58531453020387,-96.5853373964999,-96.58531482557771,-96.58533635689983,-96.58531544147148,-96.58532973793336,-96.58531638839426,-96.58532707123344,-96.58531767627413,-96.58532679986675,-96.58531931445459,-96.58532634403329,-96.58532131169144,-96.58532616793327,-96.58532367615041,-96.5853230871663,-96.58532641540536,-96.58532028293284,-96.58532953643704,-96.58531056886677,-96.58533304563245,-96.58530508703342,-96.58533694878473,-96.58530439510012,-96.58534125109365,-96.58530443743349,-96.5853459571666,-96.58530147389995,-96.58535107102007,-96.58530057956658,-96.58535659608164,-96.58530045693327,-96.58536253519237,-96.58530012689997,-96.58536889060993,-96.58529813483338,-96.58537566401172,-96.58529726876669,-96.585382856499,-96.58529700873324,-96.58539046860076,-96.58529736999989,-96.58539850027879,-96.58530111513328,-96.58540695093238,-96.58530248339996,-96.58541581940393,-96.58530155869988,-96.58542510398462,-96.58529929269982,-96.58543480242074,-96.58529860923309,-96.58544491191991,-96.58530153046635,-96.5854554291582,-96.58531742306617,-96.58546635028704,-96.58535008419989,-96.58547767094066,-96.58538828950012,-96.58548938624395,-96.5854060801001,-96.58550149082039,-96.58542516606674,-96.58551397880025,-96.58546189670004,-96.58552684382923,-96.58548971466671,-96.58554007907713,-96.58550957176658,-96.58555367724696,-96.58553239343324,-96.58556763058404,-96.58555308296656,-96.58558193088545,-96.58556639669996,-96.5855965695098,-96.58559548183337,-96.58561153738692,-96.58561204936672,-96.58562682502783,-96.58563080733329,-96.58564242253503,-96.58564722143325,-96.58565831961272,-96.58566700483325,-96.5856745055774,-96.58568985999997,-96.58569096936827,-96.58570987110006,-96.58570769955828,-96.58573478086679,-96.58572468436469,-96.58575637216677,-96.58574191166024,-96.58577822203341,-96.58575936898417,-96.5857931354,-96.58577704355338,-96.58581326196671,-96.58579492227375,-96.58584433186655,-96.58581299175138,-96.58585840953324,-96.5858312383041,-96.58586877953336,-96.58584964797278,-96.58588202856672,-96.58586820653305,-96.58589900193341,-96.58588689950666,-96.58592476170017,-96.58590571217329,-96.58594102563343,-96.58592462958187,-96.58595038970012,-96.58594363656255,-96.58597515426651,-96.58596271773823,-96.58599008399993,-96.58598185753613,-96.58599911313344,-96.58600104019966,-96.58600838266673,-96.58602024979993,-96.58603086640014,-96.58603947024737,-96.58604246866676,-96.58605868530366,-96.58606563310001,-96.58607787859287,-96.58607584973333,-96.58609703361353,-96.58610274296669,-96.58611613374974,-96.58611682079997,-96.58613516228301,-96.58613522259996,-96.58615410240343,-96.58614866593339,-96.58617293722128,-96.58617249806663,-96.58619164977823,-96.58618605039995,-96.58621022305864,-96.58620561800008,-96.58622864000083,-96.58621682959999,-96.58624688350801,-96.58624813843339,-96.58626493645967,-96.58626547940007,-96.58628278172201,-96.58627558203354,-96.58630040215938,-96.58628340100009,-96.58631778064463,-96.58630604293343,-96.58633490007001,-96.58632023406678,-96.5863517433576,-96.58633408870007,-96.58636829346987,-96.5863529528667,-96.58638453341997,-96.58636264820012,-96.586400446282,-96.58638297826674,-96.58641601520115,-96.58639846783335,-96.58643122340359,-96.58642791899997,-96.58644605420642,-96.58644147279998,-96.58646049102744,-96.58646285330006,-96.58647451739462,-96.58647546486667,-96.58648811695575,-96.58648870723324,-96.58650127348753,-96.58649894729986,-96.58651397090509,-96.58651209409986,-96.58652619327061,-96.58653718559989,-96.58653792480258,-96.5865549758,-96.58654914988435,-96.58656905356669,-96.58655985307286,-96.58658523926668,-96.58657001910674,-96.58660620290001,-96.58657963291506,-96.58662874663342,-96.58658867962511,-96.58663320483339,-96.58659714457042,-96.58663395993331,-96.58660501329855,-96.5866333204665,-96.58661227157884,-96.5866356521,-96.58661890540965,-96.58664002816683,-96.58662490102576,-96.5866602909002,-96.58663024490544,-96.58667796043351,-96.58663492377745,-96.58668319043345,-96.58663892462775,-96.58668495903348,-96.58664223470599,-96.58668751540013,-96.58664484153221,-96.58670062866727,-96.58664673290264,-96.58671313103345,-96.58664789689615,-96.58671669849981,-96.58664832187985,-96.58672031273312,-96.58664799651494,-96.58672080223315,-96.58664690976208,-96.5867182314998,-96.5866450508868,-96.58672014886659,-96.58664240946459,-96.58672063796662,-96.58663897538585,-96.58671901403306,-96.58663473886071,-96.58671902679987,-96.58662969042349,-96.58672010093328,-96.58662382093725,-96.58672031929993,-96.58661712159784,-96.58672019689989,-96.58660958393796,-96.58671873019946,-96.58660119983111,-96.58671817883261,-96.586591961495,-96.58671809726594,-96.58658186149536,-96.58671798666596,-96.58657089274872,-96.58671725256603,-96.58655904852594,-96.58671128820021,-96.58654632245488,-96.58670916600053,-96.58653270852321,-96.58670873120056,-96.58651820108081,-96.58670575486722,-96.5865027948421,-96.58668388776707,-96.58648648488854,-96.58666437173353,-96.58646926667002,-96.58665410196669,-96.58645113600713,-96.58663261339987,-96.58643208909253,-96.5865685421666,-96.58641212249235,-96.58650551926658,-96.58639123314758,-96.5864428225667,-96.58636941837507,-96.58633871376675,-96.58634667586816,-96.58625480303354,-96.5863230036978,-96.58621372006692,-96.58629840031267,-96.5861623817336,-96.58627286453975,-96.58610849080024,-96.58624639558454,-96.58606667510016,-96.58621899303067,-96.58598418763337,-96.58619065684022,-96.58586632623339,-96.58616138735293,-96.58579798146677,-96.58613118528598,-96.58575720113346,-96.58610005173307,-96.58571541016676,-96.58606798816361,-96.58564424906675,-96.58603499642182,-96.58559524163343,-96.58600107872532,-96.58556454283347,-96.58596623766387,-96.58554761116672,-96.58593047619787,-96.58551912230006,-96.58589379765661,-96.58550708320011,-96.58585620573648,-96.58550472843342,-96.58581770449887,-96.58550373530007,-96.58577829836804,-96.58550006150006,-96.58573799212891,-96.58548945646734,-96.5856967909245,-96.5854814862673,-96.58565470025323,-96.58547779200019,-96.58561172596642,-96.58547715190012,-96.58556787426502,-96.58547735443348,-96.58552315169685,-96.58547845906689,-96.58547756515325,-96.58547945986689,-96.58543112186587,-96.5854788299002,-96.58538382940293,-96.58547698463349,-96.58533569566592,-96.58547339679983,-96.58528672888571,-96.58546957379977,-96.58523693761862,-96.58546836579974,-96.58518633074242,-96.58546702683314,-96.58513491745244,-96.58546398746662,-96.58508270725689,-96.58545434543348,-96.58502970997283,-96.58541423119991,-96.5849759357215,-96.58535649496658,-96.58492139492368,-96.58526784546667,-96.58486609829508,-96.58519094276681,-96.58481005684141,-96.58511775666682,-96.58475328185331,-96.5850475921001,-96.5846957849015,-96.58490536646683,-96.58463757783151,-96.58479920593331,-96.58457867275827,-96.58471814843332,-96.58451908206088,-96.58464094483332,-96.58445881837719,-96.5845347128667,-96.58439789459793,-96.58446589463334,-96.58433632386142,-96.58442972703332,-96.58427411954746,-96.58438336196664,-96.58421129527174,-96.58436199346664,-96.58414786487975,-96.58432167913323,-96.5840838424408,-96.58425804753324,-96.58401924224202,-96.58419652869996,-96.58395407878191,-96.58410334966653,-96.58388836676448,-96.58401155256649,-96.58382212109254,-96.58392227613332,-96.58375535686152,-96.58384938436676,-96.58368808935295,-96.58377305980007,-96.58362033402797,-96.58369994696675,-96.58355210652063,-96.58362000230008,-96.58348342263139,-96.58353739850013,-96.58341429832022,-96.58347310006674,-96.58334474969996,-96.58339791540004,-96.58327479302957,-96.58329521223338,-96.58320444470711,-96.58319011460019,-96.5831337212629,-96.58308870900018,-96.58306263935252,-96.58299770720014,-96.58299121575,-96.58291553246674,-96.58291946734047,-96.58282592493332,-96.58284741111338,-96.58272715106662,-96.5827750641552,-96.58264668143332,-96.58270244364242,-96.58254678023339,-96.58262956683433,-96.58243431440006,-96.58255645106583,-96.58230170046669,-96.58248311374025,-96.58218403143334,-96.58240957232212,-96.58209217193343,-96.58233584432985,-96.58199051626671,-96.58226194732866,-96.58189017323338,-96.5821878989231,-96.58179405140001,-96.58211371674993,-96.58169446326671,-96.58203941847077,-96.58158964869992,-96.58196502176477,-96.58147979759985,-96.58189054432141,-96.58136310846665,-96.58181600383321,-96.58128042086653,-96.5817414179884,-96.58120874799994,-96.58166680446372,-96.58114769813336,-96.58159218091699,-96.58109875950011,-96.58151756498015,-96.58104562413331,-96.58144297425174,-96.58100253109997,-96.5813684262898,-96.58098675293326,-96.58129393860477,-96.5809701278333,-96.58121952865213,-96.5809526366334,-96.58114521382539,-96.58093334840015,-96.58107101144894,-96.58091270646688,-96.58099693877091,-96.5808978191001,-96.58092301295613,-96.58088087103332,-96.58084925107917,-96.5808520589334,-96.58077567011728,-96.58083638713342,-96.58070228694356,-96.58080591583332,-96.58062911831989,-96.58079108799986,-96.5805561808902,-96.58077713970006,-96.5804834911736,-96.58077354293353,-96.58041106555767,-96.58077391433349,-96.58033892029175,-96.58077370523343,-96.58026707148021,-96.58077361870016,-96.58019553507603,-96.58077318043341,-96.58012432687403,-96.58076932233348,-96.58005346250457,-96.58076016843344,-96.57998295742709,-96.58074995026693,-96.57991282692376,-96.58070920770037,-96.57984308609322,-96.58068155206693,-96.57977374984431,-96.58064107750022,-96.57970483289009,-96.58056648180013,-96.57963634974156,-96.58049511336667,-96.57956831470192,-96.58043785329993,-96.57950074186049,-96.58037229583337,-96.57943364508687,-96.58029042343337,-96.57936703802542,-96.58019341763332,-96.57930093408939,-96.58009075099997,-96.57923534645543,-96.57998986576656,-96.57917028805814,-96.57988242396657,-96.57910577158459,-96.57979043306662,-96.57904180946913,-96.57971074073333,-96.57897841388819,-96.57962059520001,-96.57891559675507,-96.57951454496673,-96.57885336971499,-96.57937384190006,-96.57879174414023,-96.5792498871668,-96.57873073112539,-96.57913087183341,-96.57867034148241,-96.57903414496677,-96.57861058573626,-96.57893795290012,-96.57855147412043,-96.57884315736665,-96.57849301657228,-96.57874588700003,-96.57843522272917,-96.57862997730007,-96.57837810192402,-96.57848474753345,-96.57832166318121,-96.57835758210003,-96.57826591521311,-96.57823814866671,-96.57821086641566,-96.57814389286666,-96.57815652486508,-96.57804803646671,-96.5781028983142,-96.57794115479999,-96.57804999418897,-96.57782171833335,-96.57799781958514,-96.57768885643334,-96.57794638126498,-96.57759111189992,-96.5778956856544,-96.57750785126659,-96.57784573883983,-96.57742614996661,-96.57779654656534,-96.57733018729988,-96.57774811423012,-96.57723582186652,-96.57770044688569,-96.57709128539999,-96.5776535492337,-96.5769686179668,-96.57760742562334,-96.57689056936685,-96.57756208004947,-96.57682865120009,-96.57751751615021,-96.57674777520009,-96.57747373720541,-96.5766480381337,-96.57743074613455,-96.57656594433288,-96.57738854549538,-96.57652030726624,-96.57734713748204,-96.57650875696645,-96.57730652392415,-96.57650708289981,-96.57726670628523,-96.57650790673313,-96.5772276856619,-96.57651062076656,-96.57718946278244,-96.57651503650051,-96.5771520380067,-96.57651671930078,-96.5771154113247,-96.57651696700083,-96.57707958235676,-96.57651700000082,-96.57704455035251,-96.57651700000082,-96.5770103141912,-96.57651700000082,-96.57697687238114,-96.57651700000082,-96.57694422306004,-96.57651700000082,-96.57691236399521,-96.57651700000082,-96.57688129258361,-96.57651700000082,-96.57685100585269,-96.57651700000082,-96.5768215004607,-96.57651700000082,-96.57679277269774,-96.57651700000082,-96.57676481848645,-96.57651700000082,-96.57673763338308,-96.57651708000083,-96.57671121257899,-96.57651840000077,-96.5766855509016,-96.57652539120055,-96.57666064281618,-96.5765192207005,-96.5766364824273,-96.57648316960078,-96.57661306348074,-96.57644556596736,-96.57659037936529,-96.57640511143379,-96.57656842311516,-96.57639389236695,-96.57654718741166,-96.57639321783324,-96.57652666458613,-96.57639309249977,-96.57650684662195,-96.57639095186664,-96.57648772515768,-96.57639007800003,-96.57646929148936,-96.57638898570008,-96.57645153657393,-96.57638494236666,-96.57643445103194,-96.5763834215666,-96.57641802515097,-96.57638407406667,-96.57640224888901,-96.57639003196685,-96.57638711187776,-96.57639190950027,-96.57637260342666,-96.57639297290012,-96.57635871252616,-96.57639357896676,-96.5763454278522,-96.57639562119988,-96.57633273776995,-96.57640311523339,-96.57632063033796,-96.57640534450006,-96.57630909331274,-96.57640589250008,-96.5762981141531,-96.57640627596676,-96.57628768002468,-96.57640691093341,-96.57627777780493,-96.57640677863328,-96.57626839408771,-96.57640830216691,-96.57625951518857,-96.57641246640033,-96.57625112714965,-96.57641489656686,-96.57624321574505,-96.57641694820012,-96.57623576648635,-96.57642652663355,-96.57622876462784,-96.5764293272003,-96.57622219517243,-96.5764294141669,-96.57621604287739,-96.57642612966653,-96.57621029226007,-96.5764250430998,-96.57620492760407,-96.57642458813308,-96.57619993296539,-96.57642525599981,-96.57619529217862,-96.57642470606629,-96.5761909888633,-96.57642873690004,-96.57618700643046,-96.57643479666686,-96.57618332808927,-96.57643847986662,-96.57617993685353,-96.57644370773309,-96.57617681554878,-96.57644557703303,-96.5761739468191,-96.57645090370002,-96.57617131313398,-96.57645196850012,-96.57616889679593,-96.57645204063348,-96.57616667994718,-96.5764519746335,-96.57616464457745,-96.57645256960025,-96.57616277253105,-96.57645569360028,-96.57616104551485,-96.57645813310008,-96.57615944510543,-96.57645820373351,-96.57615795275726,-96.57645832106684,-96.57615654981012,-96.57645721679998,-96.57615521749746,-96.57646096169981,-96.57615393695392,-96.57646545403301,-96.57615268922385,-96.57647106946658,-96.57615145526913,-96.57647288893337,-96.57615021597753,-96.57647362173338,-96.57614895217124,-96.57647528160011,-96.57614764461488,-96.57648250796655,-96.57614627402432,-96.57648784069977,-96.57614482107502,-96.57649403003339,-96.57614326641067,-96.57649797383347,-96.57614159065184,-96.5764987751667,-96.57613977440485,-96.57649870783344,-96.57613779827038,-96.57649795520018,-96.57613564285244,-96.57649263760001,-96.5761332887672,-96.576476223767,-96.57613071665196,-96.57646470160039,-96.5761279071742,-96.57644875703312,-96.57612484104054,-96.57643983733313,-96.57612149900595,-96.5764377208333,-96.57611786188271,-96.57643621150001,-96.57611391054975,-96.57642831823343,-96.57610962596168,-96.5764245071,-96.57610498915813,-96.57642130119979,-96.57609998127307,-96.57642092573305,-96.57609458354389,-96.57642176783301,-96.57608877732086,-96.57642543556651,-96.57608254407636,-96.57642947330011,-96.57607586541432,-96.57643054500026,-96.57606872307939,-96.57643057453362,-96.57606109896649,-96.5764307716335,-96.5760529751301,-96.57642806806669,-96.57604433379343,-96.57642491829978,-96.57603515735818,-96.57642337739973,-96.57602542841343,-96.57642138189979,-96.57601512974539,-96.57641958073324,-96.57600424434627,-96.57641880119986,-96.57599275542428,-96.57641881746655,-96.57598064641213,-96.57641992796651,-96.57596790097688,-96.57642847523343,-96.57595450302894,-96.57645211896686,-96.57594043673127,-96.57647137513345,-96.57592568650865,-96.5764775857666,-96.57591023705683,-96.57647901326656,-96.57589407335153,-96.5764786542999,-96.57587718065751,-96.57647368033342,-96.57585954453769,-96.57644491706654,-96.57584115086196,-96.57637803783371,-96.57582198581608,-96.5762477007669,-96.57580203591067,-96.57615046599993,-96.57578128798978,-96.57606217949986,-96.57575972923964,-96.57586230169986,-96.57573734719742,-96.57573189673336,-96.57571412975953,-96.57563996596677,-96.57569006519027,-96.57555099636663,-96.57566514213028,-96.57542288089988,-96.57563934960447,-96.57531702956653,-96.57561267703065,-96.57522398629987,-96.57558511422731,-96.57516612753321,-96.57555665142182,-96.57509868066657,-96.57552727925808,-96.57494554259993,-96.57549698880469,-96.57479107976658,-96.57546577156215,-96.57470759456658,-96.57543361947069,-96.57465321403332,-96.57540052491774,-96.57458776396673,-96.5753664807449,-96.57445586203337,-96.57533148025549,-96.57433304703341,-96.57529551722135,-96.57423838970004,-96.57525858588961,-96.5741507711666,-96.5752206809898,-96.57407071179988,-96.57518179774013,-96.57391822929998,-96.57514193185398,-96.57381147456665,-96.57510107954627,-96.57374945103322,-96.57505923753936,-96.57369861596653,-96.57501640306937,-96.57366149389988,-96.57497257389147,-96.5736112838665,-96.57492774828587,-96.57354917796654,-96.57488192506304,-96.57352997973329,-96.574835103569,-96.57351769243336,-96.57478728369027,-96.57349916316689,-96.5747384658588,-96.57347590473351,-96.57468865105652,-96.5734670847001,-96.57463784081988,-96.57346619050018,-96.57458603724403,-96.57346545733355,-96.57453324298677,-96.57346478736687,-96.57447946127243,-96.5734624933001,-96.57442469589532,-96.57346118009995,-96.57436895122314,-96.57346019096661,-96.57431223220026,-96.57345929723321,-96.57425454435023,-96.5734583425332,-96.57419589377849,-96.57345728856643,-96.57413628717524,-96.57345536889987,-96.57407573181669,-96.57345411959989,-96.57401423556776,-96.5734530432666,-96.57395180688309,-96.57345217276658,-96.5738884548088,-96.57345143499992,-96.57382418898321,-96.57345058553339,-96.57375901963798,-96.5734495852334,-96.57369295759831,-96.57344762486684,-96.57362601428318,-96.57344700616682,-96.57355820170572,-96.57344633186675,-96.57348953247221,-96.57344525463353,-96.57342001978188,-96.57344415593347,-96.57334967742575,-96.57344318560014,-96.57327851978538,-96.5734413548,-96.57320656183146,-96.57344012153331,-96.57313381912138,-96.57343919046664,-96.5730603077977,-96.57343815819993,-96.57298604458502,-96.5734373778666,-96.57291104678728,-96.57343649046651,-96.57283533228463,-96.57343440836659,-96.57275891952973,-96.57343321709993,-96.57268182754373,-96.57343222473322,-96.57260407591221,-96.57343119529982,-96.57252568478026,-96.57343023419982,-96.57244667484785,-96.57342835633342,-96.57236706736387,-96.57342707446678,-96.57228688412111,-96.57342617516676,-96.57220614744951,-96.57342503646674,-96.57212488021014,-96.5734240595001,-96.57204310578774,-96.57342309963356,-96.57196084808412,-96.57342108423356,-96.57187813150976,-96.57341402673359,-96.57179498097635,-96.57336164936666,-96.571711421888,-96.57328971270003,-96.57162748013226,-96.57318632066659,-96.57154318207098,-96.57309820583326,-96.57145855453054,-96.57300667903344,-96.57137362479155,-96.57287275453348,-96.57128842057818,-96.57270005946671,-96.57120297004796,-96.57254430310003,-96.571117301779,-96.57243070439998,-96.57103144475906,-96.57232858969992,-96.57094542837274,-96.57218314540002,-96.57085928238907,-96.5720161684666,-96.57077303694768,-96.57187694866666,-96.5706867225455,-96.57173566053336,-96.57060037002256,-96.57163306106669,-96.57051401054714,-96.57147565043337,-96.57042767560057,-96.57123123490011,-96.57034139696204,-96.57101549070013,-96.57025520669194,-96.57089723566676,-96.57016913711567,-96.57080324030007,-96.5700832208064,-96.57069428856676,-96.56999749056759,-96.57054957059994,-96.56991197941468,-96.57029160226655,-96.56982672055675,-96.57015035213325,-96.56974174737748,-96.57004764316669,-96.56965709341524,-96.56989994316675,-96.56957279234319,-96.56973776410005,-96.56948887794846,-96.56956437190007,-96.56940538411138,-96.56938603763345,-96.56932234478312,-96.5692122675334,-96.56923979396416,-96.56904905679998,-96.56915776568093,-96.56888845433328,-96.56907629396287,-96.56874679023331,-96.56899541281847,-96.56861478643332,-96.56891515621056,-96.56843094653335,-96.56883555803192,-96.56818886663345,-96.5687566520795,-96.56796752230001,-96.56867847202795,-96.5678187170667,-96.56860105140356,-96.56769292563324,-96.56852442355679,-96.56755950910001,-96.56844862163436,-96.56746021906663,-96.56837367855107,-96.56733307813325,-96.56829962696085,-96.56711745720007,-96.56822649922704,-96.56699311310008,-96.56815432739269,-96.5668858249001,-96.56808314314927,-96.56680213253328,-96.56801297780605,-96.56673334296629,-96.56794386225748,-96.56668733193321,-96.56787582695156,-96.56667394310004,-96.5678089018557,-96.56667507573341,-96.56774311642428,-96.56667581000008,-96.56767849956333,-96.56667587106674,-96.56761507959575,-96.5666762311001,-96.56755288422666,-96.56667569150018,-96.56749194050565,-96.56667119010021,-96.56743227479208,-96.56666777439993,-96.56737391271574,-96.56666656396652,-96.56731687914026,-96.56666616486656,-96.56726119812325,-96.56666645643325,-96.56720689287839,-96.5666701890671,-96.56715398573445,-96.56667154446717,-96.56710249809535,-96.56667236643382,-96.56705245039859,-96.56667685306694,-96.56700386207356,-96.56667926480004,-96.5669567514997,-96.56668224706658,-96.56691113596234,-96.56668913933332,-96.56686703160931,-96.56669417450018,-96.56682445340753,-96.56669524436687,-96.56678341509649,-96.56669682520024,-96.56674392914356,-96.56669811153355,-96.56670600669716,-96.56670022580029,-96.56666965754015,-96.56670590343342,-96.5666348900416,-96.56671088809998,-96.56660171110924,-96.56671633100001,-96.56657012613999,-96.56672213613355,-96.56654013897042,-96.56672560506664,-96.56651175182586,-96.5667277972999,-96.56648496527146,-96.56673214103309,-96.56645977815786,-96.56673562419991,-96.56643618757067,-96.56674385609996,-96.56641418877715,-96.56674714969998,-96.56639377517239,-96.56674956443329,-96.56637493822495,-96.56675393320005,-96.56635766742235,-96.56675759193351,-96.56634195021523,-96.56676191366685,-96.5663277719606,-96.56676502526668,-96.56631511586588,-96.56676973859985,-96.56630396293009,-96.5667733868999,-96.56629429188641,-96.56677439916653,-96.56628607914314,-96.56677556243325,-96.56627929872305,-96.56677870460021,-96.56627392220477,-96.56678092233348,-96.56626991866034,-96.5667803308001,-96.56626725459448,-96.56677995603346,-96.56626589388163,-96.56677990530011,-96.5662657977032,-96.56677930166684,-96.56626692448394,-96.566780243767,-96.56626922982792,-96.56678016550033,-96.56627266645295,-96.56677936046687,-96.56627718412523,-96.5667785358001,-96.56628272959261,-96.56677753903332,-96.56628924651929,-96.5667756463998,-96.56629667541534,-96.56677462406647,-96.56630495357078,-96.5667726736998,-96.56631401498564,-96.56677170699982,-96.56632379030026,-96.56677059936656,-96.56633420672546,-96.5667645346668,-96.56634518797152,-96.56673339846681,-96.56635665417571,-96.56665467869976,-96.56636852183141,-96.56660431906644,-96.56638070371497,-96.56647988506664,-96.5663931088109,-96.56641200876682,-96.56640564223964,-96.56632135416714,-96.56641820518063,-96.56625017970063,-96.56643069479885,-96.56618403323354,-96.56644300416794,-96.56615964259998,-96.56645502219287,-96.56615164706655,-96.56646663353362,-96.56614889073339,-96.56647771852673,-96.56614897673337,-96.56648815310666,-96.56615581620011,-96.56649780872721,-96.56616488346657,-96.56650655228063,-96.56617340069992,-96.5665142460176,-96.56619150473335,-96.56652074746799,-96.56621703503369,-96.56652590935593,-96.56626035340005,-96.56652957951997,-96.56627186329982,-96.56653160083002,-96.5662795074665,-96.56653181110316,-96.56628704773341,-96.56653004302018,-96.56628975593347,-96.56652612404127,-96.56629163333334,-96.56651987631966,-96.56629340163322,-96.56651111661789,-96.56629367066655,-96.56649965622006,-96.56629265066674,-96.56648530084487,-96.56629173813336,-96.56646785055972,-96.56628536260003,-96.5664470996916,-96.56627682619985,-96.5664228367385,-96.56627123160027,-96.56639484428123,-96.56627077910014,-96.56636289889389,-96.56627007656701,-96.56632677105259,-96.56626972076704,-96.56628622504687,-96.56626670850029,-96.5662410188865,-96.56626735616696,-96.56619090421206,-96.56626590996689,-96.56613562620099,-96.56626533313353,-96.5660749234768,-96.56626578710022,-96.56600852801449,-96.56627018670041,-96.5659361650474,-96.56628601013352,-96.56585755297503,-96.56629028890006],3,null,"Data Points",{"interactive":true,"className":"","stroke":true,"color":"black","weight":1,"opacity":0.5,"fill":true,"fillColor":["#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A","#E41A1C","#4DAF4A"],"fillOpacity":0.8},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLayersControl","args":[["OSM (default)","World Imagery"],"Data Points",{"collapsed":false,"autoZIndex":true,"position":"topright"}]},{"method":"addLegend","args":[{"colors":["#E41A1C","#4DAF4A"],"labels":["m1","m2"],"na_color":null,"na_label":"NA","opacity":1,"position":"bottomright","type":"factor","title":"Model","extra":null,"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[39.18044785223753,39.19518807510001],"lng":[-96.58672080223315,-96.56585755297503]}},"evals":[],"jsHooks":[]}</script>
```


## Estimate a feature or quantity of interest from your estimated trajectory (e.g., velocity, residence time, number of contacts, etc)


```r
# Calculate speed observed data
dist <- st_distance(data$geometry[1:701], data$geometry[2:702], by_element = T)
(sum(dist)/1000)*.62 # Distance observed in km
#> 2.491147 [m]
speed <- (dist/as.numeric(diff(data$time)))*2.24
plot(df$time[-1], speed)
```

<img src="02-Activity_1_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```r

#Convert model coordinates to sf object
data.hat.m1 <- st_as_sf(df.pred, coords = c("long.m1.hat", "lat.m1.hat"), 
                           crs = st_crs(data))

data.hat.m2 <- st_as_sf(df.pred, coords = c("long.m2.hat", "lat.m2.hat"), 
                           crs = st_crs(data))

# Calculate speed m1
dist.hat.m1 <- st_distance(data.hat.m1$geometry[1:741], data.hat.m1$geometry[2:742], by_element = T)
(sum(dist.hat.m1)/1000)*.62 # Distance in km model 1
#> 2.576969 [m]
speed.hat.m1 <- (dist.hat.m1/as.numeric(diff(data.hat.m1$time)))*2.24
plot(data.hat.m1$time[-1], speed.hat.m1,xlab="Time (seconds)",ylab="Velocity (miles per hour)", main = 'Polynomial regression')
```

<img src="02-Activity_1_files/figure-html/unnamed-chunk-7-2.png" width="672" />

```r

# Calculate speed m2
dist.hat.m2 <- st_distance(data.hat.m2$geometry[1:741], data.hat.m2$geometry[2:742], by_element = T)
(sum(dist.hat.m2)/1000)*.62 # Distance in km model 2
#> 2.436389 [m]
speed.hat.m2 <- (dist.hat.m2/as.numeric(diff(data.hat.m2$time)))*2.24
plot(data.hat.m2$time[-1], speed.hat.m2,xlab="Time (seconds)",ylab="Velocity (miles per hour)", main = 'Random Forest')
```

<img src="02-Activity_1_files/figure-html/unnamed-chunk-7-3.png" width="672" />



<!--chapter:end:02-Activity_1.Rmd-->

# Activity 2




## Chose an area on or close to campus where it is easy for you to understand how the elevation changes. For example, I chose the parking lot outside of Dickens Hall. Using a smartphone record the elevation at several locations (points) within the area you chose. I recommend using the app Strava, but you can use whatever you want.

*I decided to use a harvest map retrieved from a combine from my dad's farm. I used to operate the combine and I know the field very well. So I believe it will meet the requirements. Also, all the required information is available, such as coordinates, elevation and time.*

## Obtain a .gpx or .csv file for your elevation data. At minimum the file should contain the location and time of the elevation measurements.

**Upload data**


```r
# Points
points <-  st_read('https://www.dropbox.com/scl/fi/5km5t8yzjqh9fltq5wz2f/soybean23map.geojson?rlkey=7k2ppf8hl9v4oq4nxvx6n2ket&st=xhmxfx42&dl=1') %>% 
  dplyr::select(Elevation, geometry, Time) %>% 
  .[sample(nrow(.), 100), ] 
#> Reading layer `OGRGeoJSON' from data source 
#>   `https://www.dropbox.com/scl/fi/5km5t8yzjqh9fltq5wz2f/soybean23map.geojson?rlkey=7k2ppf8hl9v4oq4nxvx6n2ket&st=xhmxfx42&dl=1' 
#>   using driver `GeoJSON'
#> Simple feature collection with 25601 features and 13 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 314352.5 ymin: 6696958 xmax: 314835.8 ymax: 6697787
#> Projected CRS: WGS 84 / UTM zone 22S
# There are thousands of points, so for the purpose of this activity only a few will be utilized randomly.

# Polygon
polygon <- st_read('https://www.dropbox.com/scl/fi/bxbwxmgs22yx17j2g8m3y/pol.geojson?rlkey=lx39y99fewf8qgzfh87p0ptmp&st=ngala0hj&dl=1')
#> Reading layer `OGRGeoJSON' from data source 
#>   `https://www.dropbox.com/scl/fi/bxbwxmgs22yx17j2g8m3y/pol.geojson?rlkey=lx39y99fewf8qgzfh87p0ptmp&st=ngala0hj&dl=1' 
#>   using driver `GeoJSON'
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5891219 ymin: -3483519 xmax: -5890641 ymax: -3482545
#> Projected CRS: WGS 84 / Pseudo-Mercator
```

## Plot/map your elevation data. I would recommend using R and/or Google earth.


```r
ggplot()+
  geom_sf(data = polygon)+
  geom_sf(data = points, aes(fill = Elevation), shape = 21)+
  scale_fill_viridis_c()+
  labs(x = 'Longitude', y = 'Latitude')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30))
```

<img src="03-Activity_2_files/figure-html/unnamed-chunk-2-1.png" width="672" />

## Explore your elevation data. For example, are there any unique features of your data? Do your data contain obvious measurement error (e.g., an elevation that can’t possibly be true)? Really try to explore your data as best as possible using the plots/maps you made in .


```r
ggplot()+
  geom_histogram(data = points, aes(x = Elevation), 
                 bins = 25, 
                 color = 'black',
                 fill = 'tomato4',
                 alpha = .5)+
  theme_bw()+
  theme(panel.grid = element_blank())
```

<img src="03-Activity_2_files/figure-html/unnamed-chunk-3-1.png" width="672" />

## Write out the goals that you wish to accomplish using your elevation data. For example, my goal was to make a map of the Dicken’s Hall parking lot. This involves using the elevation data I collected to make predictions of the elevation at any possible spatial locations within the parking lot. I would also like to make inference about the location where the elevation is lowest within the parking lot.

*My goal is to make predictions of the elevation at any location of the field and also make inference about where the elevation is highest.*

## Write out several statistical or machine learning models that you think you can use to answer the questions/goals you wrote in prompt #5. Be as creative and inclusive here. For each statistical or machine learning model, make sure you explain each component (piece) of the model

**Model 1 - Random Forest**

$\hat{y}(\textbf{X}) = \frac{1}{B} \sum_{b=1}^B T_b (\textbf{X})$

$B$: number of trees in the forest.\
$T_b(\textbf{X})$: prediction of the b-th tree of input $X$.\
$\hat{y}(\textbf{X})$: final prediction of the random forest for input $X$.

**Model 2 - Kriging**

*Data Model*

$Y(s_i) = m(s_i) + \epsilon(s_i)$\
$\epsilon(s_i) \sim N(0, \sigma^2)$

*Process Model*

$\hat{Y}(s_0) \sim m(s_0) + \sum_{i=1}^{n} \lambda_i [Y(s_i) - m(s_i)]$

$\hat{Y}(s_0)$ represents the predicted elevation at the new location $s_0$.\
$m(s_0)$ is the estimated mean trend at the new location $s_0$.\
$Y(s_i)$ are the observed elevations at known locations $s_i$.\
${\lambda}_i$ are the weights calculated to minimize the variance of the prediction error, based on the spatial autocorrelation structure.\
$m(s_i)$ is the mean trend at the observed locations.\
$n$ is the number of observed locations used in the predictions.

## 7). Of the models you developed in prompt #6, find (or develop) software to fit at least two of these models to your elevation data. Note that in a perfect world, you would be able to either find existing software or develop new programs that enable you to fit any statistical or machine learning model you want. In reality, you may may end up having to make some unwanted changes to your models in prompt #6 to be able to find existing software to fit these models to the data.


**Kriging**



```r

# Create random points
newPoints <- st_sample(polygon, size = 10000, type = "random") %>% 
  as(., 'Spatial') %>% 
  spTransform(., CRS(proj4string(points %>% as(.,'Spatial'))))
```


```r
points
#> Simple feature collection with 100 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 314356.1 ymin: 6696981 xmax: 314828.5 ymax: 6697785
#> Projected CRS: WGS 84 / UTM zone 22S
#> First 10 features:
#>       Elevation                Time
#> 18847  46.69362 4/8/2023 7:40:08 PM
#> 18895  48.92382 4/8/2023 7:40:56 PM
#> 25102  61.36804 4/8/2023 9:41:34 PM
#> 2986   66.15939 4/8/2023 2:30:08 PM
#> 1842   50.01868 4/8/2023 2:04:36 PM
#> 3371   50.05362 4/8/2023 2:36:33 PM
#> 11638  71.28713 4/8/2023 5:14:48 PM
#> 4761   60.44254 4/8/2023 3:03:38 PM
#> 6746   58.86041 4/8/2023 3:42:34 PM
#> 16128  43.89501 4/8/2023 6:46:11 PM
#>                       geometry
#> 18847 POINT (314620.2 6697715)
#> 18895 POINT (314630.1 6697657)
#> 25102 POINT (314639.5 6697339)
#> 2986  POINT (314557.5 6697202)
#> 1842  POINT (314484.4 6697654)
#> 3371  POINT (314492.5 6697664)
#> 11638   POINT (314446 6697021)
#> 4761  POINT (314512.1 6697424)
#> 6746    POINT (314489 6697471)
#> 16128 POINT (314655.1 6697785)

krig.df <- data.frame(ele = points$Elevation,
                      lon = st_coordinates(points$geometry)[,1],
                      lat = st_coordinates(points$geometry)[,2])

krig.mod <- gam(ele ~ s(lon,lat, bs = 'gp'), data = krig.df)

newpoints.krig <-  as.data.frame(newPoints) %>% 
  rename("lon" = 'coords.x1', 
         'lat' = 'coords.x2')

newpoints.krig$ele <- predict(krig.mod, newpoints.krig, type = 'response')
```


**Random Forest**


```r
df.rf <- as.data.frame(points)
df.rf$lon <- st_coordinates(points)[,1]
df.rf$lat <- st_coordinates(points)[,2]

rf.fit <- randomForest(Elevation ~ lon + lat, data=df.rf, ntree=500, importance=TRUE)

plot(rf.fit)
```

<img src="03-Activity_2_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r

newPoints.rf <- as.data.frame(newPoints) %>% 
  rename("lon" = 'coords.x1', 
         'lat' = 'coords.x2')

pred.rf <- predict(rf.fit, newPoints.rf) %>% as.data.frame()

newPoints.rf$ele <- pred.rf$.
```

## Related to prompt #5, use both models you fit to your elevation data in prompt #7 to answer the questions/goals. For my elevation data, this would include making a predictive heatmap showing the elevation of the Dickens Hall parking lot and then estimating the coordinates of the point where elevation is at a minimum.


**Kriging**


```r
krig.sf <- st_as_sf(newpoints.krig, coords = c('lon','lat'), crs = st_crs(points))

ggplot()+
  geom_sf(data = krig.sf, aes(fill = ele), 
          shape = 21)+
    geom_sf(data = polygon, fill = NA, color = 'black')+
  geom_sf(data = krig.sf %>% filter(ele == max(ele)), fill = 'darkred', shape = 22,
          size = 3)+
  scale_fill_viridis_c()+
  labs(x = 'Longitude', y = 'Latitude')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30))
```

<img src="03-Activity_2_files/figure-html/unnamed-chunk-7-1.png" width="672" />

**Random Forest**


```r
rf.sf <- st_as_sf(newPoints.rf, coords = c('lon','lat'), crs = st_crs(points))

ggplot()+
  geom_sf(data = rf.sf, aes(fill = ele), 
          shape = 21)+
    geom_sf(data = polygon, fill = NA, color = 'black')+
  geom_sf(data = krig.sf %>% filter(ele == max(ele)), fill = 'darkred', shape = 22,
          size = 3)+
  scale_fill_viridis_c()+
  labs(x = 'Longitude', y = 'Latitude')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30))
```

<img src="03-Activity_2_files/figure-html/unnamed-chunk-8-1.png" width="672" />

## Based on the material in Chapter 6 of Spatio-Temporal Statistics with R and our discussion in class on March 26, compare, check and evaluate the two models from #8.


```r
# Obtain new points at the same area for model testing.
newpoints.test <- st_read('https://www.dropbox.com/scl/fi/5km5t8yzjqh9fltq5wz2f/soybean23map.geojson?rlkey=7k2ppf8hl9v4oq4nxvx6n2ket&st=xhmxfx42&dl=1') %>% 
  dplyr::select(Elevation, geometry, Time) %>% 
  .[sample(nrow(.), 100), ]
#> Reading layer `OGRGeoJSON' from data source 
#>   `https://www.dropbox.com/scl/fi/5km5t8yzjqh9fltq5wz2f/soybean23map.geojson?rlkey=7k2ppf8hl9v4oq4nxvx6n2ket&st=xhmxfx42&dl=1' 
#>   using driver `GeoJSON'
#> Simple feature collection with 25601 features and 13 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 314352.5 ymin: 6696958 xmax: 314835.8 ymax: 6697787
#> Projected CRS: WGS 84 / UTM zone 22S

# Plot training and testing datasets
ggplot()+
  geom_sf(data = polygon)+
  geom_sf(data = points, shape = 21, fill = 'gold')+
  geom_sf(data = newpoints.test, shape = 22, fill = 'pink4')+
  scale_fill_viridis_c()+
  labs(x = 'Longitude', y = 'Latitude')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30))
```

<img src="03-Activity_2_files/figure-html/unnamed-chunk-9-1.png" width="672" />


```r
# Predictions
test.df <- data.frame(ele = newpoints.test$Elevation,
                      lon = st_coordinates(newpoints.test$geometry)[,1],
                      lat = st_coordinates(newpoints.test$geometry)[,2])

test.df$pred.krig <- predict(krig.mod, newdata = test.df, type = 'response')
test.df$pred.rf <- predict(rf.fit, newdata = test.df)
```


```r
rmse.krig <- rmse(test.df$ele, as.numeric(test.df$pred.krig))
mae.krig <- mae(test.df$ele, as.numeric(test.df$pred.krig))

rmse.rf <- rmse(test.df$ele, as.numeric(test.df$pred.rf))
mae.rf <- mae(test.df$ele, as.numeric(test.df$pred.rf))

#Kriging metrics

krigingMetrics <- test.df %>% 
  ggplot()+
  geom_point(aes(pred.krig, ele), fill = 'purple4', color = 'black', shape = 21, size = 2,
             alpha = .7)+
  geom_abline(slope = 1)+
  #scale_y_continuous(limits = c(0,90), breaks = seq(0,100, 20))+
  #scale_x_continuous(limits = c(0,90), breaks = seq(0,100, 20))+
  theme_bw()+
  labs(title = 'Kriging', x = 'Predicted', y = 'Observed')+
  annotate('text', label = paste0('RMSE: ', round(rmse.krig,1)), x = 65, y = 50)+
  annotate('text', label = paste0('MAE: ', round(mae.krig,1)), x = 65, y = 48)+
  theme(panel.grid = element_blank(),
        aspect.ratio = 1,
        text = element_text(size = 12)
        )

#Rf metrics

rfMetrics <- test.df %>% 
  ggplot()+
  geom_point(aes(pred.rf, ele), fill = 'purple4', color = 'black', shape = 21, size = 2,
             alpha = .7)+
  geom_abline(slope = 1)+
  #scale_y_continuous(limits = c(0,90), breaks = seq(0,100, 20))+
  #scale_x_continuous(limits = c(0,90), breaks = seq(0,100, 20))+
  theme_bw()+
  labs(title = 'Random Forest', x = 'Predicted', y = 'Observed')+
  annotate('text', label = paste0('RMSE: ', round(rmse.rf,1)), x = 65, y = 50)+
  annotate('text', label = paste0('MAE: ', round(mae.rf,1)), x = 65, y = 48)+
  theme(panel.grid = element_blank(),
        aspect.ratio = 1,
        text = element_text(size = 12)
        )

ggarrange(krigingMetrics,rfMetrics)
```

<img src="03-Activity_2_files/figure-html/unnamed-chunk-11-1.png" width="672" />


<!--chapter:end:03-Activity_2.Rmd-->

# Activity 3




## Upload data


```r
# Upload data
url <- "https://www.dropbox.com/scl/fi/9ymxt900s77uq50ca6dgc/Enders-et-al.-2018-data.csv?rlkey=0rxjwleenhgu0gvzow5p0x9xf&dl=1"
df <- read.csv(url)
df <- df[,c(2,8:10)] %>% 
  mutate(presence = ifelse(EGA != 0, 1, 0))# Keep only the data on bird cherry-oat aphid

# Download KS shapefile
ks <- raster::getData(name="GADM", country="USA", level=1) %>% 
  st_as_sf() %>% 
  filter(NAME_1 == 'Kansas')

df_sf <- df %>% st_as_sf(coords = c('long', 'lat'), crs = st_crs(ks))
```

## Upload covariates


```r
url.nlcd <- "https://www.dropbox.com/scl/fi/ew7yzm93aes7l8l37cn65/KS_2011_NLCD.img?rlkey=60ahyvxhq18gt0yr47tuq5fig&dl=1"
rl.nlcd2011 <- raster(url.nlcd)

plot(rl.nlcd2011)
```

<img src="04-Activity_3_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r


# Make raster file that contains pixels with value of 1 if grassland and 
# zero if other type of land cover.
# NLCD legend can be found here: https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend
rl.nlcd.grass <- rl.nlcd2011
rl.nlcd.grass[] <- ifelse(rl.nlcd.grass[]==71,1,0)

plot(rl.nlcd.grass)
```

<img src="04-Activity_3_files/figure-html/unnamed-chunk-2-2.png" width="672" />

```r

pts.sample<- df

coordinates(pts.sample) =~ long + lat
proj4string(pts.sample) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Calculate percentage of land area that is grassland within 5 km of sampled location
df$grass.perc <- unlist(lapply(extract(rl.nlcd.grass,pts.sample,buffer=5000),mean))*100

hist(df$grass.perc,col="grey",main="",xlab="% grassland within \n5 km at sample location")
```

<img src="04-Activity_3_files/figure-html/unnamed-chunk-2-3.png" width="672" />

## For the data on the abundance of English grain aphids, propose three different statistical models (or machine learning approach) that are capable of predicting the number of English grain aphids at any location within the state of Kansas at any time for the years 2014 and 2015. Make sure to write out the three statistical models using formal notation and fully describe each component using words.

*Model 1*

Data model $$Z = y$$ Process model Using Poisson distribution

$$[y|\lambda] = Poisson(\lambda) $$

$$\eta_s\sim MUN(0, \sigma)$$ $$\eta_t\sim MVN(0, \sigma)$$
$$E(y)=e^{\beta_0+\beta_1\cdot X + \eta_s+\eta_t}$$

*Model 2*

Data model

$$Z = y$$ Process model Using Negative binomial distribution

$$[y|r,p] = NB(r, p)$$ $$\eta_s\sim MVN(0, \sigma)$$
$$\eta_t\sim  MVN(0, \sigma)$$
$$E(y)=e^{\beta_0+\beta_1\cdot X + \eta_s+\eta_t}$$

*Model 3*

Data model

$$Z = y$$ Process model Using zero inflated poisson distribution

$$[y|p, \lambda]=ZIP(p,\lambda)$$ $$\eta_s\sim MVN(0, \sigma^2)$$
$$P(y=0) = p\cdot e^{-0} + (1-p) \cdot e^{-{\lambda}}$$
$$P(y = k) = (1-p) \cdot \frac{(e^{-{\lambda}}{\lambda^k})}{k!}, k \in N$$
$${\lambda} > 0, p \in (0,1)$$
$$E(y)=e^{\beta_0+\beta_1\cdot X + \eta_s+\eta_t}$$

## For the three statistical models you proposed in question #1, propose a way to measure the accuracy (and perhaps the calibration) of predictions.

RMSE and MAE and AIC.

## Fit the three statistical models you proposed in question #1 to the English grain aphid abundance data.


```r

ggplot()+
  geom_sf(data = ks)+
  geom_sf(data = df_sf, shape = 21, aes(size = EGA, fill = factor(presence)))+
  theme_bw()+
  facet_wrap(~year, ncol = 1)
```

<img src="04-Activity_3_files/figure-html/unnamed-chunk-3-1.png" width="672" />

## Data Split 


```r
set.seed(100)
df.sample <- sample(c(TRUE,FALSE), nrow(df), replace=TRUE, prob=c(0.5,0.5))
df.train <- df[df.sample,]
df.test <- df[!df.sample,]
```

### Model 1


```r
m1 <- gam(EGA ~ grass.perc + as.factor(year) + s(long,lat, bs = "gp"), 
          family = poisson(link = "log"), data = df.train)

summary(m1)
#> 
#> Family: poisson 
#> Link function: log 
#> 
#> Formula:
#> EGA ~ grass.perc + as.factor(year) + s(long, lat, bs = "gp")
#> 
#> Parametric coefficients:
#>                      Estimate Std. Error z value Pr(>|z|)
#> (Intercept)         -4.072721   0.508579  -8.008 1.17e-15
#> grass.perc          -0.034166   0.001664 -20.538  < 2e-16
#> as.factor(year)2015  6.148542   0.500663  12.281  < 2e-16
#>                        
#> (Intercept)         ***
#> grass.perc          ***
#> as.factor(year)2015 ***
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df Chi.sq p-value    
#> s(long,lat) 31.45   31.8   5663  <2e-16 ***
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> R-sq.(adj) =   0.67   Deviance explained = 81.1%
#> UBRE = 17.558  Scale est. = 1         n = 178
```

### Model 2


```r
m2 <- gam(EGA ~ grass.perc + as.factor(year) + s(long,lat, bs = "gp"), 
          family = nb(theta = NULL,link = "log"), data = df.train)

summary(m2)
#> 
#> Family: Negative Binomial(0.653) 
#> Link function: log 
#> 
#> Formula:
#> EGA ~ grass.perc + as.factor(year) + s(long, lat, bs = "gp")
#> 
#> Parametric coefficients:
#>                     Estimate Std. Error z value Pr(>|z|)
#> (Intercept)         -3.29390    0.61890  -5.322 1.03e-07
#> grass.perc          -0.00141    0.00678  -0.208    0.835
#> as.factor(year)2015  5.67995    0.59656   9.521  < 2e-16
#>                        
#> (Intercept)         ***
#> grass.perc             
#> as.factor(year)2015 ***
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df Chi.sq p-value    
#> s(long,lat) 8.356  11.02  224.3  <2e-16 ***
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> R-sq.(adj) =  0.263   Deviance explained = 72.7%
#> -REML = 493.04  Scale est. = 1         n = 178
```

### Model 3


```r

m3 <- gam(list(EGA ~ grass.perc + as.factor(year) + s(long,lat, bs = "gp"), ~ grass.perc + s(long,lat, bs = "gp")), 
          family = ziplss(), data = df.train)

summary(m3)
#> 
#> Family: ziplss 
#> Link function: identity identity 
#> 
#> Formula:
#> EGA ~ grass.perc + as.factor(year) + s(long, lat, bs = "gp")
#> ~grass.perc + s(long, lat, bs = "gp")
#> 
#> Parametric coefficients:
#>                      Estimate Std. Error z value Pr(>|z|)
#> (Intercept)         -3.561202   0.936080  -3.804 0.000142
#> grass.perc          -0.039519   0.001732 -22.811  < 2e-16
#> as.factor(year)2015  5.546180   0.927371   5.981 2.22e-09
#> (Intercept).1       -0.038220   0.186544  -0.205 0.837661
#> grass.perc.1         0.001405   0.005252   0.267 0.789118
#>                        
#> (Intercept)         ***
#> grass.perc          ***
#> as.factor(year)2015 ***
#> (Intercept).1          
#> grass.perc.1           
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>                  edf Ref.df  Chi.sq p-value    
#> s(long,lat)   30.937 31.330 4819.22 < 2e-16 ***
#> s.1(long,lat)  2.828  3.463   16.34 0.00192 ** 
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Deviance explained = 78.1%
#> -REML = 1765.8  Scale est. = 1         n = 178
```

## Create points for prediction


```r
####
rl.E.y <- raster(,nrow=30,ncols=30,ext=extent(ks),crs=crs(ks))
newPoints <- data.frame(long = xyFromCell(rl.E.y,cell=1:length(rl.E.y[]))[,1],
                      lat = xyFromCell(rl.E.y,cell=1:length(rl.E.y[]))[,2]) %>%
  st_as_sf(coords = c('long', 'lat'), crs = st_crs(ks)) %>% 
  st_filter(ks) %>% as.data.frame() %>% 
  cross_join(data.frame(year = as.factor(c('2014', '2015'))))

newPoints$lat <- st_coordinates(newPoints$geometry)[,2]
newPoints$long <- st_coordinates(newPoints$geometry)[,1]

# 
# newPoints <- st_sample(ks, size = 1000, type = "regular") %>%
#   as(., 'Spatial') %>% as.data.frame() %>%
#     rename("long" = 'coords.x1',
#          'lat' = 'coords.x2') %>%
#   cross_join(data.frame(year = as.factor(c('2014', '2015'))))

    
pts.sample<- newPoints

coordinates(pts.sample) =~ long + lat
proj4string(pts.sample) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

newPoints$grass.perc <- unlist(lapply(extract(rl.nlcd.grass,pts.sample,buffer=5000),mean))*100

```


```r
# Fit mod 1
newPoints$y_pred1 <- predict(m1, newPoints, type = 'response')

# m1.pred <- st_as_sf(newPoints, coords = c('long', 'lat'), crs = st_crs(ks),
#                     agr = 'constant') 

ggplot()+

  geom_tile(data = newPoints, aes(x = long, y = lat, fill = y_pred1))+
    geom_sf(data = ks, fill = NA, color = 'black')+
  scale_fill_viridis_c()+
  labs(x = 'Longitude', y = 'Latitude')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30))+
  facet_wrap(~year, ncol = 1)
```

<img src="04-Activity_3_files/figure-html/unnamed-chunk-9-1.png" width="672" />


```r
# Fit mod 2
newPoints$y_pred2 <- predict(m2, newPoints, type = 'response')


ggplot()+

  geom_tile(data = newPoints, aes(x = long, y = lat, fill = y_pred2))+
    geom_sf(data = ks, fill = NA, color = 'black')+
  scale_fill_viridis_c()+
  labs(x = 'Longitude', y = 'Latitude')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30))+
  facet_wrap(~year, ncol = 1)
```

<img src="04-Activity_3_files/figure-html/unnamed-chunk-10-1.png" width="672" />


```r
# Fit mod 3
newPoints$y_pred3 <- predict(m3, newPoints, type = 'response')


ggplot()+

  geom_tile(data = newPoints, aes(x = long, y = lat, fill = y_pred3))+
    geom_sf(data = ks, fill = NA, color = 'black')+
  scale_fill_viridis_c()+
  labs(x = 'Longitude', y = 'Latitude')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30))+
  facet_wrap(~year, ncol = 1)
```

<img src="04-Activity_3_files/figure-html/unnamed-chunk-11-1.png" width="672" />

## For the three models you fit in question #3, which model makes the most accurate predictions? How good is the best model in real world terms? Remember we are trying to predict the number of English grain aphids, which is a count!


```r
rmse.m1 <- rmse(df.test$EGA, as.numeric(predict(m1, df.test, type = 'response')))
mae.m1 <- mae(df.test$EGA, as.numeric(predict(m1, df.test, type = 'response')))

rmse.m2 <- rmse(df.test$EGA, as.numeric(predict(m2, df.test, type = 'response')))
mae.m2 <- mae(df.test$EGA, as.numeric(predict(m2, df.test, type = 'response')))

rmse.m3 <- rmse(df.test$EGA, as.numeric(predict(m3, df.test, type = 'response')))
mae.m3 <- mae(df.test$EGA, as.numeric(predict(m3, df.test, type = 'response')))

#m1 metrics

m1Metrics <- df.test %>% 
  ggplot()+
  geom_point(aes(predict(m1, df.test, type = 'response'), EGA), 
fill = 'purple4', color = 'black', shape = 21, size = 2,
             alpha = .7)+
  geom_abline(slope = 1)+
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000, 200))+
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000, 200))+  theme_bw()+
  labs(title = 'm1', x = 'Predicted', y = 'Observed')+
  annotate('text', label = paste0('RMSE: ', round(rmse.m1,1)), x = 700, y = 200)+
  annotate('text', label = paste0('MAE: ', round(mae.m1,1)), x = 700, y = 100)+
  theme(panel.grid = element_blank(),
        aspect.ratio = 1,
        text = element_text(size = 12)
        )

#m2 metrics

m2Metrics <- df.test %>% 
  ggplot()+
  geom_point(aes(predict(m2, df.test, type = 'response'), EGA), 
             fill = 'purple4', color = 'black', shape = 21, size = 2,
             alpha = .7)+
  geom_abline(slope = 1)+
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000, 200))+
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000, 200))+  theme_bw()+
  labs(title = 'm2', x = 'Predicted', y = 'Observed')+
  annotate('text', label = paste0('RMSE: ', round(rmse.m2,1)), x = 700, y = 200)+
  annotate('text', label = paste0('MAE: ', round(mae.m2,1)), x = 700, y = 100)+
  theme(panel.grid = element_blank(),
        aspect.ratio = 1,
        text = element_text(size = 12)
        )

m3Metrics <- df.test %>% 
  ggplot()+
  geom_point(aes(predict(m3, df.test, type = 'response'), EGA), 
             fill = 'purple4', color = 'black', shape = 21, size = 2,
             alpha = .7)+
  geom_abline(slope = 1)+
  scale_y_continuous(limits = c(0,1000), breaks = seq(0,1000, 200))+
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000, 200))+
  theme_bw()+
  labs(title = 'm3', x = 'Predicted', y = 'Observed')+
  annotate('text', label = paste0('RMSE: ', round(rmse.m3,1)), x = 700, y = 200)+
  annotate('text', label = paste0('MAE: ', round(mae.m3,1)), x = 700, y = 100)+
  theme(panel.grid = element_blank(),
        aspect.ratio = 1,
        text = element_text(size = 12)
        )

ggarrange(m1Metrics,m2Metrics, m3Metrics, nrow = 1)
```

<img src="04-Activity_3_files/figure-html/unnamed-chunk-12-1.png" width="1008" />

```r

AIC(m1, m2, m3)
#>          df      AIC
#> m1 34.44604 3764.439
#> m2 14.12373  980.207
#> m3 39.79306 3313.507
```

## Summarize your results using words, numerical values and figures/maps.

The three models were tested after splitting the data into testing and training through MAE and RMSE. In addition, AIC was compared. Both methods suggested that the model 2 was the best one. Below the predictions using the model 2 are presented for both years 2014 and 2015. The year 2014 resulted in very minimum abundance of EGA, while in 2015, the East region showed a high concentration of EGA.

<img src="04-Activity_3_files/figure-html/unnamed-chunk-13-1.png" width="672" />


<!--chapter:end:04-Activity_3.Rmd-->
