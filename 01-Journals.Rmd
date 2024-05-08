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

