# Housing Price in Milwaukee
## Quick Overview
In general, Real Estate prices can be influenced by broad economic factors, such as population growth, income level, and construction costs, etc., as well as physical characteristics of the properties. As property developers, it is essential for us to estimate each property's fair value before obtaining the asking price. Thus, in this report, we build models to predict property prices in the City of Milwaukee using controllable variables, such as location, year of built, and size.

- Applied log and square root transformations on the response variables in order to get a better fit linear model.
- Discovered impact on sales price by a interaction effect between property size and property style. Tested 12 linear regression models Used AIC and Box-Cox lamda to justify for model complexity. The best performing model has an adjusted R-squared of 0.6943, a Box-cox lambda of 1.0303, and 23/25 of the predictors are significant at 95% confidence level.
