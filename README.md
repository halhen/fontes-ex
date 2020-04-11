# Introduction

This is my submission for the [programming exercise](https://github.com/fontes-lab/RShinyProgrammingExercise) given for the R/Shiny developer position at Fontes Lab.

Features:

 * Filter patients to analyze subsets of data
 * Highlight patients to contrast subsets of data
 * Explore patient data distributions in totall or by contrasting groups
 * Fetch individual subject information, including their lab value results as compared to other participants
 
## Dependencies

There are some dependencies for this application. These can be installed by running `source('install_dependencies.R')` in RStudio with this project open. 

## Process

Given the time limit of about five hours, I focused on showcasing a bit of best-practices software development (unit tests, version control, clean-as-you-go-refactoring) as well as some variations of interactive data exploration in Shiny. Since I know very little about the end-users or context for the app, I made the following assumptions:

 * The users are expert users, as opposed to casual ones. This means that the users will know the lingo of the app going in and will it frequently enough to learn it. Two examples where this assumption makes a difference are: keeping (presumably) established abbreviations over readable explanations, and to keep the UI dense over learnable.
 * The users are researchers, which means that they are well versed in statistics and its terminology.
 * Users will be on computers (as opposed to mobile devices), which is why I had a reasonable-resolution, computer monitor in mind for the UI.
 * The data to be analyzed is only the data provided. I've prepared the code to switch data sets or to implement dynamic data (via uploaded files, databases, or the like), but to keep complexity down I check data quality in build time and let the app assume it is OK.
 
## Limitations

The two main limitations of this code are time and lack of context.

As a freelancer: time is one of the constraints that the client specifies. In this case the budget was about five hours. Therefore I decided to go for a cheap, plain Shiny app over a technically more robust but also more expensive R package. With more time to build, or knowing that this app was to be maintained over time, I would likely have started building this as a package. This would have provided several long-term benefits such as a more established toolchain for testing and deployment, dependency resolution, and the opportunity to build out chart generation and other code for reuse in e.g. exploratory R work.

Specifically, I don't do very much automated testing of this app since much of it resides in Shiny/UI code. While it is possible to do some smoke testing of UI via e.g. snapshot testing, it is often quite verbose and expensive to maintain. To show _some_ automated testing in this, I wrote and maintained a small suite of tests for the data wrangling.

The lack of context -- specifically, not knowing my users and having them state or feedback on their requirements -- forced me to build quite a generic app. Much of data visualization and analytics is about finding and highlighting differences. I spent a little time looking for structure in the data -- covaration between variables, or autocorrelation in the time series -- but found none. Had I done so, or had I had my users request some feature along these lines, I would have made sure to include those. I did some false (read: uninteresting) starts with some scatteplot matrices, and some automatic clustering of the lab value time series. In the end, I felt that they added noise to data with little signal, and therefore decided to leave them out.

Finally, due to both limiting factors, I ended up doing some plain exploration tools only around the patients, as opposed to their lab value time series. To do the latter, I would like to know more about how the researchers intend to analyze the time series and/or what hypotheses they are interested in. In the spirit of showcasing Shiny chops, I settled on plain patient data.

## Data assumptions

These are some assumptions I made during development, that I would ask a stakeholder about had they been available

 * SEX U and Undifferentiated are the same thing, and I merge them to one.
 * Screening lab values are not interesting in the trial time series. Instead I moved them to belong to the patient.