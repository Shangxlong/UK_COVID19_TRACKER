# UK COVID-19 TRACKER
# Introduction:
A web application written in Shiny (R language). As the name shows, it is a data presentation platform providing simple and up-to-date information about COVID-19 condition in UK. Most importantly, it can be easily read for ordinary people.

## Getting Started
The project can be easily recreated to map the data in any places by revising the code in the app.R and dashboardSever.R in the app.file. We specifically choose to present R value (reproduction rate) in dashboard page because in our knowledge there is no map tracker for it and the value is regarded as the standard for lockdown policy in England.

### Prerequisites

All you need is R and its IDE (in this case we run it in the Rstudio). The packages needed should already be included in the packageLoad.app. Please set the working directory to source file location first. 
```
setwd('~') # ~ as the src

shiny::runApp('~')
```

### Interactive
Most of images and plots are interactive customized for different needs. The instructions are given near them.

### Preview
Homepage
![alt text](https://i.ibb.co/stF4mCz/Screen-Shot-2020-08-01-at-23-50-31.png)
Dashboard
![alt text](https://i.ibb.co/zNNxP6H/Processed-with-MOLDIV.jpg)
Forecast
![alt text](https://i.ibb.co/pzXQdwZ/Screen-Shot-2020-08-01-at-23-52-20.png)

## Authors

* **Shiney Long** - *Initial work* - [Shangxlong](https://github.com/Shangxlong)

Other two who participated in this project haven't got github account. The information will be updated as soon as they register.
## Acknowledgments

* Data source: gov.uk & coronavirus package in R
