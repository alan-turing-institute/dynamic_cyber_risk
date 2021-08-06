# Dynamic cyber risk estimation with Competitive Quantile Autoregression

The visualisation of the predictions of the cyber breaches' sizes is available [here](https://nbviewer.jupyter.org/github/alan-turing-institute/dynamic_cyber_risk/blob/main/main_size.nb.html), and of the predictions of cyber breaches' inter-arrival times is available [here](https://nbviewer.jupyter.org/github/alan-turing-institute/dynamic_cyber_risk/blob/main/main_time.nb.html). You can also reproduce the results using [Binder](https://mybinder.org). Click on [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/alan-turing-institute/dynamic_cyber_risk/HEAD?urlpath=rstudio). You will see the RStudio window. In the RStudio, you can run `main_size.Rmd` to reproduce the results for the breaches' sizes and `main_time.Rmd` to reproduce the results for the breaches' inter-arrival times. 

`main_functions.R` contains the code for Competitive Quantile Autoregression (CQAR);

`params_time.R` finds the parameters of CQAR on the training dataset;

`preprocessing.R` removes incomplete and missing data.

## Installation

The easiest way to run the project is to use Binder (see above). If you want to run the project locally you need to download [R](https://www.r-project.org) and [RStudio](https://rstudio.com).
Clone the repository:
```bash
git clone https://github.com/alan-turing-institute/dynamic_cyber_risk.git dynamic_cyber_risk
```
Open Rstudio and first run `install.R` to install the required packages. After that you should be able to run `main_size.Rmd` and `main_time.Rmd` to reproduce the paper's results.
