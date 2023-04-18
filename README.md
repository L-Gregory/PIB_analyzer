# PIB_analyzer

This app is a web-based tool that allows researchers to analyze net CO2 assimilation rate acrossed a light-dark transient (i.e., post illumination burst; PIB). The app allows you to upload a CSV file, check data quality, and estimate five unique photosynthetic parameters from the PIB data. The app is built on the R programming language and the Shiny web framework.

## Getting Started

### Prerequisite
To run the app locally, you need to have R (version 4.0.0 or higher) and RStudio installed on your computer. You also need to install the following packages:

shiny - The web framework used
shinythemes - A package used for customizing the appearance of the app
dplyr - A collection of R packages used for data wrangling 
ggplot2 - A package used for data visualization
purrr - A package that contains tools for working wih vectors


You can install these packages by running the following command in R:

```{r]
install.packages(c("shiny", "shinythemes", "dplyr", "ggplot2", "purrr"))
```

### Installation 
To install the app, you can download the code from the GitHub repository (do the following command in the terminal, after setting the desired directory):

```{r}
git clone https://github.com/L-gregory/PIB_analyzer.git
```

### Running the App
To run the app, open the app.R file in RStudio and click the **"Run App"** button in the top right corner of the script editor window. This will launch the app in a new window.

## Using the App
### Upload Data Tab

In the "Upload Data" tab, you can upload your CSV file and select the columns containing the time and net assimilation rate data. You can also view a table of the selected data and a plot of the original data.

If you do not have your own data to upload, you can use the provided demo data by clicking on the "Load Demo Data" checkbox.

### Fitting the Uploaded Data

After checking for data quality, select the **"Fit"** button. If the fitting is successful, the plot will updated with the linear regression overtop of the steady state dark respiration, and a table of fit parameters will be displayed.

If yu are unhappy with the fitting, you can adjust the number of values used in the linear regession (default = 50).


## Credit
The App was developed by Luke Gregory and Mauricio Tejera-Nieves under the supervision of Berkley Walker at Michigan State University.





