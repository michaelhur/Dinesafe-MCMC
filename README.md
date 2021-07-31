# Dinesafe-MCMC
A Bayesian approach to zero-inflated count data with an application to regulatory compliance data. Markov Chain Monte Carlo (MCMC) was used on Toronto's Dinesafe data to explore the so-called "downtown" effect.

### Dinesafe data

The dataset used in this project, "dinesafe.xml", can be found under the "data" folder. It was originally retreived from the following website:

```
https://open.toronto.ca/dataset/dinesafe/
```

To use the dataset for other purposes, please consult the "Licence" page of Toronto's Open Data Catalogue: 
```
https://open.toronto.ca/open-data-license/
```

### Prerequisites
Following R packages were used:
```
library(XML)
library("dplyr")
library("coda")
library("xtable")
library(ggplot2)
library(ggmap)
```

### Other Software Used
QGIS was used to determine whether an establishment is located in the entertainment district. The shapefile used for this part of the project can also be found in Toronto's Open Data Catalogue.

```
https://www.google.ca/search?rlz=1C1CHBF_koCA751CA751&q=neighbourhoods+planning+areas+wgs84&spell=1&sa=X&ved=0ahUKEwjs8O6w7NrVAhVM6oMKHU6tDd4QBQgjKAA&biw=1920&bih=974
```

### Acknowledgement

For the geocoding section, I directly used the code written by Shane Lynn. The code can be found in the following link:
```
https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/
```

