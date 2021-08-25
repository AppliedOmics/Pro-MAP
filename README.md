**Single Channel Microarray Preprocessing pipeline (SCaMP)** is a shiny app. It can be run within [Rstudio](https://www.rstudio.com/products/rstudio/download/) or hosted on a [shiny-server](https://shiny.rstudio.com/articles/shiny-server.html). 

A running version of the app can be found at https://metaomics.uct.ac.za/shinyapps/SCaMP/

# R packages

The following R pacakged need to be installed for the app to run

Shiny libraries
```
install.packages('shiny')
install.packages('shinydashboard')
install.packages(shinydisconnect)

```

Tideyverse Packages

```
install.packages('tidyverse')
install.packages('dplyr')
install.packages('tidyr')
install.packages('magrittr)

install.packages('ggplot2')

```

```
install.packages('plotly')
```


BioConductor Packages

```
install.packages("BiocManager")
BiocManager::install("limma")
BiocManager::install('Biobase')
BiocManager::install('MSnbase')

```
ComplexHeatamp
May require some linux packages
```
apt-get install libgtk2.0-dev, libcairo2-dev, xvfb, xauth, xfonts-base
```

```
install.packages('devtools')
library(devtools)
install_github("jokergoo/ComplexHeatmap",force = TRUE,dependencies = TRUE)

install.packages('circlize')

```


# EnhancedVolcano

```
sudo apt-get install libharfbuzz-dev
sudo apt-get install -y libfribidi-dev
install.packages('textshaping')

BiocManager::install('EnhancedVolcano')
```

RColorBrewer

```
sudo apt-get install  libudunits2-dev
```

```
install.packages('RColorBrewer')
```

Other

```
install.packages('OptimalCutpoints')
install.packages('foreach')
```