
[![R-CMD-check.yaml](https://github.com/byzheng/tidyweather/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/byzheng/tidyweather/actions/workflows/R-CMD-check.yaml)[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tidyweather)](https://cran.r-project.org/package=tidyweather)

[![](http://cranlogs.r-pkg.org/badges/grand-total/tidyweather?color=green)](https://cran.r-project.org/package=tidyweather)
[![](http://cranlogs.r-pkg.org/badges/last-month/tidyweather?color=green)](https://cran.r-project.org/package=tidyweather)
[![](http://cranlogs.r-pkg.org/badges/last-week/tidyweather?color=green)](https://cran.r-project.org/package=tidyweather)


# tidyweather
A tidyverse-style R package for agricultural weather analysis. Effortlessly summarize, and analyze weather data to support crop modeling, and climate-based decision-making in agriculture.


## Installation


### From CRAN

```r
install.packages('tidyweather')
```

### From GitHub (development version)

```r
remotes::install_github('byzheng/tidyweather')
```


## Configuration

The `tidyweather` package provides a flexible options system to customize default behaviors for weather analysis. You can configure settings like frost thresholds and other extreme weather parameters.

### Setting Options

Use `weather_options$get()` to view or and `weather_options$set()` to modify package settings:

```r
library(tidyweather)

# View all current options
weather_options$get()

# Set frost threshold to 2°C
weather_options$set("extreme.frost_threshold" = 2)

# View updated options
weather_options$get()
```

### Available Options

- **`require_full_year`**: Logical indicating whether to require a full year of data for calculations (default: TRUE)
- **`extreme.frost_threshold`**: Temperature threshold (°C) for frost analysis (default: 0)

### Reset Options

To restore all options to their defaults:

```r
weather_options$reset()
```

## Data Sources

This package includes example weather data sourced from the SILO database, operated by the Queensland Department of Environment, Science and Innovation.

> Weather data © Queensland Government, SILO climate database.  
> Licensed under the [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).  
> Source: [https://www.longpaddock.qld.gov.au/silo/](https://www.longpaddock.qld.gov.au/silo/)
