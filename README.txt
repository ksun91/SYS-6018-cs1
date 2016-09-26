README.txt

the aggregated data should be present in the MASTER_DATA.csv file.

However, you can reproduce the process of compiling this file from the raw data (located in the "data/" folder) by running the `load-Austin-data.R` file.

The `Austin-juris-map.R` file creates the graphic showing the KDE density plot, overlaid with boundaries of Austin's police districts, and points for the stores we are analyzing.

The `modeling.R` file contains all of our analysis and model selection on the MASTER_DATA.csv file.

The `making-ggplot-graphics.R` file, predicably, creates some ggplot2 graphics which we were considering including in our report.