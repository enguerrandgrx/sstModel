## Overview

This package implements the Swiss Solvency Test (SST) model.

## 1. Installation

Download either the source version (`sstModel_X.Y.Z.tar.gz`) or the binary version (`sstModel_X.Y.Z.zip`, Windows only) by clicking on the file you would like to download.

1. Open RStudio and click on the **Packages** tab in the lower right window.
2. Click on the **Install** button
3. Select **Install from: Package Archive File**
4. Click on the **Browse** button and find the archive file you just downloaded.
5. Optionnally choose in which library you want to install the package.
6. Click on the **Install** button and wait for the procedure to finish.
7. Run this command in your R console: `library(sstModel)`
8. Run this command in your R-console to check that everything is ok: `sstModel_check()`.

## 2. Quick User guide

Note that an Excel template provided by FINMA should be used and that another Excel worksheet would cause the simulations to fail.

### a. Command line

Execute an SST Simulation in the R console:

``` r
library(sstModel)
model <- excelToSstModel("path/to/excel/template.xlsm")
result <- compute(model, NUM_SIM)
```

Where `path/to/excel/template.xlsm` is the path to the Excel template on your computer and `NUM_SIM` is the desired number of simulations. Please note that very large number of simulations (> 1 000 000) will cause very long calculations.

Different solvency figures can be computed using the following methods.
``` r
marketValueMargin(result) # SST market value margin.
targetCapital(result, with.scenario = F) # SST target capital without scenario aggregation
targetCapital(result, with.scenario = T) # SST target capital with scenario aggregation
riskCapital(result, with.scenario = F) # SST one-year risk capital without scenario aggregation
riskCapital(result, with.scenario = T) # SST one-year risk capital with scenario aggregation
sstRatio(result, with.scenario = F) # SST ratio without scenario aggregation
sstRatio(result, with.scenario = T) # SST ratio with scenario aggregation
```

### b. Graphical User Interface

Simply run the following command in your terminal:

``` r
sstModel::launchDashboard()
```

A graphical user interface will show up. You can then use the **Interactive help** button to discover the interface.

