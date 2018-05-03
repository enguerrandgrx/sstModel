# Authors

This package, up to version 1.0.0, was authored by
- **Loris Michel** (@lorismichel): package implementation and documentation,
- **Melvin Kianmanesh Rad** (@melvinkian): package implementation and excel parser,
- **Adrien Lamit** (@alamit): GUI implementation,

for the Swiss Financial Market Supervisory Authority FINMA.

The authors agree **Michael Schmutz** to be the maintainer of this package.


# News

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

Note that Semantic Versioning is only used starting from version 1.0.0 The 0.x.y versions are development versions and have been used to keep the changelog clearer, see [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2018-03-28

### Added

- When Rtools is missing, it is not possible to download the excel output file. In this case, the error message was "Internal server error". Replaced this error message on GUI and package by a humanly understandable message indicating that Rtools is missing. Note that the results are still available on the GUI or using `summary.sstOutput` in the package. Implemented by **Adrien Lamit** (@alamit) and **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #478.

- Final description information for authors, maintainer and copyright-holder in DESCRIPTION file. Implemented by **Loris Michel** (@lorismichel) for FINMA in #473.

- Error message when the same MVM is provided more than once as input. Implemented by **Loris Michel** (@lorismichel) for FINMA in #471.

### Changed

- In `standalone` and `mappingTable`, replaced `rbindlist` from the `data.table` package by `do.call("rbind", ...)` in order to avoid a unwanted cast to a `data.table` object. Indeed, we do not want to cast unecessarly to `data.table` as this can introduce unwanted side-effects. Implemented by **Loris Michel** (@lorismichel) for FINMA in #471.

- Forced evaluation of parameters in `generateFunction.portfolio` to have a safe closure. Implemented by **Loris Michel** (@lorismichel) for FINMA in #471.

- `macro.economic.scenarios` are now provided directly in the constructor `sstModel` instead of `marketRisk`. Implemented by **Loris Michel** (@lorismichel) for FINMA in #459.

- Updated color scale for `sstRatio` box in dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #481:
  - red: `sstRatio < 33`
  - orange: `33 <= sstRatio < 80`
  - yellow: `80 <= sstRatio < 100`
  - green: `sstRatio >= 100`

- Changed the color of all highlighted key figures boxes to blue. Implemented by **Adrien Lamit** (@alamit) for FINMA in #481.

### Fixed

- Bug in the reordering step when only market risk is computed. Implemented by **Loris Michel** (@lorismichel) and **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #482.

- Unexpected error when only one scenario provided for the stressed Gaussian copula aggregation method (see `conditionalReordering`). Implemented by **Loris Michel** (@lorismichel) for FINMA in #466.

- Risk bearing capital box in dashboard now displays `sstOutput$rtkg` instead of `targetCapital(sstOutput)`. Implemented by **Adrien Lamit** (@alamit) for FINMA in #481.


## [0.5.5] - 2018-03-22

### Fixed

- Unexpected error when uploading the excel input on the dashboard and when launching the executable version. (`generateError` was not exported). Implemented by **Loris Michel** (@lorismichel) for FINMA in #457.


## [0.5.4] - 2018-03-21

### Added

- Error message if sensitivities of health and life insurance risks are not all provided in the reference currency. Implemented by **Loris Michel** (@lorismichel) for FINMA in #429.

- Parameters evaluation need to be forced to have safe `valFunction` closures  in all file `*-valuation.R` files for `marketItem` objects. Implemented by **Loris Michel** (@lorismichel) for FINMA in #454.

### Changed

- Allow life insurance sensitivities of different signs, which were all transformed to positive volatilities (in `valInfo.life`). This allows to have different signs in the linear combination of life insurance risk factors, allowing gains for some scenarios. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #452.

- Participations are comonotonically added to market risk to obtain total market risk by exactly matching the ranks of market and participation simulations (in `aggregateRisks`). Before, re-sampling from the empirical distribution of the market risk was used. Implemented by **Loris Michel** (@lorismichel) for FINMA in #434.

- In `generatFunction.portfolio` restricted function to generate complete valuation function for all market items. Implemented by **Loris Michel** (@lorismichel) for FINMA in #429.

- Participations are simulated instead of using theoretical quantile approximation (in `compute.sstModel` re-introduced `compute.participationRisk`). Implemented by **Loris Michel** (@lorismichel) for FINMA in #427.

- Standalone valuation evaluations and valuation terms are now handled one-by-one to optimize memory consumption and avoid reaching maximal string length (in `compute.marketRisk`). Implemented by **Loris Michel** (@lorismichel) for FINMA in #427.

- In the excel parser (`excelToSstModel`), parsing of macro-economic scenarios to the internal data structure (`macroEconomicScenarios`) is now done with exact ordering of the risk factors (same order as in the market risk definition). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #430.

- In the excel output (`write.sstOutput`), information are now displayed in millions of base currency. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #418.

- Naming conventions for market risk and participations in the summary, names of simulations to be saved on the dashboard and excel output (see `translate.sstOutput`, `summary.sstOutput`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #418.

- Market-Value-Margin computation is done using the expected shortfall of the total market risk instead of the expected shortfall of the total market risk without participations (in `marketValueMargin`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #413.

- Labels of highlighted figures updated: Target capital -> Risk bearing capital, Risk capital -> One-year risk capital. Implemented by **Adrien Lamit** (@alamit) for FINMA in #453.

- README.md to match new installation procedures. Implemented by **Adrien Lamit** (@alamit) for FINMA in #411.

- Results are now displayed in millions of base currency in the dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #409.

- Key figures boxes are now displayed correctly and resized according to window size. Implemented by **Adrien Lamit** (@alamit) for FINMA in #409.

- Titles of sections (w/ or w/o scenario) are now outside of the boxes for better display with small windows (e.g. default size of Rstudio viewer). Implemented by **Adrien Lamit** (@alamit) for FINMA in #409.

### Fixed

- Restrict sensitivities for health insurance risk to be positive, due to the fact that they are interpreted as standard deviations. Implemented by **Loris Michel** (@lorismichel) for FINMA in #423.

- During excel parsing (`excelToSstModel`), an unnecessary participation risk was created in case the exposure was `0`. Now no participation is created in such case. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #418.

- During the excel parsing (`excelToSstModel`), unexpected error in case no expected financial result input was provided. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #413.

- During the excel parsing (`excelToSstModel`), unexpected error in case no asset prices input was provided. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #413.

- During the excel parsing (`excelToSstModel`), unexpected error in case no market item, an error message is displayed instead. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #413.


## [0.5.3] - 2018-03-12

### Fixed

- Bug while creating comments in `write.sstOutput`, due to global option `stringsAsFactors`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #401.

- Bug with filename in Rstudio window mode on windows. Variable name of the shiny output must have the extension, this is a bug from Rstudio 's built in browser. Implemented by **Adrien Lamit** (@alamit) for FINMA in #401.


## [0.5.2] - 2018-03-12

### Removed

- Deleted FINMA logo from the project folder. Implemented by **Adrien Lamit** (@alamit) for FINMA in #399.


## [0.5.1] - 2018-03-09

### Added

- Interactive help to explain functionalities of the dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #377.


## [0.5.0] - 2018-03-06

### Added

- Prepared for interactive help to explain the dashboard using an interactive help button, still unstable with Rstudio's viewer. Implemented by **Adrien Lamit** (@alamit) for FINMA in #371.

- Title to display description of each row of the results table when mouse is positioned over the row. Implemented by **Adrien Lamit** (@alamit) for FINMA in #371.

- The excel output now also contains comments in cells for further details. Those comments are generated while taking the summary of the output. The functionality is modular, changing the summary is sufficient for updating the comments (see `summary.sstOutput`, `write.sstOutput`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #369.

- It is now possible to modify `nhmr` (non hedgeable market risk scale), `alpha` (expected shortfall quantile) and `sup` (should the upper expected shortfall be taken?) in any solvency figure function: `marketValueMargin`, `riskCapital`, `targetCapital`, `sstRatio` as well as in `summary.sstOutput` and `write.sstOutput` via the ellipsis `...` arguments. Those argument will recursively be passed on to all `marketValueMargin` and `expectedShortfall` calls. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #365.

- Decorator constructors for initial values of FX and interest rates as well as time mappings that are then used in `marketRisk`, this has allowed to remove global options `STRINGSASFACTORS` to be set at loading time. Implemented by **Loris Michel** (@lorismichel) for FINMA in #361.

- Comments for MVM parsing and computations as well as for fixed income parsing and initial spreads computations (in `excelToSstModel`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #356.

- Empirical Cumulative Distribution Function parsing is now implemented (see `excelToSstModel`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #356.

- It is now possible to parse an excel file without defining the cost of capital, provided that no MVM life is defined. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #356.

- Helper S3 method `translate` for class `sstOutput` returns understandable names for the columns of `sstOutput$simulations`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #353.

- Different MVM (life, health, non-life) to the summary of the output and thus to the excel output (see `summary.sstOutput`, `write.sstOutput`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #347.

- SST Ratio is now expressed in percent in the excel output (see `write.sstOutput`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #347.

- Error message if a risk factor value is defined twice for a macroeconomic scenario (see `macroEconomicScenarios`). Implemented by **Loris Michel** (@lorismichel) for FINMA in #341.

- Error handling in the excel parser (`excelToSstModel`) is now also implemented for all market and insurance items, as well as for scenarios, macroeconomic scenarios and copula aggregation parameters. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #339.

- Errors and warnings during excel parsing are now displayed all at once in the dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #335.

- Customized error message when memory is insufficient to compute the asked number of simulations on the dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #335.

- Possibility to download the full warning log for the excel parsing on the dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #335.

### Changed

- Credit risk parsing is simplified (in `excelToSstModel`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #373.

- Market-Value-Margin is now computed using same continuous rates as those defined as market risk initial values (see `excelToSstModel`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #373.

- Market-Value-Margin is computed continuous compounded rates inputs (see `mvmLife`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #373.

- Display of results tables in the dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #371.

- Signs are now all considered positive for losses and negative for gains in the summary and the output (see `summary.sstOutput`), except for scenarios and macro-economic scenarios. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #365.

- `write` is not a generic S3 method anymore. This was in conflict with other functions (such as `write.csv`). `write.sstOutput` is now a function. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #360.

- Updated the risk aggregation procedure towards a tree-based aggregation involving first a stochastic comonotonic sampling from the market risk simulations and the participations (sum-aggregated) followed by a conditional reordering with the insurances risks. Implemented by **Loris Michel** (@lorismichel) for FINMA in #358.

- In the excel parser (`excelToSstModel`): unified warning and error messages for MVM life, health and non-life. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #356.

- Error handling for macro-economic scenarios were not implemented at construction but during execution. Now error messages related to macro-economic scenarios will appear directly at construction (in `sstModel`, `macroEconomicScenarios`). Implemented by **Loris Michel** (@lorismichel) for FINMA in #341.

- Error handling for conditional reordering was not completed at construction. Added missing conditions (in `sstModel`). Implemented by **Loris Michel** (@lorismichel) for FINMA in #341.

- Option `stringAsFactors = F` is now reset properly in `excelToSstModel` using `on.exit`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #339.

- Shiny does not terminate R session anymore when running with the option "sstModel.execMode". The shiny app still quit. Implemented by **Adrien Lamit** (@alamit) for FINMA in #335.

- Updated the logo for target capital in Dashboard to a line chart logo, this is more pertinent. Implemented by **Adrien Lamit** (@alamit) for FINMA in #335.

- Progress bar when computing simulations now have three steps: excel parsing, computing simulations and rendering results. Implemented by **Adrien Lamit** (@alamit) for FINMA in #335.

- Names of checkboxes to include simulations in the output excel file are now humanly readable. Implemented by **Adrien Lamit** (@alamit) for FINMA in #335.

### Removed

- Global option `STRINGSASFACTORS` set at package loading. Implemented by **Loris Michel** (@lorismichel) for FINMA in #361.

- Unnecessary warnings, coercion warning in `marketRisk` and zero scale in `spread`, which are always there for GOVI ratings. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #360.

- Number of simulations has no more upper bound in dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #335.

### Fixed

- Participations were aggregated twice (in `riskCapital`), removed expected shortfall of participations in the one-year risk capital computation. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #363.

- Implementation of Market-Value-Margin for life (in `mvmLife`) was wrong and is now correctly implementated. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #370.

- In case of Empirical Cumulative Distribution Function (ECDF) input for non-life risk, the constructor (`nonLifeRisk`) was asking for the first probability to be `0`. This was wrong due to right-continuity of ECDFs and didn't enforce that the last jump towards probability `1` was computable. This is now corrected along with the simulation procedure (see `simulate.nonLifeRisk`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #365.

- Error while parsing tables with empty columns. In the excel parser (`excelToSstModel`), explicitly casted numerical columns to numeric to avoid casting errors and warnings while using (due to `data.table::dcast` or `data.table::melt`) occurring while reading empty columns. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #356.

- Standalone fixed income valuation term was not appearing in the output excel, dashboard and summary due to a typo in `summary.sstOutput` is now solved. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #335.

- Bug in the excel parser (`excelToSstModel`) in case no initial spreads were provided, `data.table` was throwing an unexpected error, now solved. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #345.

- Bug in the excel parser (`excelToSstModel`) in case of no scenarios (single Gaussian copula) used for the reordering, now solved. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #339.


## [0.4.1] - 2018-02-27

### Changed

- Updated cached file `.Rbuildignore` to keep only relevant folders and files in the bundled package. Implemented by **Loris Michel** (@lorismichel) for FINMA in #337.

- In the excel parser (`excelToSstModel`) symmetrisation of the correlation matrix of market risk factors is done using the mean of the matrix and its transposed instead of using the upper triangle of the matrix. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #332.

- In the excel parser (`excelToSstModel`), a warning is thrown instead of an error in case basic checks for covariance matrix, such as perfect symmetry, do not pass. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #332.

- Use argument `symmetric = T` in `eigen`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #332.

- Require exactly symmetric covariance matrix of market risk-factors (in `marketRisk`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #332.

- Relative tolerance in Newton-Raphson instead of absolute tolerance. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #331.

- Warning instead of error in the excel parser (`excelToSstModel`) in case of wrong initial spreads, absolute tolerance is now obtained using a relative tolerance of `1e-4` multiplied with the total market value. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #331.

### Fixed

- Warnings were not displaying in `excelToSstModel` if there was no error, now fixed. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #331.


## [0.4.0] - 2018-02-26

### Added

- In the dashboard, standalone market risk for different valuation terms. (`compute.sstModel` is now called with the `nested.market.computations = T` parameter). Implemented by **Adrien Lamit** (@alamit) for FINMA in #329.

- Modularization of the excel output generation. The excel output (in `write.sstOutput`) now builds up automatically from the summary of the output (`summary.sstOutput`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #319.

- Complete `summary` for `sstOutput` with list of key figures, standalone market risks, standalone insurance risks and scenarios. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #319.

- Aggregated market risk and participations to the output of `compute.sstModel`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #319.

- Download results button now has checkboxes add possibility to save simulation vectors (see `keep` parameter in `write.sstOutput`). Implemented by **Adrien Lamit** (@alamit) for FINMA in #317.

- Function `sstModel_check()`. This function checks for any packages potentially used by the sstModel package present in the user's libraries, checks those packages build versions and asks the user if he wants to reinstall those packages. Implemented by **Adrien Lamit** (@alamit) for FINMA in #303.

- Possibility to run the shiny app in `execMode` by setting the corresponding option in R. This allows to prevent exposing this explicitly to the package user. Only the executable version uses this option. When `execMode` is set to `TRUE`, the shiny app and the R session are shutdown when the user closes the browser window to prevent background processes to keep running in the executable version. Implemented by **Adrien Lamit** (@alamit) for FINMA in #301.

- Favicon in the shiny app. Implemented by **Adrien Lamit** (@alamit) for FINMA in #301.

- Initial spread computations for bonds in `initialSpread` using Newton-Raphson algorithm implemented in `newtonRaphson`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #293.

- Keyword based excel parsing in `excelToSstModel`. A new set of global variables is defined to simplify maintenance of the parser. Error handling is enhanced, exact position of the errors and warnings on the excel sheet is determined and tracked using an error log. Error messages are enhanced to be understandable by the excel user. Multiple errors and warning displayed at once to simplify input check for the user. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #293.

- Initial spreads, MVM life, credit risk, expected insurance results now computed in the excel parser (`excelToSstModel`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #293.

- Management in functions `.onLoad` and `.onUnload` of the default option parameter `stringsAsFactors` so that the option is set to `FALSE` when the package is loaded and the old state of the option is recovered when the package is unloaded. Implemented by **Loris Michel** (@lorismichel) for FINMA in #291.

- Constructor of `marketRisk` has a new parameter `base.currency` specifying to which currency we expect the FX rate in the `mapping.table` argument to map to. We also require now directly the covariance matrix `cov.mat` as input of the `marketRisk` constructor. This matrix should additionally have an attribute named `base.currency` specifying the currency in which the covariance matrix is expressed (i.e. to which the FX risk-factor map to). Implemented by **Loris Michel** (@lorismichel) for FINMA in #279.

- A new function `changeBaseCurrency`allowing change of base-risk factors covariance matrix and update of mapping.table according to a change of base currency. Implemented by **Loris Michel** (@lorismichel) for FINMA in #279.

- A new function `conditionalReordering` implementing the conditional reordering algorithm with stressed multivariate normal copulas. Implemented by **Loris Michel** (@lorismichel) for FINMA in #279.

- A new class `macroEconomicScenarios` to allow defining a specific realization of the market risk-factor vector (called macroeconomic scenarios). It is possible to valuate a portfolio and obtain corresponding change in RBC according to these scenarios with a `compute` method on `macroEconomicScenarios`. Basic S3 methods for this class (`print`,`summary`,``format`) were also added. Implemented by **Loris Michel** (@lorismichel) for FINMA in #277 and #279.

- The `portfolio` class has now two new methods `generateExpression` and `generateFunction` allowing to generate the total valuation expression or valuation function for a given list of `marketItems` in a portfolio. The `valFunctions` for classes `asset`, `cashflow`, `liability`, `assetForward`, `fxForward` and `delta` were updated accordingly to integrate with these new methods. Additionally, two helper functions `itemListToExpression` and `itemListToFunction` were implemented. Implemented by **Loris Michel** (@lorismichel) for FINMA in #277 and #279.

- Possibility to define non-life insurance risk via an Empirical Cumulative Distribution Function. Implemented by **Loris Michel** (@lorismichel) for FINMA in #276.

- Warnings are now triggered when unrealistic volatilities potentially yielding infinite values are provided for the log-normal simulation parameters. Implemented by **Loris Michel** (@lorismichel) for FINMA in #276.

- Message when loading the package linking to the `NEWS.md` file. Please note that this functionality does not work when using `devtools::load_all()`. Implemented by **Loris Michel** (@lorismichel) for FINMA in #253.

- Copyright Notice is displayed when the package is installed. Implemented by **Adrien Lamit** (@alamit) for FINMA in #287.

- Copyright notices of every libraries are displayed in the "Legal Notices" tab of the dashboard. Implemented by **Adrien Lamit** (@alamit) for FINMA in #287.

- `launchDashboard` now launches `shiny` quietly. Prevents status messages from shiny from being displayed. Implemented by **Adrien Lamit** (@alamit) for FINMA in #287.

- `launchDashboard` now has an option `shiny.quitOnSessionEnd` that allows the shiny app to quit when the browser displaying it is closed. Used for the executable version. Implemented by **Adrien Lamit** (@alamit) for FINMA in #287.

### Changed

- Download and new simulation buttons are now drawn in the sidebar when the simulation results are ready. Implemented by **Adrien Lamit** (@alamit) for FINMA in #317.

- Participations are now evaluated comonotonically with the market risk. Implemented by **Loris Michel** (@lorismichel) for FINMA in #306.

- Market-Value-Margin inputs for life, health and non-life instead of for their sum (see `portfolio`). Implemented by **Loris Michel** (@lorismichel) for FINMA in #291.

- Sensitivites in `delta`, `life`, `health` and effects in `scenarioRisk` are now required to be explicitely defined in the base currency. Implemented by **Loris Michel** (@lorismichel) for FINMA in #291.

- Some of the parameters in `sstModel` that were portfolio specific were moved to a new argument `portfolio.parameters` in the constructor of `portfolio`. Implemented by **Loris Michel** (@lorismichel) for FINMA in #279.

- Max file size for shiny uploads set to 100MB, was 5MB by default. Implemented by **Adrien Lamit** (@alamit) and **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in  #267.

### Removed

- The function `launchDashboard` does not take parameters anymore, `execMode` option has to be used to customize the behaviour of the shiny app for the executable version. Implemented by **Adrien Lamit** (@alamit) for FINMA in #301.

- Warnings on integer casting of variable `time` in constructor of class `cashflow` and `liability`. Implemented by **Loris Michel** (@lorismichel) for FINMA in #279.

- `ggplot2` and `DT` packages from dependencies for licensing compatibility. Plots are now drawn using the `graphics` package which is preinstalled with R and tables are rendered using shiny default table rendering. Implemented by **Adrien Lamit** (@alamit) for FINMA in #256.

### Fixed

- Bug in standalone constructor `standalone` when `list.arg = T` is now fixed. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #293.

- Initial spread values are now provided in the constructor of `cashflow`. Implemented by **Loris Michel** (@lorismichel) for FINMA in #275.

- Removed warnings for negative `liability` and negative `cashflow` and grouped checks in constructor of `asset`, `cashflow` and `liability`. Implemented by **Loris Michel** (@lorismichel) for FINMA in #272.

- Corrected temporary sign patch in implementation of `valInfo`, `valExpression` and `valFunction` for `assetForward` and `fxForward` Implemented by **Loris Michel** (@lorismichel) for FINMA in #271.

- Incorrect error message when simulation was < 1'000, which was displaying the error message for simulations > 5'000'000. Implemented by **Adrien Lamit** (@alamit) for FINMA in #255.


## [0.3.0] - 2018-02-01

### Added

- Option to save in `.xlsx` format additional simulation vectors from `sstOutput`, using `write` with parameter `keep`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #249.

- Possibility to save `.xslx` files in `write` with design and adaptive output depending on the risks defined as input. The excel output will be different based on which risks are defined and the presence or absence of participations. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #249.

- Interface getters like `getMarketRisk`, `getDrbc`, can now be used to compute expected shortfall by reference. This reduces the calls to `colnames(sstOutput$simulations)`, increases readability and makes the code more modular. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #249.

- Standalone for each life and health insurance risk factor are now saved in the output using the closed form formula for the expected shortfall of normal random variables. Located in `sstOutput`, the output of `sstModel` under named vectors `sstOutput$life.standalones` and `sstOutput$health.standalones`. Note that they are present only if, respectively, those risks are defined (in `portfolio`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #246.

- Logical argument `list.arg` in constructors `mappingTable` and `standalone` to allow passing a list of `riskFactor` in those constructors through the ellipsis `...`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #241.

- Possibility to compute market risk separately for each valuation term (asset prices, liability cash flows, fixed income cash flows, FX and asset forwards, delta term). This is available as an option in `compute.sstModel` through the parameter `nested.market.computations`, this has involved a new organization of the core computations in `compute.marketRisk`. Implemented by **Loris Michel** (@lorismichel) for FINMA in #241.

- Error handling in the dashboard, now the program won't crash if there is an error during the simulations. Implemented by **Adrien Lamit** (@alamit) for FINMA in  #242.

- Checks for semi-positive definite correlation matrices and corresponding error messages (see `marketRisk`, `lifeRisk`, `healthRisk` and `sstModel`). Implemented by **Loris Michel** (@lorismichel) for FINMA in #239.

- Principal components can be defined for rates. A new `riskFactor` `pcRate` can be used to create base components that can be then reference in the `riskFactor` `rate`. This allows to model yield curves by providing their principal component decomposition. Implemented by **Loris Michel** (@lorismichel) for FINMA in #239.

### Changed

- Life sensitivities (in `life`) are now considered to be values-at-risk instead of expected shortfalls, computations (in `valInfo.life`) are modified accordingly. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #246.

- All outputs of `compute` S3 methods are now harmonized to be named `data.table` objects. Implemented by **Loris Michel** (@lorismichel) for FINMA in #241.

- Correction of checks in the constructor of `mappingTable` to force all principal components defined in the `mappingTable` in a specific currency to be used to define rates in that given currency. Implemented by **Loris Michel** (@lorismichel) for FINMA in #241.

### Fixed

- Unexpected error in `format.sstModel`. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #241.

- Global variable need to be defined with `utils::globalVariable` to pass the CRAN checks due to `data.table` API. All variables used in `data.table` expression are declared in file `globalVar.R`. Implemented by **Loris Michel** (@lorismichel) for FINMA in #239.

- `launchDashboard` default browser is now Window mode instead of RStudio's setting. Linked with `shiny`'s `Connection reset by peer` bug in browser and viewer pane modes. (@alamit #238).


## [0.2.0] - 2018-01-11

### Added

- Factor for non-hedgeable market risk in market value margin computation can be set in `sstModel` and can be read from the excel sheet (see `sstModel`, `compute.sstModel`, `marketValueMargin` and `excelToSstModel`). Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #231.

### Fixed

- The warning message for the sign of a liability cash flow was wrong. A positive liability cash flow is considered as a loss (real liability) while a negative liability cash flow is a gain. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #236.


## [0.1.1] - 2018-01-10

### Changed

- Negative fixed cash flows are now allowed. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #223.

- Positive liability cash flows are now allowed. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #223.

### Fixed

- `write` generic dispatch not working for `write.sstOutput`, added generic definition. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #226.

- Liability cash flows were evaluated with the wrong sign convention. Implemented by **Melvin Kianmanesh Rad** (@melvinkian) for FINMA in #223.

## [0.1.0] - 2018-01-09

First development release.

### General

The following files were added by **Loris Michel** (@lorismichel) and **Adrien Lamit** (@alamit) for FINMA.

- `README.md` file giving installation instructions and a quick user guide.
- `LICENSE` file specifying that the software is licensed under GPL-3 License.
- `DESCRIPTION` file specifying `CRAN` details: authors, version, dependencies and more.
- `NOTICE` file providing the GPL-3 required copyright notice.
- `CONTRIBUTING.md` file specifying how one should behave to contribute to the project.

### Package

The following features were implemented by **Loris Michel** (@lorismichel) and **Melvin Kianmanesh Rad** (@melvinkian) for FINMA.

This version allows computations of market risk, life, health and non-life insurance risks, their aggregation using the reordering algorithm with a Gaussian copula and the computation of solvency figures based on those computations.

Detailed list of features:

- Software architecture, S3 classes hierarchy.
- Constructors for S3 classes for company portfolio exposures and sensitivities, called items, `asset`, `liability`, `cashflow`, `assetForward`, `fxForward`, `delta`, `participation`, `life`, `health`; basic S3 methods `format`, `print`, `summary` for those classes.
- Constructors for S3 classes for risk-factors definitions: `currency`, `rate`, `spread`, `equity`; scaled risk-factors, i.e. constant times another risk-factor can be defined.
- Constructors for S3 classes regrouping risk-factors: `mappingTable`, `standalone`; basic S3 methods `format`, `print`, `summary`.
- Constructor for S3 class for market risk definition `marketRisk`; basic S3 methods `format`, `print`, `summary`.
  * Risk-factors are multivariate normally distributed.
  * Risk-factors are defined through a `mappingTable` object.
  * Times-to-maturities are projected.
  * Getter methods to easily access attributes: `getInitialFX`, `getInitialRate`, `getInitialSpread`, `getMappingTime`, `getEquityName`, `getEquityId`, `getEquityScale`, `getCurrencyName`, `getCurrencyId`, `getCurrencyScale`, `getRateName`, `getRateId`, `getRateScale`, `getSpreadName`, `getSpreadId`, `getSpreadScale`, `getDeltaId`.
- `simulate` method for class `marketRisk`, returns a `data.table` or `matrix` of simulations for the base market risk-factors.
- Constructor for S3 class for scenario definition `scenarioRisk`; basic S3 methods `format`, `print`, `summary`.
  * Distribution for scenarios is a discrete random variable taking a finite number of values, those values are the loss for the corresponding scenario.
- `simulate` and `compute` methods for `scenarioRisk`, both returning simulations for the scenarios.
- Constructor for the S3 class for life insurance risk definition `lifeRisk`; basic S3 methods `format`, `print`, `summary`.
  * Risk-factors are multivariate normally distributed.
  * Expected shortfall of each risk-factor for some quantile is used for volatility computation.
- Constructor for the S3 class for health insurance risk definition `healthRisk`; basic S3 methods `format`, `print`, `summary`.
  * Risk-factors are multivariate normally distributed.
- Constructor for the S3 class for non-life insurance risk definition `nonLifeRisk`; basic S3 methods `format`, `print`, `summary`.
  * Allows input of user-defined simulations or parameters `mu` and `sigma` for negative lognormal distribution.
  * `simulate` method returns bootstrap from the user-defined vector of simulations or normal simulations with mean `mu` and variance `sigma^2`.
- `compute` method for `nonLifeRisk` returns i.i.d. with replacement bootstrap samples from the user-defined vector of simulations or negative log-normal simulations with parameters `mu` and `sigma`.
- `check` boolean method for the coherence of each above-mentioned portfolio item with corresponding risk definition object.
- `valInfo` methods for each item returning necessary information to valuate item with knowledge of corresponding risk-factor realization.
- `compute` methods for `lifeRisk` and `healthRisk` using `valInfo` to valuate output from `simulate`.
- `valFunction` method for each item returning a function of the risk-factors, the valuation function.
- `valExpression` method for each market item (`asset`, `liability`, `cashflow`, `assetForward`, `fxForward`, `delta`); returns a `data.table` expression for aggregating normal simulations **by reference** on the table of normal market risk-factors simulations produced using `simulate.marketRisk`; those methods also return corresponding restricted expressions if provided S3 object `standalone` in addition.
- `compute` method for `marketRisk`, taking as parameter a list of market items, and using `valExpression` to valuate output from `simulate` **by reference** using `data.table` syntax.
- Constructor for S3 class `portfolio` listing different portfolio items and base currency for the portfolio; basic S3 methods `format`, `print`, `summary`.
- Constructor for S3 class `sstModel` containing portfolio, list of risks and parameters necessary for the Monte Carlo simulations; basic S3 methods `format`, `print`, `summary`.
- Function `excelToSstModel` to parse the excel input template to an `sstModel` object.
- `compute` method for S3 class `sstModel` returns an `sstOutput` S3 object with the aggregated simulations for risk-bearing capital along with the standalone market and insurance risks vectors of simulations. Aggregation is done using helper function `aggregateRisks` doing aggregation of market, life, health and non-life risks based on the **reordering algorithm for a Gaussian copula**; all steps are done **by reference** using `data.table` syntax.
- Helper functions `expectedShortfall` and `valueAtRisk` to compute expected shortfall and value at risk of a vector of simulations.
- `marketValueMargin`, `targetCapital`, `riskCapital`, `sstRatio` S3 methods for class `sstOutput` to compute solvency figures **by reference**.
- `write` S3 method for class `sstOutput` to write a csv summary of the `sstOutput` containing 0.01-expected shortfalls of standalone risks, means and solvency figures with and without scenario aggregation.
- Roxygen2 documentation of all functions with description, parameters, returns, see also and some examples.
- Error messages and warnings implemented for all constructors.

### GUI

The following features were implemented by **Adrien Lamit** (@alamit) for FINMA.

- `shiny` and `shinydashboard` implementation.
- CSS styling of the GUI to match FINMA's corporate design.
- Input parameters page in the GUI.
- Checks for the input parameters to prevent setting trivially wrong inputs.
- Plotting functionality for simulation results.
- Run simulation button that computes the simulation with the given parameters with the sstModel package.
- Boxes to display numeric simulation results.
- Table box to display standalones simulation results.
- New simulation button to restart a simulation in the GUI.
- Download simulation results button to save simulation results in a .csv file.
- Copyright notice page as required in GUI's by the GPL-3 License.
