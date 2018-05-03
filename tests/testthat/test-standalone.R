# unit tests for standalone
context("standalone S3 class")

# checking constructor
test_that("constructor for standalone is ok", {


  ## duplicates
  expect_error(standalone(name = "equity",
                          equity(name = "EC", type = "HF",
                                 currency = "CHF"),
                          equity(name = "EC2", type = "HF",
                                 currency = "CHF")),
               "Duplicated")

  expect_error(standalone(name = "equity",
                          rate(name = "2YCHF", currency = "CHF",
                               horizon = "k"),
                          rate(name = "2YCHF", currency = "CHF",
                               horizon = "k", scale = 0.5)),
               "Duplicated")

  expect_error(standalone(name = "equity",
                          rate(name = "2YCHF", currency = "CHF",
                               horizon = "k"),
                          rate(name = "2YCHF", currency = "CHF",
                               horizon = "l")),
               "more than once")

  expect_error(standalone(name = "idk",
                          currency(name = "CHFUSD", from = "CHF",
                                   to   = "USD"),
                          rate(name    = "CHFUSD", currency = "CHF",
                               horizon = "k", scale = 0.5)),
               "currency")


})

test_that("check for standalone is OK", {


  ## a valid input
  cov.mat <- diag(rep(4, 4))
  name <- c("EURCHF", "equityCHF", "2YCHF", "AAACHF")
  colnames(cov.mat) <- name
  rownames(cov.mat) <- name
  attr(cov.mat, "base.currency") <- "CHF"

  mapping.table <- mappingTable(currency(name = "EURCHF",
                                         from = "EUR",
                                         to   = "CHF"),
                                equity(name     = "equityCHF",
                                       type     = "equity",
                                       currency = "CHF"),
                                rate(name     = "2YCHF",
                                     currency = "CHF",
                                     horizon  = "k"),
                                spread(name     = "AAACHF",
                                       currency = "CHF",
                                       rating   = "AAA"),
                                equity(name     = "equityCHF",
                                       type     = "equity",
                                       currency = "EUR",
                                       scale    = 0.5))

  initial.values <- list()

  initial.values$initial.fx <- data.frame(from             = "EUR",
                                          to               = "CHF",
                                          fx               = 1.05,
                                          stringsAsFactors = F)

  initial.values$initial.rate <- data.frame(time             = 1L,
                                            currency         = "CHF",
                                            rate             = 0.01,
                                            stringsAsFactors = F)

  mapping.time <- data.frame(time = 1L, mapping = "k", stringsAsFactors = F)

  mr <- marketRisk(cov.mat       = cov.mat,
                   mapping.table  = mapping.table,
                   initial.values = initial.values,
                   base.currency  = "CHF",
                   mapping.time   = mapping.time)

  expect_true(check(object = standalone(name = "a name",
                            equity(name = "equityCHF",
                                   type     = "equity",
                                   currency = "EUR",
                                   scale    = 0.5)),
        market.risk = mr))

  expect_true(check(object = standalone(name = "a name",
                                        equity(name = "equityCHF",
                                               type     = "equity",
                                               currency = "CHF")),
                    market.risk = mr))

  expect_false(check(object = standalone(name = "a name",
                                        equity(name = "equityCHF",
                                               type     = "equity",
                                               currency = "EUR")),
                    market.risk = mr))

})


test_that("isIn methods for standalone are OK", {

  s <- standalone(name = "example",
                  currency(name = "EURCHF",
                           from = "EUR",
                           to   = "CHF"),
                  equity(name     = "equityCHF",
                         type     = "equity",
                         currency = "CHF"),
                  rate(name     = "2YCHF",
                       currency = "CHF",
                       horizon  = "k"),
                  spread(name     = "AAACHF",
                         currency = "CHF",
                         rating   = "AAA"),
                  equity(name     = "equityCHF",
                         type     = "equity",
                         currency = "EUR",
                         scale    = 0.5))

  expect_true(equityIsIn(object = s, type = "equity", currency = "CHF"))
  expect_true(equityIsIn(object = s, type = "equity", currency = "EUR"))
  expect_false(equityIsIn(object = s, type = "hf", currency = "CHF"))

  expect_true(currencyIsIn(object = s, from = "EUR", to = "CHF"))
  expect_false(currencyIsIn(object = s, from = "USD", to = "CHF"))

  expect_true(rateIsIn(object = s, currency = "CHF", horizon = "k"))
  expect_false(rateIsIn(object = s, currency = "EUR", horizon = "k"))

  expect_true(spreadIsIn(object = s, currency = "CHF", rating = "AAA"))
  expect_false(spreadIsIn(object = s, currency = "EUR", rating = "AAA"))

})
