source("sstTestInputUi.R", local = T)
source("legalNoticesUi.R", local = T)

execMode <- getOption("sstModel.execMode")

preventQuitOnReload <- if(is.null(execMode) || !execMode) {
  NULL
} else {
  includeScript("js/preventQuitOnReload.js")
}

ui <- bootstrapPage(
  tags$head(
    includeHTML('www/favicons.html')
  ),
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      title = "SST Tool"
    ),
    shinydashboard::dashboardSidebar(
      collapsed = FALSE,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("SST Test", tabName = "sstTest", icon = icon("dashboard")),
        shinydashboard::menuItem("Legal Notices", icon = icon("balance-scale"), tabName = "legal")
      )
    ),
    shinydashboard::dashboardBody(
      includeCSS('www/style.css'),
      includeCSS('introjs/introjs.css'),
      preventQuitOnReload,
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "sstTest",
          div(id="sstTest"), # Anchor to use CSS selector in remove/add UI
          sstTestInput.pageLayout(
            sstTestInput.inputBoxes(
              sstTestInput.excelBox(),
              sstTestInput.numSimBox()
            ),
            sstTestInput.runSimButton()
          )
        ),
        shinydashboard::tabItem(tabName = "legal", legalNotices())
      ),
      includeScript('introjs/intro.min.js'),
      includeScript('js/introjsscript.js')
    )
  )
)
