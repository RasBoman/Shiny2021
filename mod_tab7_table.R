
# Code for tab 7 (9.3.2021)

# Render the whole table

tab7_ui_main <- function(id) {
  tagList(dataTableOutput(NS(id, "taulukko")))
}
### Siirrä tämä myöhemmin välilehdelle 6
tab7Server <- function(id, df_to_use) {
  moduleServer(id, function(input, output, session) {
    output$taulukko <- renderDataTable({
      req(df_to_use())
      data.table(df_to_use())})
  })
}
