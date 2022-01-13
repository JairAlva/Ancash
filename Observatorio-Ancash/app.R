library(shiny)
library(shinydashboard)
library(readr)
library(rio)
library(tidyverse)


segunda_vuelta <- import("https://github.com/JairAlva/Data-Libre/blob/main/Elecciones%20presidenciales%202021/Segunda%20vuelta/Resultados_2d_vuelta.dta?raw=true")

distritos_ancash <- segunda_vuelta |> filter(DEPARTAMENTO == "ANCASH") |>  
  mutate_at(vars(N_CVAS:VOTOS_VN), ~replace_na(.,0)) |> 
  group_by(UBIGEO, DISTRITO, PROVINCIA) |> 
  summarise(across(N_CVAS:VOTOS_VN, sum)) |> 
  mutate(votos_validos = N_CVAS - VOTOS_VB - VOTOS_VN,
         prc_Peru_Libre = VOTOS_P1/votos_validos*100,
         prc_Fuerza_popular = VOTOS_P2/votos_validos*100,
         prc_participacion = votos_validos/N_ELEC_HABIL*100) |> 
  select(UBIGEO, DISTRITO, PROVINCIA, prc_Peru_Libre, prc_Fuerza_popular,
         prc_participacion) |> 
  rename(Provincia = PROVINCIA) |> 
  mutate(Provincia = str_to_title(Provincia)) |> 
  ungroup()

ubigeos <- read_csv("https://raw.githubusercontent.com/JairAlva/Data-Libre/main/Ubigeos/TB_UBIGEOS.csv")

ubigeos <- ubigeos |>  select(ubigeo_reniec, ubigeo_inei, distrito)

distritos_ancash <- distritos_ancash |>  
  left_join(ubigeos, by = c("UBIGEO" = "ubigeo_reniec")) |>  
  mutate(distrito = str_to_title(distrito))


ui <- dashboardPage(
  dashboardHeader(title="Provincias"),
  dashboardSidebar(
    selectInput("Provincia", "Seleccione la provincia", choices = distritos_ancash |>  
                  select(Provincia) |> 
                  mutate(Provincia = str_to_title(Provincia)) |> 
                  distinct() |> 
                  arrange(Provincia))
  ),
  dashboardBody(
    fluidRow(box(plotOutput("participacion")))
  )
)

server <- function(input, output){
  
  output$participacion <- renderPlot({
    distritos_ancash |> 
      filter(Provincia == input$Provincia) |> 
      ggplot() + geom_col(aes(x=reorder(distrito, +prc_participacion), 
                              y= prc_participacion), fill = "pink") +
      theme_classic() +
      ylab("Porcentaje de participación") + xlab("Distrito") +
      coord_flip()
    
  })
  
}


shinyApp(ui, server)


