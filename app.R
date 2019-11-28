library(shiny)
library(tidyverse)
library(kableExtra)
library(formattable)
library(shinythemes)
library(RYandexTranslate)
library(stringr)
library(keras)

model <- application_inception_v3(weights = "imagenet")
api_key="trnsl.1.1.20191124T054419Z.9fb1185acab407e8.ee66e907db9042c58ef46a487c9df9677630ebd4"
options(shiny.autoreload = TRUE) # con Ctrl + Shift + enter , se guarda recarga la app

### SOLUCION ERROR CON RYandex ! en https://github.com/mukul13/RYandexTranslate/issues/2
translate = function (api_key, text = "", lang = "") 
{
  url = "https://translate.yandex.net/api/v1.5/tr.json/translate?"
  url = paste(url, "key=", api_key, sep = "")
  if (text != "") {
    url = paste(url, "&text=", text, sep = "")
  }
  if (lang != "") {
    url = paste(url, "&lang=", lang, sep = "")
  }
  url = gsub(pattern = " ", replacement = "%20", x = url)
  d = RCurl::getURL(url, ssl.verifyhost = 0L, ssl.verifypeer = 0L)
  d = jsonlite::fromJSON(d)
  d$code = NULL
  d
}


################## UI #############################
ui <-fluidPage(theme = shinytheme("cosmo"),
               #Para desacativar la barra de progreso en el fileinput
               #tags$style(".shiny-file-input-progress {display: none}"),
               
               fluidRow(column(width = 8,offset = 2, 
                               titlePanel("Reconociemiento de Imagen", 
                                          windowTitle = "ImageNet Patricio Said"),
                               p("Por Patricio Said - Noviembre 2019"))),
               fluidRow(column(width = 8, offset = 2, 
                               sidebarPanel(width = 12,
                                            fileInput(inputId = "imagen", 
                                                      label = "Seleccionar imagen (formato jpg o jpeg)",
                                                      buttonLabel = "Buscar...",
                                                      placeholder = "Ningún archivo seleccionado", 
                                                      accept = c("jpg", "jpeg")
                                            ) 
                               ),
               )),
               fluidRow(column(width = 3,offset = 3,
                               imageOutput(outputId = "plot_foto")
               ),
               column(width = 4,offset = 0,
                      htmlOutput("tabla_resultados"))),
               fluidRow(column(width = 12, offset = 0,
                               hr(),
                               p("Modelo ML de ",
                                 a(href="https://keras.rstudio.com/", "Keras"), 
                                 ", traducción de resultados con ", 
                                 a(href= "https://translate.yandex.com/" , "Yandex Translate"),
                                 ". Para más información ver ", 
                                 a(href= "https://patosaid.netlify.com/post/keras-en-shiny-app/",
                                   "post"),align= "center" )))
               
               
)


################## SERVER ############################
server <- function(input, output) {
  
  
  output$plot_foto <- renderImage({
    req(resultado())
    list(src = input$imagen$datapath,
         type = input$imagen$type,
         width = "100%")
  })
  
  resultado <- reactive({
    req(input$imagen)
    withProgress(message = 'Subiendo imagen', value = 1, {
      img <- image_load(input$imagen$datapath, target_size = c(299,299))
      ##img <- image_load(input$imagen$datapath, target_size = c(224, 224))
      x <- image_to_array(img)
      x <- array_reshape(x, c(1, dim(x)))
      ##x <- imagenet_preprocess_input(x)
      x <- inception_v3_preprocess_input(x)
      preds <- model %>% predict(x)
      tabla <- imagenet_decode_predictions(preds, top = 7)[[1]]
      tabla <- tabla %>%             
        mutate(score = round(score * 100, 1)) %>% 
        mutate(score = color_bar("lightgreen")(score)) %>%
        select(score, class_description)
    })
    tabla
  })
  
  output$tabla_resultados <- renderPrint({
    req(traducido()) 
    withProgress(message = 'Calculando', value = 1, {
      
      traducido()%>% 
        kable(escape = F, 
              col.names = c( "Puntaje", "Objeto"), 
              align = c("r","l")) %>% 
        kable_styling(bootstrap_options = c("hover"), full_width = T) %>%
        column_spec(1, width = "5cm") %>% 
        column_spec(2, width = "4cm")
    })
  })
  
  traducido <- reactive({
    req(resultado())
    withProgress(message = 'Traduciendo salida', value = 1, {
      tabla <- resultado()
      tabla <- tabla %>% 
        mutate(espacio = str_replace_all(class_description, "_"," ")) %>% 
        select(score, espacio)
      for (i in 1:nrow(tabla)) {
        
        tabla[i,2] <- translate(api_key,text= tabla[i,2],lang="en-es"  )[2]
      }
      tabla
    })
  })
}


shinyApp(ui = ui, server = server)
