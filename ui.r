shinyUI(fluidPage(
  titlePanel("Emi Music Recommender"),
  
  fluidRow(
    column(4,
      numericInput("num1", label = h3("Rango minimo"), value = 1)
    ),
    column(4,
      numericInput("num2", label = h3("Rango maximo"), value = 100)     
    ),
    column(4,
      h3("Tamaño del dataset 100.000"),
      h6("Seleccione un rango de tamaño = 100")
    )
  )
  
  ,
  
  hr(),
  
  fluidRow(
    h3("Imagen de usuario seleccionado"),
    imageOutput("myimagen")
  ),
  hr(),
  fluidRow(
    h3("evaluacion"),
    column(6, verbatimTextOutput("error1")),
    column(6, verbatimTextOutput("error2"))
  ),
  hr(),
  fluidRow(
    h3("items"),
    imageOutput("myimagenpibcfc"),
    imageOutput("myimagenpibcfj")
  ),
  fluidRow(
    h3("users"),
    imageOutput("myimagenpubcfc"),
    imageOutput("myimagenpubcfj")
  )
  
  
))