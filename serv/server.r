shinyServer(function(input, output) {
  
  install = function(pkg){
    #Si ya está instalado, no lo instala.
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
    }
  }
  install("foreach")
  #Seleccionamos los archivos que queremos instalar
  archive = c("shiny", "rmarkdown", "recommenderlab", "caret")
  foreach(i = archive) %do% install(i)
  
  ################################################################
  # Carga de datos
  ################################################################
  train <- read.csv("../train.csv")
  test <- read.csv("../test.csv")
  
  ################################################################
  # Preprocesamiento
  ################################################################
  
  # Eliminación de variables que no aportan información al modelo
  train$Artist <- NULL
  train$Time <- NULL
  
  track.id <- as.factor(train$Track)
  user.id <- as.factor(train$User)
  music.sparse = sparseMatrix(i = as.numeric(user.id), j = as.numeric(track.id), 
                              x = as.numeric(train$Rating))
  colnames(music.sparse) <- levels(track.id)
  rownames(music.sparse) <- levels(user.id)
  dim(music.sparse)
  
  train.matrix <- new("realRatingMatrix", data = music.sparse)
  
  output$myimagen <- renderPlot(
    image(train.matrix[input$num1:input$num2,1:184], main = "Data de Ratings", col.regions = rainbow(100))
      )
  Split <- evaluationScheme(train.matrix, method="split", train = 0.9, given=1, goodRating = 100)
  
  #Evaluando los modelos
  
  ibcf.cos <- Recommender(getData(Split, "train"), method = "IBCF", 
                          param = list(normalize = "Z-score", method = "Cosine", 
                                       minRating = 0))
  ibcf.jac <- Recommender(getData(Split, "train"), method = "IBCF", 
                          param = list(normalize = "Z-score", method = "Jaccard", 
                                       minRating = 0))
  ubcf.jac <- Recommender(getData(Split, "train"), method = "UBCF", 
                          param = list(normalize = "Z-score", method = "Jaccard", 
                                       minRating = 0))
  
  ubcf.cos <- Recommender(getData(Split, "train"), method = "UBCF", 
                          param = list(normalize = "Z-score", method = "Cosine", 
                                       minRating = 0))
  
  #Realizando predicciones
  
  p.ibcfc <- predict(ibcf.cos, getData(Split, "known"), type="ratings")
  p.ibcfj <- predict(ibcf.jac, getData(Split, "known"), type="ratings")
  #p.ubcfj <- predict(ubcf.jac, getData(Split, "known"), type="ratings")
  #p.ubcfc <- predict(ubcf.cos, getData(Split, "known"), type="ratings")
  
  #Evaluando el rendimiento de los modelos
  
  eval.jac <- calcPredictionAccuracy(p.ibcfc, test.matrix)
  eval.cos <- calcPredictionAccuracy(recom.cos, test.matrix)
  
  error.ibcf <- rbind(
    calcPredictionAccuracy(p.ibcfc, getData(Split, "unknown")),
    calcPredictionAccuracy(p.ibcfj, getData(Split, "unknown"))
  )
  output$error1 <- renderText({
    rownames(error.ibcf) <- c("IBCF Cos","IBCF Jac")
    error.ibcf
  })
  error.ubcf <- rbind(
    calcPredictionAccuracy(p.ubcfc, getData(Split, "unknown")),
    calcPredictionAccuracy(p.ubcfj, getData(Split, "unknown"))
  )
  output$error2 <- renderText({
    rownames(error.ubcf) <- c("UBCF Cos","UBCF Jac")
    error.ubcf  
  })
  
  
  #Plot de las predicciones
  
  #p.ibcfc
  output$myimagenpibcfc <- renderPlot({
    image(p.ibcfc[200:300,1:184], main = "Data de Ratings", col.regions = rainbow(100))  
  })
  
  #p.ibcfj
  output$myimagenpibcfj <- renderPlot({
    image(p.ibcfj[200:300,1:184], main = "Data de Ratings", col.regions = rainbow(100))
  })
  
  #p.ubcfj
  output$myimagenpubcfj <- renderPlot({
    image(p.ubcfj[200:300,1:184], main = "Data de Ratings", col.regions = rainbow(100))  
  })
  
  #p.ubcfc
  output$myimagenpubcfc <- renderPlot({
    image(p.ubcfc[200:300,1:184], main = "Data de Ratings", col.regions = rainbow(100))
  })
  
  
  
  
  
})