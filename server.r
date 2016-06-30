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

# Carga de datos
train <- read.csv("train.csv")

# Preprocesamiento

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

# Dibujando la Data con un Subset de 0.01% de la poblacion

#Tama?o de la muestra
t_size <- floor(0.001 * length(train$User))

#Patici?n
t_indx <- sample(seq_len(length(train$User)), size = t_size)
train2 <- train[t_indx, ]

#Matriz sparce con sampling de 0.001%
track.id2 <- as.factor(train2$Track)
user.id2 <- as.factor(train2$User)
music.sparse2 = sparseMatrix(i = as.numeric(user.id2), j = as.numeric(track.id2), 
                             x = as.numeric(train2$Rating))
colnames(music.sparse2) <- levels(track.id2)
rownames(music.sparse2) <- levels(user.id2)
dim(music.sparse2)

# Realizando las particiones train y test

Split <- evaluationScheme(train.matrix, method="split", 
                          train = 0.9, given=1, goodRating = 100)

#Evaluando los modelos

#ibcf.cos <- Recommender(getData(Split, "train"), method = "IBCF", 
#                        param = list(normalize = "Z-score", method = "Cosine", 
#                                     minRating = 0))
#ibcf.jac <- Recommender(getData(Split, "train"), method = "IBCF", 
#                        param = list(normalize = "Z-score", method = "Jaccard", 
#                                     minRating = 0))
#ubcf.jac <- Recommender(getData(Split, "train"), method = "UBCF", 
#                        param = list(normalize = "Z-score", method = "Jaccard", 
#                                     minRating = 0))
#ubcf.cos <- Recommender(getData(Split, "train"), method = "UBCF", 
#                        param = list(normalize = "Z-score", method = "Cosine", 
#                                     minRating = 0))

#Cargando predicciones

load("p_ibcfc.RData")
load("p_ibcfj.RData")
load("p_ubcfc.rda")
load("p_ubcfj.rda")

#Evaluando el rendimiento de los modelos

eval.jac <- calcPredictionAccuracy(p.ibcfc, test.matrix)
eval.cos <- calcPredictionAccuracy(recom.cos, test.matrix)

error.ibcf <- rbind(
  calcPredictionAccuracy(p.ibcfc, getData(Split, "unknown")),
  calcPredictionAccuracy(p.ibcfj, getData(Split, "unknown"))
)

error.ubcf <- rbind(
  calcPredictionAccuracy(p.ubcfc, getData(Split, "unknown")),
  calcPredictionAccuracy(p.ubcfj, getData(Split, "unknown"))
)

shinyServer(function(input, output) {
  
  output$myimagen <- renderPlot(
    image(train.matrix[input$num1:input$num2,1:184], main = "Data de Ratings", col.regions = rainbow(100))
  )
  
  output$error1 <- renderText({
    rownames(error.ibcf) <- c("IBCF Cos","IBCF Jac")
    error.ibcf
  })
  
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