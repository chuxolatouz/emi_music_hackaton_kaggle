################################################################
# Minería de Datos
# Proyecto: EMI Music Data Science Hackathon
# Grupo: Nadia Bastidas - CI: 20.976.705
#        Carlos Pires - CI: 22.667.768
#        Jesús Rincón - CI: 20.142.782
#        Andrea Telleria - CI: 20.614.114
################################################################

################################################################
# Instalación de Paquetes necesarios
################################################################

#Creamos la función que recibe los paquetes
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
train <- read.csv("train.csv")
test <- read.csv("test.csv")

################################################################
# Preprocesamiento
################################################################

# Eliminación de variables que no aportan información al modelo
train$Artist <- NULL
train$Time <- NULL

################################################################
# Cambiando el tipo de datos de "Train"
################################################################

#Matriz sparce con los datos con los usuarios como rownames y los item (tracks) como colnames
track.id <- as.factor(train$Track)
user.id <- as.factor(train$User)
music.sparse = sparseMatrix(i = as.numeric(user.id), j = as.numeric(track.id), 
                           x = as.numeric(train$Rating))
colnames(music.sparse) <- levels(track.id)
rownames(music.sparse) <- levels(user.id)
dim(music.sparse)

# Usando la función as, para conseguir una realRatingMatrix
train.matrix <- new("realRatingMatrix", data = music.sparse)
dim(train.matrix)

# Dibujo de la data onde se marcan las canciones escuchadas por un usuario con el rating como el color
image(train.matrix[1:100,1:184], main = "Data de Ratings", col.regions = rainbow(100))
summary(getRatings(train.matrix))

################################################################
# Cambiando el tipo de datos de "Test"
################################################################

#Matriz sparce con los datos con los usuarios como rownames y los item (tracks) como colnames
track.id3 <- as.factor(test$Track)
user.id3 <- as.factor(test$User)
music.sparse3 = sparseMatrix(i = as.numeric(user.id3), j = as.numeric(track.id3))
colnames(music.sparse3) <- levels(track.id3)
rownames(music.sparse3) <- levels(user.id3)
dim(music.sparse3)
View(as.data.frame(as.matrix(music.sparse3)))

# Usando la función as, para conseguir una realRatingMatrix
test.matrix <- new("realRatingMatrix", data = music.sparse3)
dim(train.matrix)


################################################################
# Dibujando la Data con un Subset de 0.001% de la poblaci?n
################################################################

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

# Dibujo de la data donde se marcan las canciones escuchadas por el 0.001% de la poblaci?n
image(music.sparse2[1:187,1:107], main = "Data de Ratings", col.regions = rainbow(100))
summary(getRatings(train.matrix))

################################################################
# Creando el modelos
################################################################

# Modelo usando UBCF + Normalizado con Coseno
ubcf.model.cos <- Recommender(train.matrix, method="UBCF", 
                              param=list(normalize = "Z-score", method = "Cosine", 
                                         nn = 5, minRating = 0))

# Modelo usando UBCF + Normalizado con Jaccard
ubcf.model.jac <- Recommender(train.matrix, method = "UBCF", 
                              param = list(normalize = "Z-score", method ="Jaccard", 
                                           nn = 5, minRating = 0))

# Modelo usando IBCF + Normalizado con Coseno
ibcf.model.cos <- Recommender(train.matrix, method = "IBCF", 
                              param = list(normalize = "Z-score", method = "Cosine", 
                                           minRating = 0))

# Modelo usando IBCF + Normalizado con Jaccard
ibcf.model.jac <- Recommender(train.matrix, method = "IBCF", 
                              param = list(normalize = "Z-score", method = "Jaccard", 
                                           minRating = 0))

################################################################
# Prediciendo con los modelos planteados
################################################################

recom.jac <- predict(ibcf.model.jac, train.matrix[1:nrow(train.matrix)], type="ratings")
recom.cos <- predict(ibcf.model.cos, train.matrix[1:nrow(train.matrix)], type="ratings")

#recom.jac <- predict(ubcf.model.jac, train.matrix[1:nrow(train.matrix)], type="ratings")
#recom.cos <- predict(ubcf.model.cos, train.matrix[1:nrow(train.matrix)], type="ratings")


# Viendo la predicción
pred.matrix.jac = as(recom.jac, "matrix")
pred.matrix.cos = as(recom.cos, "matrix")