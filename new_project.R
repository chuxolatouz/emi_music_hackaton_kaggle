train <- read.csv("train.csv")

# Eliminacion de variables que no aportan informacion al modelo
train$Artist <- NULL
train$Time <- NULL

#Analisis exploratorio

hist(train$Rating, col = "skyblue", 
     main = "Histograma de Rating", 
     xlab = "Rating", 
     ylab = "Frecuencia")

#Haciendo la matriz sparse

track.id <- as.factor(train$Track)
user.id <- as.factor(train$User)
music.sparse = sparseMatrix(i = as.numeric(user.id), j = as.numeric(track.id), 
                            x = as.numeric(train$Rating))
colnames(music.sparse) <- levels(track.id)
rownames(music.sparse) <- levels(user.id)
dim(music.sparse)

# Consiguiendo una realRatingMatrix
train.matrix <- new("realRatingMatrix", data = music.sparse)
dim(train.matrix)

rm(track.id, user.id)

image(train.matrix[1:100,1:184], main = "Data de Ratings", col.regions = rainbow(100))
summary(getRatings(train.matrix))


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

# Dibujo de la data donde se marcan las canciones escuchadas por el 0.001% de la poblaci?n
image(music.sparse2[1:dim(music.sparse2)[1],1:dim(music.sparse2)[2]], main = "Data de Ratings", col.regions = rainbow(100))


# Realizando las particiones train y test

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
p.ubcfj <- predict(ubcf.jac, getData(Split, "known"), type="ratings")
p.ubcfc <- predict(ubcf.cos, getData(Split, "known"), type="ratings")

#Evaluando el rendimiento de los modelos

error.ibcf <- rbind(
  calcPredictionAccuracy(p.ibcfc, getData(Split, "unknown")),
  calcPredictionAccuracy(p.ibcfj, getData(Split, "unknown"))
)
rownames(error.ibcf) <- c("IBCF Cos","IBCF Jac")
error.ibcf

error.ubcf <- rbind(
  calcPredictionAccuracy(p.ubcfc, getData(Split, "unknown")),
  calcPredictionAccuracy(p.ubcfj, getData(Split, "unknown"))
)

rownames(error.ubcf) <- c("UBCF Cos","UBCF Jac")
error.ubcf

#Plot de las predicciones

#p.ibcfc
image(p.ibcfc[200:300,1:184], main = "Data de Ratings", col.regions = rainbow(100))

#p.ibcfj
image(p.ibcfj[200:300,1:184], main = "Data de Ratings", col.regions = rainbow(100))

#p.ubcfc
image(p.ubcfj[200:300,1:184], main = "Data de Ratings", col.regions = rainbow(100))

#p.ubcfc
image(p.ubcfc[200:300,1:184], main = "Data de Ratings", col.regions = rainbow(100))


#NO LE PARES A ESTO!

algorithms <- list(IBCF_cos = list(name = "IBCF", 
                                   param = list(method = "cosine")),
                   IBCF_jac = list(name = "IBCF", 
                                   param = list(method = "jaccard"))
                   )

results <- evaluate(Split, algorithms, type = "ratings")

plot(results, annotate = TRUE, main = "ROC curve")

