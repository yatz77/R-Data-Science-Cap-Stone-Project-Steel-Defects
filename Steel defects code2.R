# In here the data for the expired Kaggle competition regarding steel 
# defects will be analysed. The competition can be found here:
# https://www.kaggle.com/mullerismail/steel-defect-understand-plot-with-r-novice

# The dataset of the competition contains 3 directories:
# 1) train.csv: information on the train data
# 2) train_images: images used to train the models
# 3) test_images: images to test the final model
install.packages("imager")
library(tidyverse)
library(imager)
library(stringr)
library(caret)

download.file()

## Construct function to plot an image with all defects ##
# Code was adapted from Ismail Müller: 
# https://www.kaggle.com/mullerismail/steel-defect-understand-plot-with-r-novice

# Plot an image by loading the path to the program
path_to_image  <- list.files(path = "./Data/train_images", 
                             pattern = ".jpg",recursive = TRUE, full.names=TRUE)[1]
path_to_image # 0002cc93b.jpg
image_to_plot <- imager::load.image(path_to_image)


list.files("./Data/train_images")[1:5] # file names of train images
train <- readr::read_csv("./Data/train.csv") # load in the train data
head(train)
train <- train %>% separate(ImageId_ClassId, into = c("image_id", "class_id"), sep = "_")
train

image_to_plot <- imager::grayscale(image_to_plot)
print(image_to_plot)
plot(image_to_plot)

# Image the defects of 1 type of defects on 1 image
splitted_pixels <- str_split(pixels_to_split, "[:space:]") %>% # splits the EncodedPixel observation using :space: character as a pattern
  unlist() %>% # unlists/as.vector the result of str_split
  as.numeric() %>% # the valus are still characters, so we transform them into numerical values
  matrix(ncol = 2, byrow = TRUE) %>%  # create a matrix of 2 columns with the values
  as.data.frame() %>% # transform the matrix into a data.frame
  rename(pixel_start = V1, run_length = V2) %>% # rename the columns
  mutate(pixel_end = pixel_start + run_length) # create a variable pixel_end

height <- dim(image_to_plot)[2]
width <- dim(image_to_plot)[1]

mask <- matrix(0, nrow = height, ncol = width)

activated_pixels <- purrr::pmap(splitted_pixels, ~ seq(..1, ..3, by=1) ) %>% unlist()
mask[activated_pixels] <- 1


plot(image_to_plot)
t(mask) %>% as.cimg() %>% as.pixset() %>% highlight(col="red")

# functions to plot images with colored lines

split_pixels_single_class <- function(EncodedPixels){
  str_split(EncodedPixels, "[:space:]", simplify = TRUE) %>% # splits the EncodedPixel observation using :space: character as a pattern
    as.numeric() %>% # the valus are still characters, so we transform them into numerical values
    matrix(ncol = 2, byrow = TRUE) %>%  # create a matrix of 2 columns with the values
    as.data.frame() %>% # transform the matrix into a data.frame
    transmute(pixel_start = V1, run_length = V2,
              pixel_end = pixel_start + run_length - 1) # create a variable pixel_end
}

split_pixels_multi_class <- function(image_name){
  train %>% filter(ImageId == image_name, !is.na(EncodedPixels)) %>% 
    split( .$ClassId ) %>% 
    map( ~ split_pixels_single_class(.$EncodedPixels) %>% 
           pmap(~ seq(..1, ..3, by=1)) %>% unlist )
}


plot_image <- function(image_name){
  # find the image
  path_to_image  <- list.files(path = "./Data/train_images", 
                               pattern = image_name,recursive = TRUE, full.names=TRUE)
  if( length(path_to_image) == 0 ){ stop(str_glue("Image {image_name} wasn't found in the images")) }
  
  # load the image
  image_to_plot <- path_to_image %>% imager::load.image() %>% imager::grayscale()
  # Create the masks for each defect class
  height <- dim(image_to_plot)[2]
  width <- dim(image_to_plot)[1]
  
  masks <- split_pixels_multi_class(image_name) %>% 
    map(~ {
      m <- matrix(0, nrow=height, ncol=width)
      m[.x] <- 1
      t(m) %>% as.cimg() %>% as.pixset()
    })
  
  # plot the image and the defects
  plot(image_to_plot)
  for(i in 1:length(masks)) {
    highlight(masks[[i]], 
              col = c("blue","green","red","yellow")[as.numeric(names(masks)[i])] )
  } 
}
plot_image("0002cc93b.jpg")
# Plot a random image
plot_image(as.character(train[sample(1:nrow(train),1),1]))

## Data visualization

#Number of defects for each id
train %>% group_by (ClassId) %>%
  summarize(n = n()) %>%
  ggplot (aes(x = as.factor(ClassId), y = n, fill = as.factor(ClassId))) +
  geom_col() +
  scale_fill_manual(values = c("blue","green","red","yellow")) +
  xlab("ClassId") +
  labs(fill="ClassId")

#Extract height, width and number of pixels that a defect occupies for one image
train_1 <- train[4856,]
train_1 <- train[sample(1:nrow(train),1),]
number_pixels_1 <- train_1$EncodedPixels %>% str_split(., "[:space:]") %>% # splits the EncodedPixel observation using :space: character as a pattern
  unlist() %>% # unlists/as.vector the result of str_split
  as.numeric() %>% # the valus are still characters, so we transform them into numerical values
  matrix(ncol = 2, byrow = TRUE) %>%  # create a matrix of 2 columns with the values
  as.data.frame() %>% # transform the matrix into a data.frame
  rename(pixel_start = V1, run_length = V2) 
widthlist <- function(j) {ifelse(any(j:(j+255) %in% number_pixels_1$pixel_start),1,0)} # scan every column for presence of defect
columnlist <- seq(from = 1, to = 4096000, by = 256) # list containg the first pixel of every column
width <- sum(sapply(columnlist, widthlist)) # function needed to determine the width
number_pixels_1 %>% summarize(height = max(run_length), width = width , total_pixels = sum(run_length)) %>%
  unlist()



#Extract number of pixels that a defect occupies for a selected number of images (e.g. 500)
train_500 <- train[1:500,]    
train_500_encoded_pixels <- as.list(train_500$EncodedPixels)
train_encoded_pixels <- as.list(train$EncodedPixels)

number_pixels <- function(i){
  i%>% str_split(., "[:space:]") %>% # splits the EncodedPixel observation using :space: character as a pattern
    unlist() %>% # unlists/as.vector the result of str_split
    as.numeric() %>% # the valus are still characters, so we transform them into numerical values
    matrix(ncol = 2, byrow = TRUE) %>%  # create a matrix of 2 columns with the values
    as.data.frame() %>% # transform the matrix into a data.frame
    rename(pixel_start = V1, run_length = V2) %>% # rename the columns
    summarize(total_pixels = sum(run_length)) %>%
    .$total_pixels
}
number_pixels_per_defect <- as.data.frame(as.matrix(lapply(train_encoded_pixels, number_pixels)))

# defect width, defect height, number of pixels defect for a selected number of images (e.g. 2)

defect_width_height_pixels <- function(i){
  columnlist <- seq(from = 1, to = 4096000, by = 256) # list containing the first pixel of every column
  i <- i %>% str_split(., "[:space:]") %>% # splits the EncodedPixel observation using :space: character as a pattern
    unlist() %>% # unlists/as.vector the result of str_split
    as.numeric() %>% # the valus are still characters, so we transform them into numerical values
    matrix(ncol = 2, byrow = TRUE) %>%  # create a matrix of 2 columns with the values
    as.data.frame() %>% # transform the matrix into a data.frame
    rename(pixel_start = V1, run_length = V2) 
  widthlist <- function(j) {ifelse(any(j:(j+255) %in% i$pixel_start),1,0)}
  width <- sum(sapply(columnlist, widthlist)) # function needed to determine the width
  i %>% summarize(height = ifelse(max(run_length) > 256, 256, max(run_length)), width = width , total_pixels = sum(run_length)) %>%
    unlist()
}

train_2 <- train[1:2,] 
train_2
train_2_encoded_pixels <- as.list(train_2$EncodedPixels)

width_height_pixels_per_defect_2 <- as.data.frame(t(as.matrix(sapply(train_2_encoded_pixels, defect_width_height_pixels))))
width_height_pixels_per_defect_2
width_height_pixels_per_defect_image_2 <- cbind(width_height_pixels_per_defect_2, ImageId = train_2$ImageId, ClassId = train_2$ClassId)
width_height_pixels_per_defect_image_2
train_2r <- right_join(train_2,width_height_pixels_per_defect_image_2)
train_2r

# defect width, defect height, number of pixels defect for a train set

add_width_height_pixels <- function(train_set){
  train_set_encoded_pixels <- as.list(train_set$EncodedPixels)
  defect_width_height_pixels <- function(i){
    columnlist <- seq(from = 1, to = 4096000, by = 256) # list containing the first pixel of every column
    i <- i %>% str_split(., "[:space:]") %>% # splits the EncodedPixel observation using :space: character as a pattern
      unlist() %>% # unlists/as.vector the result of str_split
      as.numeric() %>% # the valus are still characters, so we transform them into numerical values
      matrix(ncol = 2, byrow = TRUE) %>%  # create a matrix of 2 columns with the values
      as.data.frame() %>% # transform the matrix into a data.frame
      rename(pixel_start = V1, run_length = V2) 
    widthlist <- function(j) {ifelse(any(j:(j+255) %in% i$pixel_start),1,0)}
    width <- sum(sapply(columnlist, widthlist)) # function needed to determine the width
    i %>% summarize(height = ifelse(max(run_length) > 256, 256, max(run_length)), width = width , total_pixels = sum(run_length)) %>%
      unlist()
  }
  width_height_pixels_per_defect <- as.data.frame(t(as.matrix(sapply(train_set_encoded_pixels, defect_width_height_pixels))))
  width_height_pixels_per_defect_image <- cbind(width_height_pixels_per_defect, ImageId = train_set$ImageId, ClassId = train_set$ClassId)
  right_join(train_set,width_height_pixels_per_defect_image)
}
train2 <- add_width_height_pixels(train) # apply function to train
as_tibble(train2)

# effect of height (boxplot)

height_boxplot <- train2  %>% group_by(ClassId) %>%
  ggplot(aes(x=as.factor(ClassId), y=height)) +
  geom_boxplot()

height_boxplot

# visualize the type one defects with heights being outliers
height_filter <- train2 %>% filter(height > 200, ClassId == 1) %>%
  select(ImageId)
plot_image(as.character(height_filter[1,]))
plot_image(as.character(height_filter[2,]))
plot_image(as.character(height_filter[3,]))

# effect of width (boxplot)

width_boxplot <- train2  %>% group_by(ClassId) %>%
  ggplot(aes(x=as.factor(ClassId), y=width)) +
  geom_boxplot()

width_boxplot

# visualize the type two defects with widths being outliers
width_filter <- train2 %>% filter(width > 70, ClassId == 2) %>%
  select(ImageId)
width_filter
plot_image(as.character(width_filter[1,]))
plot_image(as.character(width_filter[2,]))

# effect of number of pixels (boxplot)

pixels_boxplot <- train2  %>% group_by(ClassId) %>%
  ggplot(aes(x=as.factor(ClassId), y=total_pixels)) +
  geom_boxplot()


total_pixels_over_300000 <- train2 %>% filter(total_pixels > 300000) %>%
  select(ImageId)
plot_image(as.character(total_pixels_over_300000[1,]))


## Machine learning
# Guess defect using width height and pixel size

# separate the train data frame in a train and test set
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(1:nrow(train2), times = 1, p = 0.1, list = FALSE)
test_index
train3 <- train2[-test_index,]
test <- train2[test_index,]

# define x and y 
y_train <- as.factor(train3$ClassId)
y_test <- as.factor(test$ClassId)
x_train <- as.matrix(train3[,4:6])
x_test <- as.matrix(test[,4:6])


#qda
train_qda <- train(x_train, y_train, method = "qda")
predict(train_qda, x_test, type = "prob") %>% head()
predict(train_qda, x_test) %>% head()
confusionMatrix(predict(train_qda, x_test), y_test)$table
confusionMatrix(predict(train_qda, x_test), y_test)$overall["Accuracy"]

#lda
train_lda <- train(x_train, y_train, method = "lda")
confusionMatrix(predict(train_lda, x_test), y_test)$table
confusionMatrix(predict(train_lda, x_test), y_test)$overall["Accuracy"]

#knn
train_knn <- train(x_train, y_train, method = "knn", tuneGrid = data.frame(k = seq(3, 100, 2)))
confusionMatrix(predict(train_knn, x_test), y_test)$table
confusionMatrix(predict(train_knn, x_test), y_test)$overall["Accuracy"]

#rpart
train_rpart <- train(x_train, y_train, method = "rpart",tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)))
confusionMatrix(predict(train_rpart, x_test), y_test)$table
confusionMatrix(predict(train_rpart, x_test), y_test)$overall["Accuracy"]

#randomforest
library(randomForest)
train_rf <- randomForest(x_train, y_train)
confusionMatrix(predict(train_rf, x_test), y_test)$table
confusionMatrix(predict(train_rf, x_test), y_test)$overall["Accuracy"]

#rf
train_rf2 <- train(x_train, y_train, method = "rf")
confusionMatrix(predict(train_rf2, x_test), y_test)$table
confusionMatrix(predict(train_rf2, x_test), y_test)$overall["Accuracy"]

#naive bayes
train_naive_bayes <- train(x_train, y_train, method = "naive_bayes")
confusionMatrix(predict(train_naive_bayes, x_test), y_test)$table
confusionMatrix(predict(train_naive_bayes, x_test), y_test)$overall["Accuracy"]

#svmLinear
train_svmLinear <- train(x_train, y_train, method = "svmLinear")
confusionMatrix(predict(train_svmLinear, x_test), y_test)$table
confusionMatrix(predict(train_svmLinear, x_test), y_test)$overall["Accuracy"]

#multinom
train_multinom <- train(x_train, y_train, method = "multinom")
confusionMatrix(predict(train_multinom, x_test), y_test)$table
confusionMatrix(predict(train_multinom, x_test), y_test)$overall["Accuracy"]


#all models at once
models <- c("qda", "lda", "knn", "rpart", "rf",  "naive_bayes", "multinom")

set.seed(1, sample.kind = "Rounding")

fits <- lapply(models, function(model){
  print(model)
  train(x_train, y_train, method = model)
})

names(fits) <- models

predmatrix <- sapply(fits, function(x){
  predict(x,x_test)
}) 

pred <- map(fits, function(object) # makes the predictions for all models
  predict(object, newdata = x_test))


acc <- sapply(pred, function(object){ #accuracy of all the models
  confusionMatrix(data = object, reference = y_test)$overall[["Accuracy"]]
}) 

acc

#ensemble of all models

i <- c(1,2,3,4)
times_classID_predicted <- sapply(i, function(x){ # calculates how many times an algorithm predicted a particular classId per observation
  rowSums(predmatrix == x)
})
names(times_classID_predicted) <- i
j <- seq(1,length = nrow(times_classID_predicted), by =1)
ensemble <- sapply(j, function(y){ # takes the ClassId which has been predicted the most
  as.factor(which.max(times_classID_predicted[y,]))
})
confusionMatrix(ensemble, y_test)$table
confusionMatrix(ensemble, y_test)$overall[["Accuracy"]]

