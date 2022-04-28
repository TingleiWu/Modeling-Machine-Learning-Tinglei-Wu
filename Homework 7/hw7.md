hw7
================
Tinglei Wu
4/19/2022

# Work through the “Image Classification” tutorial on the RStudio Keras website.

``` r
library(keras)

fashion_mnist <- dataset_fashion_mnist()
```

    ## Loaded Tensorflow version 2.8.0

``` r
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test
```

``` r
class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')
```

``` r
dim(train_images)
```

    ## [1] 60000    28    28

``` r
dim(train_labels)
```

    ## [1] 60000

``` r
train_labels[1:20]
```

    ##  [1] 9 0 0 3 0 2 7 2 5 5 0 9 5 5 7 9 1 0 6 4

``` r
dim(test_images)
```

    ## [1] 10000    28    28

``` r
dim(test_labels)
```

    ## [1] 10000

``` r
library(tidyr)
library(ggplot2)

image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")
```

![](hw7_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#Scale data
train_images <- train_images / 255
test_images <- test_images / 255

# show 25 pic to check whether data format is correct
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}
```

![](hw7_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')
```

``` r
model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)
```

``` r
model %>% fit(train_images, train_labels, epochs = 5, verbose = 2)
score <- model %>% evaluate(test_images, test_labels, verbose = 0)

#cat('Test loss:', score$loss, "\n")
#cat('Test accuracy:', score$acc, "\n")
```

``` r
predictions <- model %>% predict(test_images)

# check
predictions[1, ]
```

    ##  [1] 1.062044e-05 2.627344e-08 1.616137e-06 9.370547e-08 1.062512e-06
    ##  [6] 4.516072e-02 2.029345e-06 1.965128e-01 3.038776e-06 7.583079e-01

``` r
which.max(predictions[1, ])
```

    ## [1] 10

``` r
class_pred <- model %>% predict(test_images) %>% k_argmax()
class_pred[1:20]
```

    ## tf.Tensor([9 2 1 1 6 1 4 6 5 7 4 5 7 3 4 1 2 2 8 0], shape=(20,), dtype=int64)

``` r
test_labels[1]
```

    ## [1] 9

``` r
# see some prediction. correct: green, wrong: red
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}
```

![](hw7_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
## predict single image
# Grab an image from the test dataset
# take care to keep the batch dimension, as this is expected by the model
img <- test_images[1, , , drop = FALSE]
dim(img)
```

    ## [1]  1 28 28

``` r
predictions <- model %>% predict(img)
predictions
```

    ##              [,1]         [,2]        [,3]         [,4]         [,5]       [,6]
    ## [1,] 1.062046e-05 2.627344e-08 1.61614e-06 9.370574e-08 1.062514e-06 0.04516073
    ##             [,7]      [,8]         [,9]     [,10]
    ## [1,] 2.02935e-06 0.1965128 3.038779e-06 0.7583079

``` r
# subtract 1 as labels are 0-based
prediction <- predictions[1, ] - 1
which.max(prediction)
```

    ## [1] 10

``` r
class_pred <- model %>% predict(img) %>% k_argmax()
class_pred
```

    ## tf.Tensor([9], shape=(1,), dtype=int64)

# 2. Use the Keras library to re-implement the simple neural network discussed during lecture for the mixture data (see nnet.R). Use a single 10-node hidden layer; fully connected.

``` r
library('rgl')
library('ElemStatLearn')
library('nnet')
library('dplyr')
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
## load binary classification example data
data("mixture.example")
dat <- mixture.example
```

``` r
## fit single hidden layer,10 hidden nodes, fully connected NN
model2 <- keras_model_sequential()
model2 %>%
  #layer_flatten(input_shape = c(2)) %>%
  layer_dense(units = 10, activation = 'relu') %>% 
  layer_dense(units = 2, activation = 'softmax')

model2 %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy'))

model2 %>% fit(x=dat$x, y=dat$y, epochs = 5, verbose = 2)
```

# 3. Create a figure to illustrate that the predictions are (or are not) similar using the ‘nnet’ function versus the Keras model.

``` r
## create 2D plot of mixture data
plot_mixture_data <- expression({
  plot(dat$xnew[,1], dat$xnew[,2], type="n",xlab="x1", ylab="x2")
  ## plot points and bounding box
  x1r <- range(dat$px1)
  x2r <- range(dat$px2)
  pts <- plot(dat$x[,1], dat$x[,2],
                type="p",col=ifelse(dat$y, "orange", "blue"))
  lns <- lines(x1r[c(1,2,2,1,1)], x2r[c(1,1,2,2,1)])

  prob <- matrix(dat$prob, length(dat$px1), length(dat$px2))
  cls <- contourLines(dat$px1, dat$px2,prob, levels=0.5)
  pls <- lapply(cls, function(p) 
    lines(p$x, p$y, col='purple', lwd=3))
})
```

``` r
plot_keras_predictions <- function(fit, dat=mixture.example) {
  
  ## create figure
  eval(plot_mixture_data)
  
  ## compute predictions from nnet
  preds <- predict(fit, dat$xnew, type="class")
  probs <- predict(fit, dat$xnew, type="raw")[,1]
  probm <- matrix(probs, length(dat$px1), length(dat$px2))
  cls <- contourLines(dat$px1, dat$px2, probm, levels=0.5)
  
  ## plot classification boundary
  pls <- lapply(cls, function(p) 
    lines(p$x, p$y,col='red', lwd=2))
}

plot_keras_predictions(model2)
```

![](hw7_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->![](hw7_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
# compute and plot predictions
plot_nnet_predictions <- function(fit, dat=mixture.example) {
  
  ## create figure
  eval(plot_mixture_data)
  
  ## compute predictions from nnet
  preds <- predict(fit, dat$xnew, type="class")
  probs <- predict(fit, dat$xnew, type="raw")[,1]
  probm <- matrix(probs, length(dat$px1), length(dat$px2))
  cls <- contourLines(dat$px1, dat$px2, probm, levels=0.5)
  
  ## plot classification boundary
  pls <- lapply(cls, function(p) 
    lines(p$x, p$y, z=1, col='green', lwd=2))
}

nnet_fit <- nnet(x=dat$x, y=dat$y, size=10, entropy=TRUE, decay=0) 
```

    ## # weights:  41
    ## initial  value 154.107417 
    ## iter  10 value 99.290701
    ## iter  20 value 92.845416
    ## iter  30 value 79.424181
    ## iter  40 value 69.134926
    ## iter  50 value 67.081211
    ## iter  60 value 66.170752
    ## iter  70 value 65.724973
    ## iter  80 value 65.646280
    ## iter  90 value 65.541820
    ## iter 100 value 65.408169
    ## final  value 65.408169 
    ## stopped after 100 iterations

``` r
plot_nnet_predictions(nnet_fit)
```

![](hw7_files/figure-gfm/nnet%20plot-1.png)<!-- -->

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): "z" is not a graphical
    ## parameter

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): "z" is not a graphical
    ## parameter

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): "z" is not a graphical
    ## parameter

![](hw7_files/figure-gfm/nnet%20plot-2.png)<!-- -->
