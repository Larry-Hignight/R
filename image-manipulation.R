####  ----------------------------------------------------------------------  ####
####  Basic image manipulation functions that work with the png package       ####
####                                                                          ####
####  Many of the functions in this script are based on the following paper:  ####
####  http://arxiv.org/pdf/1003.4053.pdf                                      ####
####                                                                          ####
####                                                                          ####
####  ----------------------------------------------------------------------  ####

library(png)

## Creates the negative of an image
img_negative <- function(image) 1 - image

## The thresholding transformation returns an image with values extreme values depending on the threshold
img_threshold <- function(image, threshold = .5, max.val = 1, min.val = 0) ifelse(image >= threshold, max.val, min.val)

## This brightness function increases the brightness in a non-linear manner
## Note - I wrote this function for some extremely dark photos, hence the 'custom'
img_brightness.custom <- function(image, coef = 0) image + log(2 - image, 50) + coef

## The log transformation function from the arxiv paper
img_brightness.log <- function(image, coef = 1) coef * log(1 + image)

## The gamma transformation function from the arxiv paper (I need to experiment with the settings)
img_gamma_correction <- function(image, lambda = .5, coef = 1) coef * image ^ lambda

## Creates an image with increased or decreased contrast
img_contrast <- function(image, factor = 1.05) factor * (image - .5) + .5

## Crops the image
img_crop <- function(image, rows, cols) image[rows, cols, ]

## Converts a color image to a gray scale image based on the mean RGB image value
img_gray_scale <- function(image) (image[ , , 1] + image[ , , 2] + image[ , , 3]) / 3

## Converts a color iamge to a gray scale image using the supplied function (defaults to the max function)
img_gray_scale2 <- function(image, FUN = max) {
  gray <- image[ , , 1]
  dims <- dim(image)             ## Do I need to use brackets?
  for (r in 1:dims[1]) for (c in 1:dims[2]) gray[r, c] <- FUN(image[r, c, ])
  return(gray)
}

## Clips image values that are outside of the range [0, 1]
img_clip_value <- function(image) {
  tmp <- ifelse(image < 0, 1, image)
  ifelse(tmp > 1, 1, tmp)
}

## Maps image values into the range [0, 1]
img_map_value <- function(image) {
  min.val <- min(image)
  (image - min.val) / (max(image) - min.val)
}


#### BOX FUNCTIONS -----------------------------------------------------------

img_draw_box <- function(image, x1, y1, x2, y2) {
  image[x1:x2, y1, ] <- 1
  image[x1:x2, y2, ] <- 1
  image[y1:y2, x1, ] <- 1
  image[y1:y2, x2, ] <- 1
  return(image)
}


#### PLOT FUNCTIONS -----------------------------------------------------------

## Creates a histogram style plot of the image colors
img_color_plot <- function(image, max = 5) {
  plot(0, 0, xlim = c(0, 1), ylim = c(0, max), main = 'Color Histogram', 
       xlab = 'Color Value', ylab = 'Density', type = 'n')
  lines(density(image[ , , 1]), col = 'red', lwd = 2)
  lines(density(image[ , , 2]), col = 'green', lwd = 2)
  lines(density(image[ , , 3]), col = 'blue', lwd = 2)
}

img_grey_plot <- function(image) {
  plot(density(image), main = 'Grey Scale Histogram', col = 'darkgray', lwd = 2,
       xlab = 'Grey Scale Value', ylab = 'Density', type = 'l', pch = '.')
}

