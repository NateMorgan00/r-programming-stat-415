# QUESTION 1

# Read in "Pathogenicity" file
pathogenicity <- read_delim("Pathogenicity.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
cols( 
expt = col_double(), 
Isolate_ID = col_double(), 
plant = col_double(), 
Infection = col_double() 
) 
view(pathogenicity)

# Read in "Species" file
species <- read_delim("Species.txt", "\t", escape_double = FALSE, trim_ws = TRUE) 
cols( 
Isolate_ID = col_double(), 
ID = col_character(), 
Species = col_character() 
)
view(species)



# QUESTION 2

# Access library
library(pixmap)
# Import image
selfie <- read.pnm(“chrissyselfie.pgm”, cellres = 1)
# Locate position of mosaic
locator()                                                                                       
$x
[1] 565.5584  441.7693

$y
[1] 594.6179  497.8374 

# Get total size of image
str(Selfie) 
Formal class 'pixmapGrey' [package "pixmap"] with 6 slots
  ..@ grey    : num [1:600, 1:1078] 0.157 0.106 0.125 0.125 0.114 ...
  ..@ channels: chr "grey"
  ..@ size    : int [1:2] 600 1078
  ..@ cellres : num [1:2] 1 1
  ..@ bbox    : num [1:4] 0 0 1078 600
  ..@ bbcent  : logi FALSE

# Create mosaic
mosaic.plot <- function(Selfie, xrange, yrange){
     length.y <- length(594:497)
     length.x <- length(441:565)
     image2 <- Selfie
     whitenoise <- matrix(nrow = length.y, ncol = length.x, runif(length.y * length.x))
     image2@grey[6:103, 441:565] <- whitenoise
     return(image2)
}

# Place mosaic over image
Selfie.mosaic <- mosaic.plot(Selfie, 509:592, 441:565)
mosaic.plot2 <- function(Selfie.mosaic, yrange, xrange, degree) {
     length.y <- length(470:387)
     legnth.x <- length(617:723)
     image3 <- Selfie.mosaic
     image3@grey[130:213, 617:723] <- degree
     return(image3)
}
Selfie.mosaic2 <- mosaic.plot2(Selfie.mosaic, 470:387, 617:723, 1)
plot(Selfie.mosaic2)
