##### MCB, 19.12.19; 27.01.20  ####
# Note that finishing a line of code with #### generates an entry in an index below

################################## libraries #### 
library(tidyverse) ### tidyverse suite of libraries, including dplyr and ggplot2
library(ggpubr) ### further libraries for ggplot objects
library(MASS) ### library from Venables & Ripley's book - Modern Applied Statistics with S-Plus
library(plotly) ### makes interactive, sharper graphs
library(scales) ###  provides methods for automatically determining breaks and labels for axes and legends
library(ggthemes) ### themes to modify ggplots, see https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
library(wesanderson) ### provides palettes inspired by the films of Wes Anderson
library(ggrepel) ### improves automatic positioning of text in ggplots
library(devtools) ### provides R developer tools
library(splines)

require(jpeg)
require(graphics)
require(grDevices)
### bagplot functions ####
### The bagplot is a bivariate analogue of the boxplot and is 
### useful to detect joint outliers
### Download  to your directory the files 000._geom_bag.R, 001_bag_functions.R, 002_bag_demo.R 
### from Ben Marwick's GitHub repository  https://gist.github.com/benmarwick/00772ccea2dd0b0f1745
### and source them to your session
### or load these functions:

devtools::source_gist("00772ccea2dd0b0f1745", filename = "000_geom_bag.r")
devtools::source_gist("00772ccea2dd0b0f1745", filename = "001_bag_functions.r")

### Note that the notation devtools::source_gist 
### means you're calling function source_gist from library devtools
### If the files 000_geom_bag.R and 001_bag_functions.R are in
### your working directory then source them into this session

source('000_geom_bag.R')
source('001_bag_functions.R')
source('002_bag_demo.R')


################################## options ####

options(scipen=999) ## turns off scientific notation


################################# example 0 - some simple ggplots ####
#### we'll use the data.frame Pima.te from library MASS

?Pima.te #### see helpfile for this data.frame
names(Pima.te)
# "npreg" "glu"   "bp"    "skin"  "bmi"   "ped"   "age"   "type" 
str(Pima.te) ### structure of the variables in the data.frame

### scatterplot bmi vs glucose

g0<- ggplot(data=Pima.te, aes(x=bmi, y=glu)) +  ### assign the output to g0
     geom_point() + 
     NULL  ### it's good practice to close a plot with NULL
### this plot graphs glucose concentration vs BMI
### aes stands for "aesthetics" it refers to elements in data.frame
### functions like geom_ refer to the plot's geometry

print(g0) ### print the plot object

g0 ### same 

#### modify the plot

g0 + theme_classic() + 
     NULL 
### this simply prints the plot with a minimal theme
### but object g0 is not changed
### function theme_classic is in library ggplot2
### we're obviating functions in this library without using ::

g0 + ggthemes::theme_fivethirtyeight() + 
     NULL ### another theme, from a non-ggplot2 library

### notice that the original plot hasn't changed

print(g0)

### now add some colour 
g1<- ggplot(data=Pima.te, 
            aes(x=bmi, y=glu, group=type, color=type)) +
     geom_point() + 
     NULL

print(g1) ### a simple two-coloured plot
### note that this works because variable type's class is "factor"
### see what would happen if its class were "numeric":

ggplot(data=Pima.te, 
            aes(x=bmi, y=glu, group=as.numeric(type))) +
  geom_point(aes(color=as.numeric(type))) + 
  NULL

### themes can be changed

g1 + theme_bw() + 
   NULL ### a black & white theme

############### histograms ####

g1H<- ggplot(Pima.te, aes(x=bmi)) +
      geom_histogram() + 
      NULL
g1H ### print


g1H <- ggplot(Pima.te, aes(x=bmi)) +
       geom_histogram(binwidth = 5)  + ### smoother
       NULL

g1H


g1H <- ggplot(Pima.te, aes(x=bmi)) +
       geom_histogram(binwidth = 1)  + ### rougher
       NULL

g1H + theme_fivethirtyeight() + 
       NULL

g1H <- ggplot(Pima.te, aes(x=bmi)) +
  geom_histogram(bins = 10)  + ### define smootnes with num of bins
  NULL

g1H

g1H <- ggplot(Pima.te, aes(x=bmi)) +
       geom_histogram(bins = 10, fill='red')  + 
  ### define smootnes with num of bins and use red bars
  ### note that comments can be inserted anywhere between
  ### ggplot and NULL
       NULL

g1H + theme_classic2() ### another minimalist theme

g1H1<- ggplot(Pima.te, aes(x=bmi, fill=type)) +
  ### separate by Diabetes Yes/No
       geom_histogram(bins=10) + 
       NULL

g1H1 + theme_minimal() +
       NULL

#################################### colour palettes ####
### change the colours, manually
### use the palettes in library wesanderson

names(wesanderson::wes_palettes) ### list of palettes

### try GrandBudapest1 or Darjeeling2 with two levels

wesanderson::wes_palette("GrandBudapest1",2) ## plots the palette
wesanderson::wes_palette("Darjeeling2",2) ## plots the palette

col.GB2<- as.vector(unlist(wesanderson::wes_palette("GrandBudapest1",2)))
col.Darj2<- as.vector(unlist(wesanderson::wes_palette("Darjeeling2",2)))


### this puts the two colours defining the palette in a vector

print(col.GB2)

############################# user-defined themes ####

Theme1<- theme(
  title = element_text(size = 24), 
  axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
  axis.text.x = element_text(size = 16),  axis.text.y = element_text(size = 16)
)
### this theme can be used in ggplot2 objects
### it defines sizes for the main title and the axes titles
### simply add it to  a plot using the "+" operator

#################### more histograms ####

g1H1<- ggplot(Pima.te, aes(x=bmi, fill=type)) +
  geom_histogram(bins=10) + 
  scale_fill_manual(values=col.GB2) +
  NULL

print(g1H1 + Theme1)

g1H1<- ggplot(Pima.te, aes(x=bmi, fill=type)) +
  geom_histogram(bins=10) + 
  scale_fill_manual(values=col.Darj2) +
  NULL

print(g1H1 + theme_calc() + NULL) ## this syntax also works

############################# density estimates #### 

g2 <- ggplot(Pima.te, aes(x=bmi, fill=type))  + 
  ### using fill=type in the aesthetic defines groups by the
  ### levels of the variable type in the data.frame
  geom_density( alpha=0.5) + 
  ### density estimate is a smoothed histogram
  ### alpha is parametre of transparency between 0 and 1
  ### the two groups - try different values
  ### also, see what happens if you include alpha inside
  ### the aesthetics for geom_density
  labs(title="Density plot", 
       subtitle="data.frame = MASS::Pima.te",
       caption="Source: Venables & Ripley",
       x="Body mass index",
       fill="Diabetes") + 
  scale_fill_manual(values=col.GB2) +
  NULL

g2 +  theme_classic() + NULL  ### using a minimalist theme

g2 +  Theme1 + theme_classic() ### note that the order of the themes does affect the result
g2 +  theme_classic() + Theme1 ### note that the order of the themes does affect the result

### more than two classes
#################### density estimates #### 



g3 <- ggplot(Pima.te, aes(x=bp, fill=type))  + 
  ### using fill=type in the aesthetic defines groups by the
  ### levels of the variable type in the data.frame
  geom_density( alpha=0.5) + 
  ### density estimate is a smoothed histogram
  ### alpha is parametre of transparency between 0 and 1
  ### the two groups - try different values
  ### also, see what happens if you include alpha inside
  ### the aesthetics for geom_density
  labs(title="Density plot", 
       subtitle="data.frame = MASS::Pima.te",
       caption="Source: Venables & Ripley",
       x="Body mass index",
       fill="Diabetes") + 
  scale_fill_manual(values=col.GB2) +
  NULL

g2 +  theme_classic() + NULL  ### using a minimalist theme

g2 +  Theme1 + theme_classic() ### note that the order of the themes does affect the result
g2 +  theme_classic() + Theme1 ### note that the order of the themes does affect the result



############### boxplots ####


g4 <- ggplot(Pima.te, aes(y=bmi)) + 
  geom_boxplot(fill="orange") +  ### try changing colors
  labs(title="Box plot", 
       subtitle="data.frame MASS::Pima.te",
       caption="Source: Venables & Ripley",
       y="Body mass index",
       x="") + 
  scale_x_discrete(breaks = NULL) + 
  ### try with and without this line - it removes the x-axis scale
  NULL

g4
### uses the default theme in ggplot2 - gray background and grids

g4 + ggpubr::theme_pubclean() + NULL
### a cleaner plot

##### more than one boxplot ####

g4 <- ggplot(Pima.te, aes(y=bmi, fill=type)) + 
  geom_boxplot() +  ### use default palette colors
  labs(title="Box plot", 
       subtitle="data.frame MASS::Pima.te",
       caption="Source: Venables & Ripley",
       y="Body mass index",
       x="type") + 
   scale_x_continuous(breaks=c(-0.2,0.2), ### try without this
                      labels=levels(Pima.te$type)) +
  NULL

g4
### uses the default theme in ggplot2 - gray background and grids
### the x-axis scale should be modified and legend might be removed

## legend is redundant - remove it
g4 +  ggpubr::theme_pubclean() +
      theme(legend.position = 'none') + 
      NULL
### a cleaner plot with meaningful x-labels
### note how the order of the themes affects the final plot

#### use your own colors in the boxplot

### define the colours, e.g. based on a wesanderson palette
wesanderson::wes_palette("GrandBudapest1",2) ## plot the palette

GB1.2<- as.vector(unlist(wesanderson::wes_palette("GrandBudapest1",2)))
Royal1.2<- as.vector(unlist(wesanderson::wes_palette("Royal1",2)))
### two palettes each with two colours

##defines the colors

g4 <- ggplot(Pima.te, aes(y=bmi, fill=type)) + 
  geom_boxplot() +  ### use default palette colors
  #scale_fill_manual(values=GB1.2) + 
  scale_fill_manual(values=Royal1.2) + 
  #scale_fill_manual(values=c('darkgreen','lightgreen')) +
  ###only one scale_fill_manual function can be used
  ###comment/uncomment one to see changes
  labs(title="Box plot", 
       subtitle="data.frame MASS::Pima.te",
       caption="Source: Venables & Ripley",
       y="Body mass index",
       x="type") + 
  scale_x_continuous(breaks=c(-0.2,0.2), ### try without this
                     labels=levels(Pima.te$type)) +
  theme_classic() + 
  theme(legend.position = 'none') +  ## note again that the order of themes
  NULL

g4 
##

### scatterplots and transformations ####
##### example 1 -  avg brain and body wt for 28 species of land mammals 

head(MASS::Animals) ### it's good practice to use the notation x::f where x is a library and  f is one of its functions
summary(MASS::Animals) ### average body and brain of 32 species
Animals$Names<- rownames(Animals)

g1<- ggplot(Animals, aes(body, brain, label=Names)) + 
  geom_point() + 
  #geom_text(color='red') +  ### try with and without this
  scale_x_log10(breaks=c(1/32, 1/16,1/8, 1/4, 1/2,1,2,5,10,50,100,
                         200,500,1000,2000,
                         5000,10000,25000,75000),
                labels=c("1/32", "1/16","1/8","1/4","1/2","1","2","5","10",
                         "50","100","200","500","1000",
                         "2000","5000","10000","25000","75000")) + 
  scale_y_log10(breaks=c(1/2,1,2,5,10,20,50,100,200,500,1000, 
                         2000,5000), 
                labels=c("1/2", as.character(c(1,2,5,10,20,50,
                                               100,200,500,1000, 
                                               2000,5000)))) + 
  labs(x='body weight (kg)', y='brain weight (g)') + 
  theme_classic() + 
  ggtitle("brain weight vs body weight for 28 species of land mammals", 
          subtitle="sources: Rousseeuw & Leroy (1997), and Venables & Ripley (1999)") + 
  NULL ### it''s good practice to close a ggplot with a NULL value


print(g1)

### generate g1 above again commenting out the call to the geom_text() function
g1<- g1 + ggrepel::geom_text_repel(col='red')  +  ### change the labels' positions using repelling text
  NULL

print(g1)

g1<- g1 + geom_bag(color='dark green') + #### add a bagplot, a plot similar to a bivariate boxplot
  NULL 

print(g1)

### see https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/ for ideas of themes 

g1 + ggthemes::theme_fivethirtyeight() ### change the theme, note that this statement only prints the resulting plot


g1 + ggthemes::theme_stata() ### if you must.... 

g1 + ggthemes::theme_excel() ### if you must.... 

g1<- g1 + ggthemes::theme_tufte() ### Tufte's look - maximal data, minimal ink

####save the last version of g1
ggsave(filename='animals1.jpeg', plot=g1, 
       units='cm', width=21, height=14) ###save

g1.p<- plotly::ggplotly(g1) #### make an interactive plot
g1.p #### print and use interactively


########################################  histograms, boxplots & density estimates ####

### use data in object cars


g2 <- ggplot(mpg, aes(x=manufacturer)) + 
  geom_bar(aes(fill=class), width = 0.5) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") + 
  theme(legend.position = c(1/2,3/4)) +  ## run with or without this function
  NULL

print(g2)


g3 <- ggplot(mpg, aes(x=cty))  + ### city mileage (mpg)
  geom_density(aes(fill=factor(cyl)), alpha=0.8) + ####density estimate is a smoothed histogram
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders") + 
  NULL

g3 +  theme_classic() + Theme1 ### print before assigning to an object

g3 +  Theme1 + theme_classic() ### note that the order of the themes does affect the result


g4 <- ggplot(mpg, aes(x=class, y=cty)) + 
  geom_boxplot(varwidth=T, fill="orange") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage") + 
  NULL

g4 +  theme_classic() 


### use a Wes Anderson palette with 4 colours

col.GB1<- wesanderson::wes_palette('GrandBudapest1', 4) ###define the palette with 4 colours
col.GB1 ### plot the colours

col.GB1<- as.vector(col.GB1) ### extract the definition of the palette's colours
col.GB1 ### see the difference

g5 <- ggplot(mpg, aes(x=cyl, y=cty)) + 
  geom_boxplot(aes(fill=factor(cyl))) + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by number of cylinders",
       caption="Source: mpg", x="Number of cylinders", y="City Mileage") + 
  scale_fill_manual(name='Cylinders', values=col.GB1) + ### use a specific palette
  theme(legend.position="none") + ### no legend needed
  NULL 


print(g5 + theme_classic()) 
### look at the effect of theme_classic() 
print(g5)

ggplotly(g5) ### use it interactively, note the bar on top of the plot


#################################### facet_wrap ####

### loess nonparametric smoother 

g6 <- ggplot(mpg, aes(displ, hwy, group=as.factor(year))) +
  geom_point(aes(color=as.factor(year))) +
  stat_smooth(aes(color=as.factor(year)), method='loess',
              se=FALSE) +   ###compare with se=TRUE
  theme(legend.position='none') + 
  NULL

g6<- g6 +  facet_wrap(~  year) + 
     scale_color_manual(values=col.Darj2) + 
     theme(strip.background = element_rect(fill="orange")) + 
  NULL

print(g6)

#### use regression for prediction curves


g6a <- ggplot(mpg, aes(displ, hwy, group=as.factor(year))) +
  geom_point(aes(color=as.factor(year))) +
  stat_smooth(aes(color=as.factor(year)), method='lm',
              formula = y ~ splines::ns(x, df=4),## natural spline with 4 df
              se=FALSE) + 
  ###compare with se=TRUE
  theme(legend.position='none') + 
  NULL

g6a<- g6a +  facet_wrap(~  year) + 
  scale_color_manual(values=col.Darj2) + 
  theme(strip.background = element_rect(fill="lightblue")) + 
  NULL

print(g6a)

##################### 

##### raster images ####
## works in R base, not tidyverse
##

#### 
img<- jpeg::readJPEG("StillLife_SanchezCotan.jpg", native=FALSE) 
##returns an array
### see ?readJPEG and writeJPEG
dim(img) #### 950 x 1153 x 3
### the entries of each of img[i,j, ] are the components in 
### RGB color space = (read, green, blue)
### see e.g. https://en.wikipedia.org/wiki/RGB_color_model


op <- par(bg = "white")#, xlim=c(0,1153), ylim=c(0,950))
MASS::eqscplot(c(0,1153), c(0,950), type = "n", 
               xlab = "", ylab = "",  axes=FALSE,
               xlim=c(0,1153),ylim=c(0,950))
graphics::rasterImage(img, 0,0, 1153, 950)
### prints image

pts<- locator(5) 
### choose 5 points in the plot, try the center of each object
points(pts, col='red', pch=19,cex=2)
fit.img<- lm( pts$y ~ I(1/pts$x)) ### I() allows functions verbatim
### fit a hyperbola
lines(pts$x, fitted(fit.img), col='white', lwd=3)

#############################################

