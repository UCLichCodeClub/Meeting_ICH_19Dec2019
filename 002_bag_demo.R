# load the functions from this Gist:
devtools::source_gist("00772ccea2dd0b0f1745", filename = "000_geom_bag.r")
devtools::source_gist("00772ccea2dd0b0f1745", filename = "001_bag_functions.r")

# then run the demo code:

library(ggplot2)
set.seed(9)

n <- 2100
data <- data.frame(x = rnorm(n),
                   y = rnorm(n),
                   z = unlist(lapply(letters[1:3], function(i) rep(i, n/3))),
                   a = unlist(lapply(letters[4:6], function(i) rep(i, n/3)))
)

# just the bag
p <- ggplot(data, aes(x, y)) +
  geom_bag() +
  theme_bw()

# have a look
p

# with points
p + geom_point()

# with multiple groups, facetted
p <- ggplot(data, aes(x, y, colour = z, fill = z)) +
  geom_bag() +
  facet_wrap(~z)

# have a look
p

# with points
p + geom_point()

# with multiple groups on the same plot
p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species, fill = Species)) +
  geom_bag() +
  theme_minimal()

# have a look
p

# with points
p + geom_point()

# compare to some examples from the aplpack bagplot docs (http://search.r-project.org/library/aplpack/doc/bagplot.pdf)

library(rpart)
cardata <- car.test.frame[,6:7]
ggplot(cardata, aes(Weight, Disp.)) +
  geom_point() +
  geom_bag() +
  theme_bw()
# looks good


seed <- 222; n <- 200
data <- data.frame(x = rnorm(n, mean = 100),
                   y = rnorm(n, mean = 300))
datan <- data.frame(x = c(data$x, 106),
                    y = c(data$y, 294) * 100)

ggplot(datan, aes(x, y)) +
  geom_point() +
  geom_bag() +
  theme_bw() 
# excludes outlier as expected
