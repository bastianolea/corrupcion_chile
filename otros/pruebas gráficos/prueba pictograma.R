# prueba gráfico de pictograma ----

# https://stackoverflow.com/questions/25014492/geom-bar-pictograms-how-to


library(ggplot2)
library(dplyr)


#png ----
library(png)
library(grid)

fill_images <- function() {
  l <- list()
  for (i in 1:nrow(df3)) 
  {
    for (j in 1:floor(df3$units[i]))
    {
      #seems redundant, but does not work if moved outside of the loop (why?)
      img <- readPNG("pruebas gráficos/R_logo.png")
      # img <- svgparser::read_svg("iconos/billete.svg")
      g <- rasterGrob(img, interpolate=TRUE)
      l <- c(l, annotation_custom(g, xmin = i-1/2, xmax = i+1/2, ymin = j-1, ymax = j))
    }
  }
  l
}

#datos de prueba
df3 <- data.frame(units = c(1.3, 1.8, 2.7, 4.2, 4.7, 6.7, 20), 
                  what = c('Wikipedia', 'London Olympic Park', 'Aircraft carrier', 
                           'The Great Pyramid', 'Stonehenge', 'Burj Khalifas', 
                           'Empire State Building')) |> 
  mutate(what = as.factor(what),
         what = forcats::fct_reorder(what, units))

#graficar
ggplot(df3, aes(what, units)) + 
  geom_bar(fill="white", colour="white", alpha=1, stat="identity") +
  coord_flip() + 
  # scale_y_continuous(breaks=seq(0, 20, 2)) + 
  scale_x_discrete() + 
  theme_minimal() + 
  theme(axis.title.x  = element_blank(), axis.title.y  = element_blank()) + 
  fill_images()



#svg ----

# remotes::install_github('coolbutuseless/ggsvg')
library(ggsvg)

# svg_url <- 'https://www.svgrepo.com/download/289000/jellyfish.svg'
svg_txt <- paste(readLines("iconos/billete.svg"), collapse = "\n")
grid::grid.draw( svg_to_rasterGrob(svg_txt) )


test_df <- data.frame(
  x = runif(10), 
  y = runif(10), 
  count = sample(3:5, 10, T),
  type  = sample(c('a', 'b', 'c'), 10, T))

test_df

ggplot(test_df) + 
  geom_point_svg(aes(x, y), svg = svg_txt) + 
  theme_bw()




# ggimage ----

# install.packages("ggimage")
library(ggimage)

# img <- list.files(system.file("extdata", package="ggimage"),
#                   pattern="png", full.names=TRUE)
img <- c("iconos/billete.svg", "iconos/billete2.svg", "iconos/billete3.svg", "iconos/billete4.svg")

d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(img, size=20, replace = TRUE),
                size = sample(c(.1, .12, .13, .09), size=20, replace = TRUE)
)

ggplot(d, aes(x, y)) + 
  geom_image(aes(image=image), size=.1, color = "red") +
  geom_text(aes(label=image), nudge_y = -0.35)

ggplot(d, 
       aes(x, y)) + 
  geom_image(aes(image=image), color = "green4") +
  theme_void()

ggplot(d, 
       aes(x, y)) + 
  geom_image(image = "iconos/billete2.svg", color = "green4") +
  theme_void()
