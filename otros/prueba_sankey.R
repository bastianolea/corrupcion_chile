# remotes::install_github("davidsjoberg/ggsankey")
library(dplyr) 
library(ggplot2)
library(ggsankey)


corrupcion <- readRDS("app/corrupcion_datos.rds")

df <- corrupcion |> 
  mutate(monto = as.integer(monto/1000000)) |> 
  slice(1:20) |> 
  select(caso, partido, sector, monto) |> 
  arrange(sector, partido, caso) |> 
  # tidyr::uncount(monto) |> 
  make_long(caso, partido, sector, value = monto)

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")



mtcars %>%
  select(cyl, vs, am, gear, carb)

mtcars %>%
  make_long(cyl, vs, am, gear, carb) |> 
  ggplot(aes(x = x, 
                 next_x = next_x, 
                 node = node, 
                 next_node = next_node,
                 fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")


corrupcion |> 
  group_by(sector, partido) |> 
  summarize(monto = sum(monto)) |> 
  ggplot(aes(y = monto, x = sector, fill = sector)) +
  geom_col()
