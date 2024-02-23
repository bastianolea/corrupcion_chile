library(waffle)

data.frame(
  parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  col = rep(c("navy", "black", "maroon"), 3),
  fct = c(
    rep("Thing 1", 3),
    rep("Thing 2", 3),
    rep("Thing 3", 3)
  )
) -> xdf

xdf %>%
  count(parts, wt = vals) %>%
  ggplot(
    aes(fill = parts, values = n)
  ) +
  geom_waffle(
    n_rows = 20,
    size = 0.33, 
    colour = "white",
    flip = TRUE
  )

corrupcion_2 |> 
  filter(año >= 2020) |> 
  slice(1:5) |> 
  mutate(n = round(monto/1000000000, 0)) |> 
  select(caso, monto, n) |> 
  mutate(caso = as.character(caso)) |> 
  ggplot(aes(fill = caso, values = n)) +
  geom_waffle(
    #make_proportional = TRUE,
    color = "white",
    size = .5,radius = unit(1, "mm"),
    n_rows = 10, flip = T 
    # flip = TRUE
  ) +
  facet_wrap(
    ~caso, 
    ncol = 1,
    strip.position = "left"
  ) +
  theme(legend.position = "none") +
  theme_enhance_waffle()


storms_df <- storms %>% 
  filter(year >= 2010) %>% 
  slice(1:100) |> 
  count(year, status) 

ggplot(
  data = storms_df, 
  aes(fill = status, values = n)
) +
  geom_waffle(
    color = "white", 
    size = .25, 
    n_rows = 10, 
    flip = TRUE
  ) +
  facet_wrap(
    ~year, 
    nrow = 1, 
    strip.position = "bottom"
  ) +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 10, # make this multiplier the same as n_rows
    expand = c(0,0)
  ) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    x = "Year", y = "Count",
    title = "Faceted Waffle Bar Chart",
    subtitle = "{dplyr} storms data"
  ) +
  theme_minimal(
    base_family = "Roboto Condensed"
  ) +
  theme(
    panel.grid = element_blank(), 
    axis.ticks.y = element_line()
  ) +
  guides(
    fill = guide_legend(reverse = TRUE)
  )  

# install_fa_fonts()

xdf %>%
  count(parts, wt = vals) %>%
  ggplot(
    aes(label = parts, values = n)
  ) +
  geom_pictogram(
    n_rows = 10, 
    aes(colour = parts), 
    flip = TRUE, 
    # make_proportional = TRUE
  ) +
  scale_color_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("apple-alt", "bread-slice", "pizza-slice"),
    labels = c("Fruit", "Sammich", "Pizza")
  ) +
  coord_equal() +
  # hrbrthemes::theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(
    legend.key.height = unit(2.25, "line"),
    legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)
  ) +
  facet_wrap(~parts, ncol = 1)


