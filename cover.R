library(tidyverse)
library(vosonSML)
library(text)
library(caret)
library(pls)

#------------------------------------------------------------------------------#
# Data Collection
#------------------------------------------------------------------------------#

## emobank (https://github.com/JULIELab/EmoBank/tree/master)
## VALENCE-AROUSAL-DOMINANCE
emobank <- read_csv("https://raw.githubusercontent.com/JULIELab/EmoBank/master/corpus/emobank.csv") |> 
  filter(abs(as.numeric(scale(V))) > 1 | abs(as.numeric(scale(A))) > 1 | abs(as.numeric(scale(D))) > 1)

## get data from r/relationship_advice (takes a while)
## collect a listing of the 5000 top threads by upvote of all time
#
# ra_posts <- Authenticate("reddit") |>
#   Collect(endpoint = "listing", subreddits = "relationship_advice",
#           sort = "top", period = "all", max = 5000, verbose = TRUE) |>
#   select(selftext, title, ups, upvote_ratio, score, created, edited, num_comments)
# saveRDS(ra_posts, "data/ra_posts.rds")

ra_posts <- readRDS("data/ra_posts.rds")

ra_posts <- ra_posts |> 
  mutate(word_length = str_count(selftext, ' ') + 1L,
         selftext_trunc = if_else(word_length > 200, word(selftext, 1L, 200L), selftext))

#------------------------------------------------------------------------------#
# Processing
#------------------------------------------------------------------------------#

# BERT Embedding (takes hours on cpu - plan accordingly)

# emobank_distilroberta <- textEmbed(
#   emobank$text, 
#   layers = -2,
#   model = "distilroberta-base",
#   dim_name = FALSE,
#   keep_token_embeddings = FALSE,
#   max_token_to_sentence = 3
#   )
# emobank_distilroberta <- emobank_distilroberta$texts[[1]] |> 
#   mutate(id = emobank$id)
# emobank_distilroberta <- emobank |> 
#   left_join(emobank_distilroberta)

emobank_distilroberta <- readRDS("data/emobank_distilroberta.rds")

# ra_posts_distilroberta <- textEmbed(
#   ra_posts$selftext_trunc, 
#   layers = -2,
#   model = "distilroberta-base",
#   dim_name = FALSE,
#   keep_token_embeddings = FALSE,
#   max_token_to_sentence = 3,
#   logging_level = "info"
#   )
# ra_posts_distilroberta <- ra_posts_distilroberta$texts[[1]] |> 
#   mutate(id = 1:n())
# ra_posts_distilroberta <- ra_posts |> 
#   mutate(id = 1:n()) |> 
#   left_join(ra_posts_distilroberta)
ra_posts_distilroberta <- readRDS("data/ra_posts_distilroberta.rds")

# PLS Regression (to get principle component with respect to each emotional dimension)

pls_V <- train(
  V ~ ., data = select(emobank_distilroberta, V, Dim1:Dim768), method = "pls",
  scale = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(pls_V)

emobank |> 
  mutate(comp1 = predict(pls_V, ncomp = 1),
         V_scaled = scale(V)) |> 
  arrange(desc(comp1)) |> View()


pls_A <- train(
  A ~ ., data = select(emobank_distilroberta, A, Dim1:Dim768), method = "pls",
  scale = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(pls_A)

pls_D <- train(
  D ~ ., data = select(emobank_distilroberta, D, Dim1:Dim768), method = "pls",
  scale = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(pls_D)

# Add model predictions to Reddit posts
ra_posts <- ra_posts |> 
  mutate(
    V_pred = predict(pls_V, ncomp = pls_V$bestTune$ncomp,
                     newdata = ra_posts_distilroberta),
    A_pred = predict(pls_A, ncomp = pls_A$bestTune$ncomp,
                     newdata = ra_posts_distilroberta),
    D_pred = predict(pls_D, ncomp = pls_D$bestTune$ncomp,
                     newdata = ra_posts_distilroberta)
    )

#------------------------------------------------------------------------------#
# Graphing
#------------------------------------------------------------------------------#
library(grid) 
bg <- grid::rasterGrob(c("#132749", "#2F3157"), 
                 width=unit(1,"npc"), 
                 height = unit(1,"npc"), 
                 interpolate = TRUE)

ytop <- 3.53
ybottom <- 2.78
yheight <- ytop - ybottom

cover <- ra_posts |> 
  ggplot(aes(V_pred, D_pred, color = A_pred, size = num_comments)) +
    annotation_custom(bg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    annotate("text",
             x = min(range(ra_posts$V_pred)), y = ytop - 2*yheight/10,
             size = 20, family = "Avenir Next",
             lineheight = .9, hjust = 0,
             label = "Data Science\nfor Psychology",
             color = "white"
             ) +
    annotate("text",
             x = min(range(ra_posts$V_pred)), y = ytop - 4*yheight/10,
             size = 10, family = "Avenir Next",
             lineheight = .9, hjust = 0,
             label = "with examples in R",
             color = "#F27B68"
    ) +
    annotate("text",
             x = max(range(ra_posts$V_pred)), y = ybottom + yheight/15,
             size = 8, family = "Avenir Next",
             lineheight = 1.2, hjust = 1,
             label = "Almog Simchon\n& Louis Teitelbaum",
             color = "white"
    ) +
    geom_point(alpha = .8, stroke = NA) +
    geom_hline(color = "white", linewidth = 1, yintercept = ytop) +
    colorspace::scale_color_continuous_sequential(
      palette = "ag Sunset",
      guide = "none"
      ) +
    scale_size_area(guide = "none", max_size = 12) +
    coord_cartesian(xlim = c(range(ra_posts$V_pred)[1],
                             range(ra_posts$V_pred)[2]), 
                    ylim = c(ybottom, ytop)) +
    theme_void()

ggsave(filename = "images/cover.png", plot = cover,
       width = 16, height = 16, units = "cm")

favicon <- ra_posts |> 
  ggplot(aes(V_pred, D_pred, color = A_pred, size = ups)) +
  annotate("point", x = mean(range(ra_posts$V_pred)), y = mean(range(ra_posts$D_pred)),
           size = 32, shape = 21, fill = "#132749") +
  geom_point(alpha = .8, stroke = NA) +
  colorspace::scale_color_continuous_sequential(
    palette = "ag Sunset",
    guide = "none"
  ) +
  scale_size_area(guide = "none", max_size = 7) +
  theme_void() +
  theme(
    panel.background = element_rect(fill='transparent', colour = 'transparent'),
    plot.background = element_rect(fill='transparent', colour = 'transparent')
  )

ggsave(filename = "images/favicon.png", plot = favicon,
       width = 3, height = 3, units = "cm")
