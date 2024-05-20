library(tidyverse)
library(RedditExtractoR)

# NOTE: Variable names output by RedditExtractoR do not match those from vosonSML!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Retrieve the most recent posts from r/RedditAPIAdvocacy

APIAdvocacy_posts <- find_thread_urls(
  subreddit = "RedditAPIAdvocacy", 
  sort_by = "new",
  period = "all"
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Retrieve Hadley's r/dataisbeautiful AMA thread

# List of thread urls (in this case only one)
threads <- c("https://www.reddit.com/r/dataisbeautiful/comments/3mp9r7/im_hadley_wickham_chief_scientist_at_rstudio_and/")

# Retrieve the data
## Since the Reddit API is open, we don't need
## to give any passwords to Authenticate()
hadley_threads <- get_thread_content(threads)

# Peak at Hadley's responses
hadley_threads[["comments"]] |>
  filter(author == "hadley") |> 
  select(comment_id, comment) |> 
  head()


# plot thread as tree
library(ggraph)

hadley_threads[["comments"]] |> 
  mutate(
    level = str_count(comment_id, "_") + 1L,
    parent = str_remove(comment_id, "_[[:digit:]]+$"),
    parent = if_else(level == 1, "0", parent)
  ) |> 
  select(parent, comment_id) |> 
  tidygraph::as_tbl_graph() |> 
  ggraph(layout = 'tree', circular = FALSE) +
  geom_edge_diagonal(alpha = .2, linewidth = 1) +
  geom_node_point(shape = 21, fill = "orangered") +
  theme_void()
