# Useful scripts for dealing with vector embeddings

# simple dot product
dot_prod <- function(x, y){
  dot <- x %*% y
  as.vector(dot)
}

# cosine similarity function
cos_sim <- function(x, y){
  dot <- x %*% y
  normx <- sqrt(sum(x^2))
  normy <- sqrt(sum(y^2))
  as.vector( dot / (normx*normy) )
}

# Euclidean distance function
euc_dist <- function(x, y){
  diff <- x - y
  sqrt(sum(diff^2))
}

# get text embeddings by averaging word embeddings
textstat_embedding <- function(dfm, model){
  feats <- featnames(dfm)
  # find word embeddings
  feat_embeddings <- predict(model, feats, type = "embedding")
  feat_embeddings[is.na(feat_embeddings)] <- 0
  # average word embeddings of each document
  out_mat <- (dfm %*% feat_embeddings)/ntoken(dfm)
  colnames(out_mat) <- paste0("V", 1:ncol(out_mat))
  as_tibble(as.matrix(out_mat), rownames = "doc_id")
}

# for GloVe

# function to retrieve embeddings
#   `object`: an "embeddings" object (matrix with character rownames)
#   `newdata`: a character vector of tokens
#   `type`: 'embedding' gives the embeddings of newdata. 
#           'nearest' gives nearest embeddings by cosine similarity 
#           (requires the cos_sim function)
#   `top_n`: for `type = 'nearest'`, how many nearest neighbors to output?
predict.embeddings <- function(object, newdata, 
                               type = c("embedding", "nearest"), 
                               top_n = 10L){
  embeddings <- as.matrix(object)
  embeddings <- rbind(embeddings, matrix(ncol = ncol(embeddings), dimnames = list("NOT_IN_DICT")))
  newdata[!(newdata %in% rownames(embeddings))] <- "NOT_IN_DICT"
  if (type == "embedding") {
    embeddings[newdata,]
  }else{
    if(length(newdata) > 1){
      target <- as.vector(apply(embeddings[newdata,], 2, mean))
    }else{
      target <- as.vector(embeddings[newdata,])
    }
    sims <- apply(object, 1, cos_sim, target)
    embeddings <- embeddings[rev(order(sims)),]
    head(embeddings, top_n)
  }
}

# anchored vector scores (neg = 0, pos = 1)
anchored_sim <- function(point_vec, pos_vec, neg_vec){
  # direction vector of the line segment
  line_direction <- pos_vec - neg_vec
  # vector from the starting point of the line to the point
  vector_to_point <- point_vec - pos_vec
  # dot product
  dot <- vector_to_point %*% line_direction
  # squared length of the line segment
  length_squared <- sum(line_direction^2)
  # Calculate t
  t <- dot / length_squared
  as.numeric(t) + 1
}

# for plotting projections
project_points_onto_line <- function(line_start, line_end, points_df) {
  
  # Calculate the direction vector of the line segment
  line_direction <- line_end - line_start
  
  # Calculate the vector from the starting point of the line to the points
  vectors_to_points <- t(t(points_df) - line_start)
  
  # Calculate the dot product of the vectors
  dot_products <- rowSums(vectors_to_points %*% line_direction)
  
  # Calculate the squared length of the line segment
  length_squared <- sum(line_direction^2)
  
  # Calculate the parameter t for the projection
  t <- dot_products / length_squared
  
  # Calculate the coordinates of the projected points
  line_start_mat <- matrix(line_start, nrow = nrow(points_df), ncol = length(line_start), byrow = TRUE)
  line_direction_mat <- matrix(line_direction, nrow = nrow(points_df), ncol = length(line_start), byrow = TRUE)
  projected_points <- line_start_mat + line_direction_mat * t
  
  # Transpose the result to have one column per dimension
  result_df <- data.frame(projected_points)
  
  return(result_df)
}

# `data`: a dataframe with one embedding per row
# `cols`: tidyselect - columns that contain numeric embedding values
# `reduce_to`: number of dimensions to keep
# `scale`: perform scaling in addition to centering?
reduce_dimensionality <- function(data, cols, reduce_to, scale = FALSE){
  in_dat <- dplyr::select(data, {{ cols }})
  pca <- stats::prcomp(~., data = in_dat, scale = scale, rank. = reduce_to)
  out_dat <- as.data.frame(pca$x)
  dplyr::bind_cols( select(data, -{{ cols }}), out_dat )
}