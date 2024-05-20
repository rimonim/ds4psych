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

# for plotting projections
project_points_onto_line <- function(line_start, line_end, points_df) {
  
  # Calculate the direction vector of the line segment
  line_direction = line_end - line_start
  
  # Calculate the vector from the starting point of the line to the points
  vectors_to_points = t(t(points_df) - line_start)
  
  # Calculate the dot product of the vectors
  dot_products = rowSums(vectors_to_points %*% line_direction)
  
  # Calculate the squared length of the line segment
  length_squared = sum(line_direction^2)
  
  # Calculate the parameter t for the projection
  t = dot_products / length_squared
  
  # Calculate the coordinates of the projected points
  line_start_mat <- matrix(line_start, nrow = nrow(points_df), ncol = length(line_start), byrow = TRUE)
  line_direction_mat <- matrix(line_direction, nrow = nrow(points_df), ncol = length(line_start), byrow = TRUE)
  projected_points =  line_start_mat + line_direction_mat * t
  
  # Transpose the result to have one column per dimension
  result_df <- data.frame(projected_points)
  
  return(result_df)
}