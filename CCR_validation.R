library(tidyverse)
library(text)

# relevant scripts
source("vector_scripts.R")

# data from https://osf.io/jcuqk/?view_only=
CCR_fb <- read_csv("data/CCR/lda_weights.csv")

# data from https://osf.io/bu6wg/
CCR_behavioral <- read_csv("data/CCR/CCR_clean_behavioral.csv")
CCR_items <- read_csv("data/CCR/Questionnaires - Experiment - Questionnaire.csv")

# negative items
# - individualism and collectivism were used as each other's opposites

# consistent embedding scheme
contextualized_embedding <- function(x){
  textEmbed(
    x,
    model = "sentence-transformers/all-MiniLM-L12-v2", # model name
    layers = -1,  # last layer
    dim_name = FALSE,
    keep_token_embeddings = FALSE
  )
}

average_vector <- function(mat){
  mat <- as.matrix(mat)
  apply(mat, 2, mean)
}

# item embeddings
care_sbert
equality_sbert
proportionality_sbert
loyalty_sbert
authority_sbert
purity_sbert
individualism_sbert <- contextualized_embedding(CCR_items$Individualism[!is.na(CCR_items$Individualism)])
collectivism_sbert <- contextualized_embedding(CCR_items$Collectivism[!is.na(CCR_items$Collectivism)])
sd_sbert <- contextualized_embedding(CCR_items$SD[!is.na(CCR_items$SD)])
po_sbert <- contextualized_embedding(CCR_items$PO[!is.na(CCR_items$PO)])
un_sbert <- contextualized_embedding(CCR_items$UN[!is.na(CCR_items$UN)])
ac_sbert <- contextualized_embedding(CCR_items$AC[!is.na(CCR_items$AC)])
se_sbert <- contextualized_embedding(CCR_items$SE[!is.na(CCR_items$SE)])

religiosity_sbert <- contextualized_embedding(CCR_items$Religiosity[!is.na(CCR_items$Religiosity)])
conservatism_secs_sbert <- contextualized_embedding(CCR_items$Conservatism_secs[!is.na(CCR_items$Conservatism_secs)])
nfc_sbert <- contextualized_embedding(CCR_items$NFC[!is.na(CCR_items$NFC)])
tightness_sbert <- contextualized_embedding(CCR_items$Tightness[!is.na(CCR_items$Tightness)])

sd_sbert <- contextualized_embedding(CCR_items$ST[!is.na(CCR_items$SD)])
sd_sbert <- contextualized_embedding(CCR_items$CO[!is.na(CCR_items$SD)])
sd_sbert <- contextualized_embedding(CCR_items$TR[!is.na(CCR_items$SD)])
sd_sbert <- contextualized_embedding(CCR_items$HE[!is.na(CCR_items$SD)])
sd_sbert <- contextualized_embedding(CCR_items$BE[!is.na(CCR_items$SD)])


# response embeddings
values_survey_sbert <- contextualized_embedding(CCR_behavioral$ValuesSurvey)
saveRDS(values_survey_sbert, "data/values_survey_sbert.rds")
behaviors_survey_sbert <- contextualized_embedding(CCR_behavioral$BehaviorsSurvey)
saveRDS(behaviors_survey_sbert, "data/behaviors_survey_sbert.rds")

# rejoin other variables
CCR_behavioral <- values_survey_sbert$texts[[1]] |>
  mutate(ID = hippocorpus_df$AssignmentId)

# rejoin other variables
hippocorpus_sbert <- hippocorpus_df |>
  rename(ID = AssignmentId) |> 
  left_join(hippocorpus_sbert)