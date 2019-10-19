#### SLOW. RUN OVERNIGHT. ####

library("tidyverse")
library("caret")
library("Rborist")

temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(dbh_rate, spp, dbh_mid, cr_mid, crown_class_s, tree_class_s,
         ba_mid, bal_mid, forest_type_s, stocking_s, landscape, 
         site_class, slope, aspect, lat, lon, elev) 

# test set is 20% of full dataset
test_size <- .2

set.seed(10)
index <- createDataPartition(nf_fia$dbh_rate, times = 1, p = test_size, list = FALSE)

train <- nf_fia[-index,]
test <- nf_fia[index,]


#####################################################################
# Preprocess data
#####################################################################

preproc <- preProcess(train[,-1], method = c("center", "scale", "YeoJohnson"))
train_tran <- predict(preproc, train)
test_tran <- predict(preproc, test)

x <- train_tran[,-1]
y <- train_tran[,1]


#####################################################################
# Train full model
#####################################################################

# calculates RMSE:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


set.seed(1)
model_dbh_full <- 
  train(x, y,
        method = "ranger",
        num.trees = 200,
        importance = 'impurity',
        tuneGrid = data.frame(mtry = seq(12, 16, by = 2),
                              splitrule = rep("variance", 3),
                              min.node.size = rep(5, 3)))


#####################################################################
# Results full model
#####################################################################

model_dbh_full$results

plot(model_dbh_full)

varImp(model_dbh_full, scale = F)


#####################################################################
# Train operational model
#####################################################################

x2 <- select(x, -landscape, -site_class, -crown_class_s, 
             -tree_class_s, -aspect, -slope, -stocking_s, -elev)

set.seed(1)
model_dbh_op <- 
  train(x2, y,
        method = "ranger",
        num.trees = 200,
        importance = 'impurity',
        tuneGrid = data.frame(mtry = seq(4, 8, by = 2),
                             splitrule = rep("variance", 3),
                             min.node.size = rep(5, 3)))


#####################################################################
# Results operational model
#####################################################################

model_dbh_op$results

plot(model_dbh_op)

varImp(model_dbh_op, scale = F)


#####################################################################
# Save
#####################################################################

save(preproc, file = "rda/preproc.rda")
save(model_dbh_full, file = "rda/model-dbh-full.rda")
save(model_dbh_op, file = "rda/model-dbh-op.rda")
