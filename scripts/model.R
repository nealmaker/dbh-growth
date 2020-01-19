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
         site_class, slope, aspect, lat, lon, elev, plot) %>% 
  rename(dbh = dbh_mid, cr = cr_mid, crown_class = crown_class_s,
         tree_class = tree_class_s, ba = ba_mid, bal = bal_mid,
         forest_type = forest_type_s, stocking = stocking_s)

# test set is 20% of full dataset
test_size <- .2

# define test set based on plots (to make it truely independent)
set.seed(10)
test_plots <- sample(unique(nf_fia$plot), 
                     size = round(test_size*length(unique(nf_fia$plot))), 
                     replace = FALSE)

index <- which(nf_fia$plot %in% test_plots)
train <- nf_fia[-index,]
test <- nf_fia[index,]


#####################################################################
# Preprocess data
#####################################################################

preproc <- preProcess(train[,-1], method = c("center", "scale", "YeoJohnson"))
train_tran <- predict(preproc, train)
test_tran <- predict(preproc, test)

x <- select(train_tran, -plot, -dbh_rate)
y <- select(train_tran, dbh_rate)


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
