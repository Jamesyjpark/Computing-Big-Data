# AUC with caret

library(caret)
# Functional version- changing the parameter value changes the training ratio

train.validate <-
  function(x){
    part_x <- createDataPartition(probands$BC.10, p = x, list = FALSE)
    training_x <- probands[part_x, ]
    testing_x <- probands[-part_x, ]
    mod_fit_x <- train(as.factor(BC.10) ~ AgeOvary + AgeBreast, data = traini
                       ng_x,
                       method = "glm", family = "binomial")
    pred_x <- predict(mod_fit_x, newdata = testing, type = "prob")
    testing_x$pred <- pred_x[, 2]
    roc70 <- roc(testing_x$BC.10, testing_x$pred)
    print(roc70)
    plot(roc70)
  }

train.validate(0.7)

# AUC with H2O
install.packages("h2o")
library(h2o)
write.csv(probands, "probands.csv")
h2o.init() # To run H2o, must call 'h2o init' without any arguments- H2o auto
matically launched
path <- "/n/home09/bst262u1825/apps/probands.csv"
breast.hex <- h2o.uploadFile(path = path, destination_frame = "breast.hex")
# Making it functional
AUC <-
  function(x){
    breast.split <- h2o.splitFrame(data = breast.hex, ratios = x, seed = 1234
    )
    train <- breast.split[[1]]
    valid <- breast.split[[2]]
    breast.glm = h2o.glm(x = c("AgeOvary", "AgeBreast"), y = "BC.10",
                         training_frame = train, validation_frame = valid,
                         family = "binomial", alpha = 0.5)
    breast.glm # return all the values necessary
    fpr <- breast.glm@model$training_metrics@metrics$thresholds_and_metric_sc
    ores$fpr
    tpr <- breast.glm@model$training_metrics@metrics$thresholds_and_metric_sc
    ores$tpr
    fpr_val = breast.glm@model$validation_metrics@metrics$thresholds_and_metr
    ic_scores$fpr
    tpr_val = breast.glm@model$validation_metrics@metrics$thresholds_and_metr
    ic_scores$tpr
    plot(fpr,tpr, type='l') # Generates a plot
    title('AUC curve')
    abline(a=0, b=1, col = "green")
    h2o.auc(breast.glm, valid=FALSE) # AUC on train
    h2o.auc(breast.glm, valid=TRUE) # AUC on test
  }

AUC(0.7) # 70% training
AUC(0.6) # 60% training
AUC(0.8) # 80% training

breast.split <- h2o.splitFrame(data = breast.hex, ratios = 0.8, seed = 1234)
train <- breast.split[[1]]
valid <- breast.split[[2]]

breast.glm = h2o.glm(x = c("AgeOvary", "AgeBreast"), y = "BC.10",
                     training_frame = train, validation_frame = valid,
                     family = "binomial", alpha = 0.5)

breast.glm # return all the values necessary
fpr <- breast.glm@model$training_metrics@metrics$thresholds_and_metric_scores$fpr
tpr <- breast.glm@model$training_metrics@metrics$thresholds_and_metric_scores$tpr
fpr_val = breast.glm@model$validation_metrics@metrics$thresholds_and_metric_scores$fpr
tpr_val = breast.glm@model$validation_metrics@metrics$thresholds_and_metric_scores$tpr

plot(fpr,tpr, type='l', main = "AUC curve- 80% Training") # Generates a plot
abline(a=0, b=1, col = "green")
h2o.auc(breast.glm, valid=FALSE) # AUC on train
h2o.auc(breast.glm, valid=TRUE) # AUC on test

## Gradient Boosting
df.gbm <- h2o.importFile(path = "/n/home09/bst262u1825/apps/probands.csv") #
path
response <- "BC.10" # pick a response for the supervised prorblem

# The response variable is an integer- we turn this into a categorical/factor for binary classificatio
df.gbm[["BC.10"]] <- as.factor(df.gbm[["BC.10"]])

# Use all otherr columnss as predictors (other than BC.10)
predictor <- setdiff(names(df.gbm), c(response, "name"))
splits <- h2o.splitFrame(
  data = df.gbm,
  ratios = c(0.69,0.3), ## only need to specify 2 fractions, the 3rd is imp
  lied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
)

train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# Only provide the required parameters- everything else is default
gbm <- h2o.gbm(x = predictor, y = response, training_frame = train)

# Get the AUC on the validation set
h2o.auc(h2o.performance(gbm, newdata = valid))
plot(gbm, main = "GBM training scoring history") # training score histroy by
the number of trees
plot(h2o.performance(gbm)) # H2O curve

## Deep learning

df.dl <- h2o.importFile(path = "/n/home09/bst262u1825/apps/probands.csv")
splits <- h2o.splitFrame(
  data = df.gbm,
  ratios = c(0.69,0.3), seed = 1234)

train <- h2o.assign(splits[[1]], "train.hex")
valid <- h2o.assign(splits[[2]], "valid.hex")

m1 <- h2o.deeplearning(
  model_id="df.dl",
  training_frame=train, ## training dataset: used to build the model
  validation_frame=valid, ## validation dataset: used for scoring and early
  stopping
  x=predictor,
  y=response,
  variable_importances=T ## Specify whether to compute variable importance
)

head(as.data.frame((h2o.varimp(m1)))) # variable importance
head(as.data.frame((h2o.varimp_plot(m1)))) # plots
summary(m1) ## model metric contains AUC for binary classficiation- AUC 0.61
plot(h2o.performance(m1)) # H2O curve


### SparklyR

options(rsparkling.sparklingwater.version = "2.1.14")
install.packages("sparklyr")
install.packages("rsparkling")
devtools::install_github("rstudio/sparklyr") # To upgrade to the latest versi on of sparklyr
library(sparklyr)
library(rsparkling)

# To update h2o package before making the connection to your Spark
detach("package:rsparkling", unload = TRUE)
if ("package:h2o" %in% search()) { detach("package:h2o", unload = TRUE) }
if (isNamespaceLoaded("h2o")){ unloadNamespace("h2o") }
remove.packages("h2o")
install.packages("h2o", type = "source", repos = "https://h2o-release.s3.amaz
onaws.com/h2o/rel-weierstrass/2/R")
library(h2o)
h2o.init()

sc <- spark_connect(master = "local", version = "2.1.0") # Connect to spark

# You can connect to both local instances of Spark as well as remote Spark cl
usters
path <- "/n/home09/bst262u1825/apps/probands.csv"
probands <- h2o.uploadFile(path = path, destination_frame = "proband")
probands <- as.data.frame(probands)

# change it from h2o data frame to a data frame so that it can be copied to spark

probands <- copy_to(sc, probands, overwrite = TRUE)

# Start by copying some datasets from R into the Spark clusters
class(probands) # Class of probands is not tbl_spark
colnames(probands)

part <- createDataPartition(probands$BC_10, p = 0.7, list = FALSE)
training <- probands[part, ]
testing <- probands[-part, ]

mod_fit <- train(as.factor(BC.10) ~ AgeOvary + AgeBreast, data = training,
                 method = "glm", family = "binomial")

pred <- predict(mod_fit, newdata = testing, type = "prob")
testing$pred <- pred[, 2]
roc70 <- roc(testing$BC.10, testing$pred)
plot(roc70)