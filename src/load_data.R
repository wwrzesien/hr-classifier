load_data <- function(dir="./data/", trainset_filename="aug_train.csv", testset_filename="aug_test.csv", resultset_filename="test_answers.npy") {
  train <- read.csv(paste(dir, trainset_filename, sep=""), na.strings=c(""," ","NA"))
  test <- read.csv(paste(dir, testset_filename, sep=""), na.strings=c(""," ","NA"))
  results <- npyLoad("./data/test_answers.npy")

  # add target results to test data
  test$target = results

  return(list(train=train, test=test))
}