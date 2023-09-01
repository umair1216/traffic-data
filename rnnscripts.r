data <- read.csv("~/Downloads/movement-speeds-quarterly-by-hod-san-francisco-2020-Q1.csv")
segment_data <- list()
for(segment in unique(data$segment_id)) {
    segment_data[[segment]] <- data[data$segment_id == segment, "speed_mph_mean"]
}
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}
split_data <- function(data, train_ratio = 0.8) {
    n <- length(data)
    train_size <- floor(train_ratio * n)
    train_data <- array(data[1:train_size], dim = c(train_size, 1))
    test_data <- array(data[(train_size 1):n], dim = c(n - train_size, 1))
    return(list(train_data = train_data, test_data = test_data))
}
build_model <- function() {
    model <- keras_model_sequential() %>%
        layer_lstm(units = 50, return_sequences = TRUE, input_shape = list(NULL, 1)) %>%
        layer_lstm(units = 50, return_sequences = TRUE) %>%
        layer_lstm(units = 50) %>%
        layer_dense(units = 1)
    model %>% compile(
        optimizer = 'adam',
        loss = 'mean_squared_error'
    )
    return(model)
}
mse_list <- c()
for(segment in names(segment_data)) {
    data <- normalize(segment_data[[segment]])  
    split <- split_data(data)
    X_train <- split$train_data
    y_train <- split$train_data
    X_test <- split$test_data
    y_test <- split$test_data
    
    model <- build_model()
    
    history <- model %>% fit(
        x = X_train,
        y = y_train,
        epochs = 50,
        batch_size = 64,
        verbose = 1
    )
    
    predictions <- model %>% predict(X_test)
    
    mse <- mean((predictions - y_test)^2)
    mse_list <- c(mse_list, mse)
    
    print(paste("MSE for segment", segment, ":", mse))
}
average_mse <- mean(unlist(mse_list))
print(paste("Average MSE:", average_mse))
[1] "Average MSE: 0.0557792112070032"
