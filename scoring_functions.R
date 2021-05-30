# GLM - log - Poisson
predict_cnt_prob_poisson = function(model, test){
  prediction = predict(model, test, type="response")
  cnt_probabilities = matrix(0, length(prediction), length(u_cnt))
  for (i in 1:length(prediction)){
    cnt_probabilities[i,] = ppois(u_cnt, lambda = prediction[i], log = FALSE)
  }
  cnt_probabilities
}

# Zero Inflated Poisson
predict_cnt_prob_zero_poisson = function(model, test){
  prediction = predict(model, test, type="prob", at=u_cnt)
  cnt_probabilities = matrix(0, nrow(test), length(u_cnt))
  for (i in 1:nrow(test)){
    cnt_probabilities[i,] = cumsum(prediction[i,])
  }
  cnt_probabilities
}

# Mixture CNT=0 and Gaussian Id Link for logBA
predict_ba_prob_gaussian_0 = function(p0, model, test){
  prediction = predict(model, test, type="response", se.fit=TRUE)
  ba_probabilities = matrix(0, nrow(test), length(u_ba))
  for (i in 1:nrow(test)){
    ba_probabilities[i,1] = p0[i]
    mu = prediction$fit[i]
    sd = prediction$se.fit[i]
    ba_probabilities[i,-1] = pnorm(log(u_ba[-1]), mean = mu, sd = sd)
  }
  ba_probabilities
}

# Gaussian Id Link for log(BA+1)
predict_ba_prob_gaussian_1 = function(model, test){
  prediction = predict(model, test, type="response", se.fit=TRUE)
  ba_probabilities = matrix(0, nrow(test), length(u_ba))
  for (i in 1:nrow(test)){
    mu = prediction$fit[i]
    sd = prediction$se.fit[i]
    ba_probabilities[i,] = pnorm(log(u_ba+1), mean = mu, sd = sd)
  }
  ba_probabilities
} 


#gbm
predict_ba_prob_gaussian_2 = function(model, test, type){
  prediction = predict(model, test, type=type)
  ba_probabilities = matrix(0, nrow(test), length(u_ba))
  for (i in 1:nrow(test)){
    mu = prediction[i]   #$fit[i]
    #sd = prediction$se.fit[i]
    sd = sqrt(sum((mu-mean(prediction))^2)/(length(test)-1))
    ba_probabilities[i,] = pnorm(log(u_ba+1), mean = mu, sd = sd)
  }
  ba_probabilities
}

predict_ba_prob_gaussian_gbm1 = function(model, test){
  prediction = predict(model, test, type="response")
  #sd = sqrt(sum((mu-mean(prediction))^2)/(length(test)-1)) # same for all -_-
  sd = sd(prediction)
  ba_probabilities = matrix(0, nrow(test), length(u_ba))
  for (i in 1:nrow(test)){
    mu = prediction[i]
    ba_probabilities[i,] = pnorm(log(u_ba+1), mean = mu, sd = sd)
  }
  ba_probabilities
}


# Scoring function
get_score_cnt = function(prediction_cnt, obs, u_cnt, weights_cnt){
  distr_obs = c()
  for(k in 1:length(u_cnt)){
    distr_obs = cbind(distr_obs, ifelse(u_cnt[k] < obs, 0, 1))
  }
  weights_mat = matrix(weights_cnt, ncol = length(weights_cnt), nrow = length(obs), byrow = TRUE)
  score_cnt = sum(weights_mat * (distr_obs - prediction_cnt)^2)
  score_cnt
}

get_score_ba = function(prediction_ba, obs, u_ba, weights_ba){
  distr_obs = c()
  for(k in 1:length(u_ba)){
    distr_obs = cbind(distr_obs, ifelse(u_ba[k] < obs, 0, 1))
  }
  weights_mat = matrix(weights_ba, ncol = length(weights_ba), nrow = length(obs), byrow = TRUE)
  score_ba = sum(weights_mat * (distr_obs - prediction_ba)^2)
  score_ba
} 
