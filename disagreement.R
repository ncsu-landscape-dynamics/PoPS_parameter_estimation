library(rgdal)
library(raster)
library(caret)
library(diffeR)

quantity_allocation_disagreement<- function(reference, comparison, parameters=NULL){
  counter = 0
  testing <- data.frame(row = rep(0, ncell(reference)), column = 0, reference = 0, prediction = 0)
  for (i in 1:nrow(reference)){
    for(j in 1:ncol(reference)){
      counter = counter + 1
      testing$row[counter] <- i
      testing$column[counter] <- j
      testing$reference[counter] <- reference[i,j]
      testing$prediction[counter] <- comparison[i,j]
    }
  }
  

  confusion_matrix <- confusionMatrix(factor(testing$prediction), factor(testing$reference))
  conf_matrix <- crosstabm(reference, comparison)
  difference_table <- diffTablej(conf_matrix)
  correct = 0
  wrong = 0
  quantity_allocation_disagreement <- data.frame(category = difference_table[,1],quantity_disagreement = difference_table[,2], allocation_disagreement = difference_table[,3]+difference_table[,4], total_disagreement = 0 , omission = 0, commission = 0 ,total_allocation_distance = 0)
  for (i in 1:ncol(conf_matrix)){
    quantity_allocation_disagreement$omission[i] <- ifelse(sum(conf_matrix[i,])-sum(conf_matrix[,i])>0,sum(conf_matrix[i,])-sum(conf_matrix[,i]), 0)
    quantity_allocation_disagreement$commission[i] <- ifelse(sum(conf_matrix[i,])-sum(conf_matrix[,i])<0,abs(sum(conf_matrix[i,])-sum(conf_matrix[,i])), 0)
    correct[i] <- conf_matrix[i,i]
    wrong[i] <- sum(conf_matrix[,i])-conf_matrix[i,i]
    }
  wrong <- wrong[wrong !=0]
  correct <- correct[correct !=0]
  quantity_allocation_disagreement$odds_ratio[nrow(quantity_allocation_disagreement)] <- prod(correct)/prod(wrong)
  quantity_allocation_disagreement$total_disagreement <- quantity_allocation_disagreement$quantity_disagreement + quantity_allocation_disagreement$allocation_disagreement
  quantity_allocation_disagreement$kappa[nrow(quantity_allocation_disagreement)] <- confusion_matrix$overall[2]
  
  ## compute distances to closest point of disagreement
  # dist <- gridDistance(compare, 1, omit=0)
  # dist[is.na(dist)] <- 0
  # dist <- as.matrix(dist)
  # output$total_allocation_distance = sum(dist)
  
  return(quantity_allocation_disagreement)
}
