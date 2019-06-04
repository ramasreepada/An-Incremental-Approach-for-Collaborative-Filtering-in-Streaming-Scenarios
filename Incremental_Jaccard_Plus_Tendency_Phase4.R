
#############################################################################################
initial_time = proc.time()
#### TRAIN FOR third BATCH######################################
for(i in 1:nrow(train_set_part4))
{
  user_i = train_set_part4[i,1]
  item_i = train_set_part4[i,2]
  rating_i = train_set_part4[i,3]
  
  # update all the required details
  if(rating_i >= 4)
  {
    #### FIND ALL THE USERS WHO LIKED IT ALREADY
    users_who_liked_itemI = as.matrix(users_who_liked_each_item[item_i,(1:items_liked_cardinality[item_i])])
    
    
    ### UPDATE THE INTERSECTION MATRIX
    if((as.numeric(nrow(users_who_liked_itemI)) > 0) && items_liked_cardinality[item_i] > 0)
    {
      intersect_liked_users[user_i, users_who_liked_itemI] = intersect_liked_users[user_i, users_who_liked_itemI] + 1
      intersect_liked_users[users_who_liked_itemI, user_i] = intersect_liked_users[users_who_liked_itemI, user_i] + 1
    }
    
    # increment the cardinality of items and users
    users_liked_cardinality[user_i] = users_liked_cardinality[user_i] + 1
    items_liked_cardinality[item_i] = items_liked_cardinality[item_i] + 1
    
    users_who_liked_each_item[item_i, items_liked_cardinality[item_i]] = user_i
  }
  
  else
  {
    #### FIND ALL THE USERS WHO LIKED IT ALREADY
    users_who_disliked_itemI = as.matrix(users_who_disliked_each_item[item_i, (1:items_disliked_cardinality[item_i])])
    
    ### UPDATE THE INTERSECTION MATRIX
    if((as.numeric(nrow(users_who_disliked_itemI)) > 0) && items_disliked_cardinality[item_i] > 0)
    {
      
      intersect_disliked_users[user_i, users_who_disliked_itemI] = intersect_disliked_users[user_i, users_who_disliked_itemI] + 1
      intersect_disliked_users[users_who_disliked_itemI, user_i] = intersect_disliked_users[users_who_disliked_itemI, user_i] + 1
    }
    
    # increment the cardinality of items and users
    users_disliked_cardinality[user_i] = users_disliked_cardinality[user_i] + 1 
    items_disliked_cardinality[item_i] = items_disliked_cardinality[item_i] + 1
    users_who_disliked_each_item[item_i, items_disliked_cardinality[item_i]] = user_i
  }
  
  # compute total user rating value and item rating value
  users_total_rating_values[user_i] = users_total_rating_values[user_i] + rating_i
  items_total_rating_values[item_i] = items_total_rating_values[item_i] + rating_i
  
  # compute user mean and item mean
  users_mean[user_i] = users_total_rating_values[user_i]/(users_liked_cardinality[user_i] + users_disliked_cardinality[user_i])
  
  #store the old item mean value temporarily
  old_item_mean = items_mean[item_i]
  
  items_mean[item_i] = items_total_rating_values[item_i]/(items_liked_cardinality[item_i] + items_disliked_cardinality[item_i])
  
  users_temp_1 = 0
  
  if((items_liked_cardinality[item_i] - 1) > 0)
  {
    users_temp_1 = users_who_liked_each_item[item_i, (1:(items_liked_cardinality[item_i] - 1))]
  }
  
  users_temp_2 = 0
  
  if((items_disliked_cardinality[item_i] - 1 ) > 0)
  {
    users_temp2 = users_who_disliked_each_item[item_i, (1:(items_disliked_cardinality[item_i] - 1))]
  }
  users_for_intersect = 0
  
  if(users_temp_1 > 0 && users_temp_2 > 0)
  {
    users_for_intersect = rbind(users_temp_1, users_temp_2)
  }
  
  else if(users_temp_1 == 0 && users_temp_2 > 0)
  {
    users_for_intersect = users_temp_2
  }
  
  else if(users_temp_1 > 0 && users_temp_2 == 0)
  {
    users_for_intersect = users_temp_1
  }
  
  
  intersect_all_users[user_i, users_for_intersect] = intersect_all_users[user_i, users_for_intersect] + 1
  intersect_all_users[users_for_intersect, user_i] = intersect_all_users[users_for_intersect, user_i] + 1
  
  ############################################################################################
  
  # tendency of user updation
  
  ############################################################################################
  
  # tendency of user
  
  # store the mean rating values for each user based on the items rated by the users
  item_mean_for_each_user_for_tendency[user_i] = item_mean_for_each_user_for_tendency[user_i] + items_mean[item_i]
  
  
  # now update for each user who rated item i
  item_mean_for_each_user_for_tendency[users_for_intersect] = item_mean_for_each_user_for_tendency[users_for_intersect] - old_item_mean + items_mean[item_i]
  
  remove(old_item_mean) 
}
final_time = proc.time()

total_time = final_time - initial_time
#############################################################################

#### testing phase ##############

#############################################################################

# incremental train set
incremental_train_set = rbind(incremental_train_set, train_set_part4)


# precompute the user similarity matrix
user_similarity = matrix(0, nrow=nrow(intersect_liked_users), ncol = ncol(intersect_liked_users))


for(i in 1:(nrow(user_similarity) - 1))
{
  for(j in (i+1): nrow(user_similarity))
  {
    # if((users_liked_cardinality[i] + users_disliked_cardinality[i] + users_liked_cardinality[j] + users_disliked_cardinality[j] - intersect_all_users[i,j]) > 0)
    if((intersect_all_users[i,j]) > 0)
    {
      # user_similarity[i,j] = (intersect_liked_users[i,j] + intersect_disliked_users[i,j])/(users_liked_cardinality[i] + users_disliked_cardinality[i] + users_liked_cardinality[j] + users_disliked_cardinality[j] - intersect_all_users[i,j])
      user_similarity[i,j] = (intersect_liked_users[i,j] + intersect_disliked_users[i,j])/(intersect_all_users[i,j])
      user_similarity[j,i] = user_similarity[i,j]
    }
    
  }
}

ratings_UI_matrix = matrix(0, nrow = nrow(users_mean), ncol = nrow(items_mean))

for(i in 1:nrow(incremental_train_set))
{
  ratings_UI_matrix[incremental_train_set[i,1], incremental_train_set[i,2]] = incremental_train_set[i,3]
  
}

ratings_UI_test = matrix(0, nrow = nrow(users_mean), ncol = nrow(items_mean))

for(i in 1:nrow(train_set_part5))
{
  ratings_UI_test[train_set_part5[i,1], train_set_part5[i,2]] = train_set_part5[i,3]
  
}


mae = 0
Beta = 0.5
  
results = matrix(0, nrow = nrow(train_set_part5), ncol = 2)

for(i in 1:as.numeric(nrow(train_set_part5)))
{
  user_i = train_set_part5[i,1]
  item_i = train_set_part5[i,2]
  expected_rating = train_set_part5[i,3]
  
  ### compute the tendency of user_i
  if((users_liked_cardinality[user_i] + users_disliked_cardinality[user_i]) > 0)
  {
    tendency_user = (users_total_rating_values[user_i] - item_mean_for_each_user_for_tendency[user_i])/(users_liked_cardinality[user_i] + users_disliked_cardinality[user_i])
  }
  
  else
  {
    tendency_user = 0
  }
  
  
  # find the neighbors of active user to compute item tendency
  
  # find the users who rated target item
  
  if(items_liked_cardinality[item_i] > 0)
  {
    users_who_rated_target_item_liked = as.matrix(users_who_liked_each_item[item_i, (1:items_liked_cardinality[item_i])])
  }
  
  if(items_disliked_cardinality[item_i] > 0)
  {
    users_who_rated_target_item_disliked = as.matrix(users_who_disliked_each_item[item_i, (1:items_disliked_cardinality[item_i])])
  }
  
  if((items_liked_cardinality[item_i] > 0) && (items_disliked_cardinality[item_i] > 0))
  {
    users_who_rated_target_item = rbind(users_who_rated_target_item_liked, users_who_rated_target_item_disliked)
    
    
  }
  
  else if((items_liked_cardinality[item_i] == 0) && (items_disliked_cardinality[item_i] > 0))
  {
    users_who_rated_target_item = users_who_rated_target_item_disliked
  }
  
  else if((items_liked_cardinality[item_i] > 0) && (items_disliked_cardinality[item_i] == 0))
  {
    users_who_rated_target_item = users_who_rated_target_item_liked
  }
  
  
  remove(users_who_rated_target_item_liked)
  remove(users_who_rated_target_item_disliked)
  
  neighbors_of_active_user = as.matrix(which(user_similarity[user_i,] > 0))
  
  neighbors_who_rated_target_item = as.matrix(intersect(users_who_rated_target_item,neighbors_of_active_user))
  
  if(as.numeric(nrow(neighbors_who_rated_target_item)) > 0)
  {
    neighbors_ratings_on_target_item = as.matrix(ratings_UI_matrix[neighbors_who_rated_target_item,item_i])
    
    neighbors_mean_values = as.matrix(users_mean[neighbors_who_rated_target_item])
    
  }
  
  else
  {
    neighbors_who_rated_target_item = users_who_rated_target_item
    
    neighbors_ratings_on_target_item = as.matrix(ratings_UI_matrix[neighbors_who_rated_target_item,item_i])
    
    neighbors_mean_values = as.matrix(users_mean[neighbors_who_rated_target_item])
    
  }
  
  
  tendency_item = (sum(neighbors_ratings_on_target_item) - sum(neighbors_mean_values))/(as.numeric(nrow(neighbors_mean_values)))
  
  if((tendency_item > 0) && (tendency_user > 0))
  {
    predicted_rating = max((tendency_item + users_mean[user_i]), (tendency_user + items_mean[item_i]))
  }
  
  else if((tendency_item < 0) && (tendency_user < 0))
  {
    predicted_rating = min((tendency_item + users_mean[user_i]), (tendency_user + items_mean[item_i]))
  }
  
  else if((tendency_item > 0) && (tendency_user < 0))
  {
    predicted_rating = min(max(users_mean[user_i],((tendency_user + items_mean[item_i] * Beta)+(tendency_item + users_mean[user_i] * (1-Beta)))),items_mean[item_i])
  }
  
  else
  {
    predicted_rating = Beta * users_mean[user_i] + (1 - Beta)* items_mean[item_i]
  }
  
  if(predicted_rating > 5) 
  {
    predicted_rating = 5
  }
  
  if(predicted_rating < 1)
  {
    predicted_rating = 1
  }
  
  mae = mae + abs(expected_rating - predicted_rating)
  
  results[i,1] = expected_rating
  results[i,2] = predicted_rating
}

mae_phase3 = mae/nrow(train_set_part5)

write.csv(results, 'Results_phase4.csv')
################################################################################
print('total time - phase 4')
print(total_time)