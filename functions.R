#install.packages('biclust')
library(biclust)

###build recommender function
recommender=function(method,prof_matr,genre_matr){
  if (method=='UBCF'){
    sim=t(prof_matr)%*%prof_matr
    diag_elem=diag(sim)
    norm_elem=sqrt(diag_elem)%*%t(sqrt(diag_elem))
    cos_sim=sim/norm_elem
    return_list=list(sim_matr=sim, cos_sim_matr=cos_sim)
    return(return_list)####
  }
  else if(method=='IBCF'){
    item_sim=prof_matr%*%t(prof_matr)
    diag_elem=diag(item_sim)
    norm_elem=sqrt(diag_elem)%*%t(sqrt(diag_elem))
    item_cos_sim=item_sim/norm_elem
    rownames(item_cos_sim)=c(1:nrow(prof_matr))
    colnames(item_cos_sim)=c(1:nrow(prof_matr))
    item_cos_sim[is.nan(item_cos_sim)]=0
    return_list=list(sim_matr=item_sim, cos_sim_matr=item_cos_sim)
    return(return_list)####
  }
  else if(method=='CB'){
    #normalize genre matrix
    genre_matrix_norm=matrix(0,nrow(prof_matr),ncol(genre_matr))
    sum_row=NULL
    for (i in 1:nrow(genre_matr)){
      sum_row[i]=0
      sum_row[i]=sum(genre_matr[i,])
    }
    genre_matrix_norm=genre_matr/sqrt(sum_row)
    genre_matrix_norm[is.na(genre_matrix_norm)]=0
    #create prof vec matr
    #return those two
    return_list=list(genre_matr_norm=genre_matrix_norm)
    return(return_list)
  }
}
###build function to calculate cos similarity of new user with old users...
##i will include the code of this function in the prediction function
##because i dont have to run it for every kind of recommendation
new_cos_sim=function(user,prof_matr,recommender){
  sim=user%*%prof_matr
  norm_elem=sqrt(user%*%t(user))%*%sqrt(t(diag(recommender$sim_matr)))
  n_cos_sim=sim/norm_elem
  return(n_cos_sim)
}
genre_matrix_nor=matrix(0,nrow(profile_matrix_2),ncol(genre_matrix_upd))
####checking new_cos_sim function
ncs=new_cos_sim(us_1,profile_matrix_2,recommender1)
tncs=t(ncs)
rownames(tncs)=as.integer(1:6040)
ksu=rownames(tncs)
x=order(tncs[,1],decreasing = TRUE)
y=order(cos_sim[,1],decreasing = TRUE)[1:10]
ksu=(tncs[order(tncs[,1],decreasing = TRUE)])
x=rownames(ksu)
###build prediction function
pred_function=function(method,user_matr,prof_matr,recommender,genre_matr,movies_upd,nbh_type,k_nn,user_indx,threshold,
                       top_n){
  if (method=='UBCF'){
    sim_matr=recommender$sim_matr
    sim=user_matr%*%prof_matr
    norm_elem=sqrt(user_matr%*%t(user_matr))%*%sqrt(t(diag(sim_matr)))
    act_user_cos_sim=sim/norm_elem
    
    user_prediction=matrix(0,nrow(prof_matr),2)
    user_prediction[,1]=1:nrow(prof_matr)
    
    if (nbh_type=='k_nn'){
      k_sim_users=order(t(act_user_cos_sim)[,user_indx],decreasing = TRUE)[1:k_nn]
      user_prediction[,2]=(prof_matr[,k_sim_users]%*%t(act_user_cos_sim)[k_sim_users,user_indx])/sum(t(act_user_cos_sim)[k_sim_users,user_indx])
      
      return_list=list(ubcf_preds=user_prediction, top_n_pred=user_prediction[order(user_prediction[,2],decreasing = TRUE),][1:top_n,1])
      
      return(return_list)
    }
    else if (nbh_type=='threshold'){
      bin_sim_matr=binarize(act_user_cos_sim, threshold=threshold)
      act_user_cos_sim_b=bin_sim_matr*act_user_cos_sim
#      act_user_cos_sim_b=act_user_cos_sim_b - diag(ncol(prof_matr))
      
      user_prediction[,2]=(prof_matr%*%t(act_user_cos_sim_b)[,user_indx])/sum(t(act_user_cos_sim_b)[,user_indx])
      
      return_list=list(ubcf_preds=user_prediction, top_n_pred=user_prediction[order(user_prediction[,2],decreasing = TRUE),][1:top_n,1])
      return(return_list)
    }
  }
  else if (method=='IBCF'){
    top_sim_items=binarize(recommender$cos_sim_matr, threshold = threshold)
    top_sim_items=top_sim_items*recommender$cos_sim_matr
    top_sim_items=top_sim_items - diag(nrow(prof_matr))
    
    ibcf_prediction=matrix(0,nrow(prof_matr),2)
    ibcf_prediction[,1]=1:nrow(prof_matr)
    
    ibcf_prediction[,2]=(top_sim_items%*%t(user_matr)[,user_indx])[1:3952]/rowSums(top_sim_items)
    
    return_list=list(ibcf_preds=ibcf_prediction, top_n_pred=ibcf_prediction[order(ibcf_prediction[,2],decreasing = TRUE),][1:top_n,1])
    return(return_list)
  }
  else if (method=='CB'){
    genre_matrix_norm=as.matrix(recommender$genre_matr_norm)
    prof_vec_matrix=matrix(0,nrow(user_matr),ncol(genre_matr))
    prof_vec_matrix=user_matr%*%genre_matrix_norm
    norm_rows=NULL
    for (i in 1:nrow(prof_vec_matrix)){
      norm_rows[i]=prof_vec_matrix[i,]%*%prof_vec_matrix[i,]
    }
    prof_vec_matrix_norm=prof_vec_matrix/sqrt(norm_rows)
    #get preds via dot product of norm genre matr and prof vec matr
    cb_prediction=prof_vec_matrix_norm%*%t(genre_matrix_norm)
    #return preds
    colnames(cb_prediction)=1:nrow(genre_matrix_norm)
    rownames(cb_prediction)=1:nrow(prof_vec_matrix_norm)
    recom=data.table(recom_score=cb_prediction[user_indx,],movieID=1:nrow(prof_matr))
    recom=recom[order(recom_score, decreasing = TRUE),]
    top_n_movies=recom[1:top_n,.(recom_score,movieID)]
    movies_names=movies_upd[MovieID%in%top_n_movies[,movieID],.(Title)]
    return_list=list(cb_preds=cb_prediction[user_indx,],top_n_recs=top_n_movies,movies_titles=movies_names)
    return(return_list)
  }
}
#checking
dim(t(diag(recommender2$sim_matr)))
recommender3=recommender('IBCF',profile_matrix_2,genre_matrix_upd)
us_1=t(profile_matrix_2)[1,]
us_1=t(us_1)
predictions=pred_function('CB',us_1,profile_matrix_2,recommender1,genre_matrix_upd,movies_upd,'threshold',30,1,0.5,10)
predictions$top_n_pred
predictions$movies_titles
predictions$top_n_pred
predictions$top_n_preds