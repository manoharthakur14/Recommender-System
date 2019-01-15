# Recommender-System
Recommender System built for Movie-lens 1m dataset.
To build a recommendation system this project attempts 2 majors methods.
First is the content-based recommendation system. Here we made use of user preferences to generate a user profile. The 
Systems achieves this by considering the item profiles (vector describing an item) and their corresponding user rating. Once
the profile was generated, we calculated the similarity of the user profile with all the items in the dataset using a method ‘cosine’ 
to find the similarity between the user profile and item profile.
Second is the Collaborative filtering technique. There are two ways this technique was implemented. First it is user-based (UBCF)which 
consists of making a recommendation for a user based on ratings and 
preferences many users focusing on both neighbourhoods of k most similar users(k-nn) and similarity threshold. This means that if 2 users, user AandBlikeamovieXanduserBalsolikesamovie Y 
then the latter will be recommended to user A. This is a very useful technique used by various internet platforms like Amazon.com, Netflix, Facebook. The Item Based (IBCF) was used to recommend movies the ratings of users. we computed the similarity between users by using the row vector in the ratings matrix corresponding to a user as a representation for that user. The similarity is computed by using cosine correlation. To predict the rating for a user for a given movie, we find the top k similar users to 
this user and then take a weighted average of the ratings of the k similar users with the weights being the similarity values.The code is encapsulated with functions.The performance of the recommender systems was checked against the the recommender lab function.
