#find total of tweets by sentiment
negative <- tweet_sentiment[tweet_sentiment$ave_sentiment < 0]
positive<- tweet_sentiment[tweet_sentiment$ave_sentiment > 0]
neutral <- tweet_sentiment[tweet_sentiment$ave_sentiment == 0]
n <- nrow(tweet_sentiment)
#joint probability
neg_35 <- nrow(negative[negative$word_count > 35])/n
pos_35 <- nrow(positive[positive$word_count > 35])/n
neu_35 <- nrow(neutral[neutral$word_count > 35])/n
#bayestheorem
p_neglong <- scales::percent(neg_35/(neg_35+pos_35+neu_35)) #scales::percent converts number to percent
p_poslong <- scales::percent(pos_35/(neg_35+pos_35+neu_35))
p_neulong <- scales::percent(neu_35/(neg_35+pos_35+neu_35))
prob<- data.frame(p_neglong,p_poslong,p_neulong)
#renaming columns
colnames(prob)<- c('P(Neg|Long)','P(Pos|Long)','P(Neg|Long)')
prob
