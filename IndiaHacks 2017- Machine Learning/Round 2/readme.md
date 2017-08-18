#IndiaHacks 2017- Machine Learning : Round 2

The problem statement for the second round was to predict the number of lanes present in the roads. This is the step after the lane lines are detected on the roads using Lidar data. 

My apporach was to build as many new features as possible but only found two of 5 features to be significant. None of the boosting algorithms helped me, surprisingly random forest came for the rescue. 

Feature engineering always gives a jump in the scores whereas this competition was purely on hyper-paramter tuning which help me to get a decent score. 

I built three random forest models and ensembled them which gave an F1 score of 0.82324 on the public leaderboard.

At the end of the 21hrs hackathon I finished 3rd on the public and 6th(F1-score : 0.79542) on the private leader board. 

You can find the code in this directory.
