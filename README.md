
This project models offensive points from FBS college football games this season and predicts the W-L outcomes for the remaining bowl games and potential national championship games. 

The model is simple. It models the (square root of the) points a team scores as a function random forest predictions of the points (with lots of rankings systems of the offensive and defensive teams as inputs) and random intercepts for the offensive and defensive team.

The data game outcome data come from [here](https://www.masseyratings.com/scores.php?s=295489&sub=11604&all=1) and the rankings data come from [here](https://www.masseyratings.com/cf/compare.csv).