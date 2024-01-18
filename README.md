
## NHL RAPM Development


This repo is has my code concerning the development of a RAPM metric for NHL skaters at even strength. Also included here is the code to train my expected goals (xG) model.  

The code is not a finished product. Before I can train the RAPM models for each season (and I hope to do 3 and 5 year models once I figure out how to make it less computationally expensive) I need 
to retrain the xG model with 2024 data. The catch is that there were some changes to the NHL API this season, so some of the data types are different in 2024 compared to all seasons in the past. 
As a result I need make some alterations to the code to facilitate these changes.  

At some point I would like to create a metric that uses RAPM as a base and incorporates play-by-play derived stats to create a more stable player metric. RAPM even over the course of a season is still subject to some issues
with collinearity. Incorporating some more basic metrics that stabilize more quickly should assist in creating a better overall stat. I would imagine, when the time comes, this model that incorporates RAPM plus some other 
undetermined information will be trained with 5-year RAPM as the target, which is probably the best representation of player isolated player skill (though still dependent on role) that we can possibly have in the public 
sphere. Obviously the catch is that it takes five years to get the requisite data to value a player, so it is not the most practical player measure. But as a tool for training models on the scale of a single year, 
it is extremely useful. 
