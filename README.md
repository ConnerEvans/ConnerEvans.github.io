# ConnerEvans.github.io

I created this website to host some of the projects that I have created. Here is a description of each one:

Expected Points Per Drive - When creating a model to predict which team will win NFL football games, the final scores should be normalized for the amount of opportunities teams had to score in each game. I created a script in R that properly assigns the EPA (https://www.nfeloapp.com/analysis/expected-points-added-epa-nfl/) from nflfastR to each of the 8 units that make up a football team: Offense, Defense, Kickoff Kicking, Kickoff, Receiving, Punt Kicking, Punt Receiving, Field Goal/Extra Point Kicking, Field Goal/Extra Point Defending. I made sure that the expected points assigned to each unit of each team equaled the actual score differential for each half. This included reporting multiple bugs to nflfastR that have been fixed. I host a CSV file of this data for every game back to 2016 which can be used to train a model to predict future points per drive.

Maximizing Expected Growth - I derived an equation that maximizes the expected growth of seed money given betting lines and the actual probabilities of the events. I explain the theory behind the formula in an article using visualizations and provide an example algorithm that implements the formula.

Clustering - I used Bayesian reasoning to create an algorithm that inputs user data about pairs of items and then outputs a clustering for the items. The data is whether the user thinks random pairs of items are Similar, Not Similar, or Completely Different. The data entry is facilitated through the website and then processed in a Google Colab notebook that also explains the algorithm and all of the theory behind it. In the future I will add the ability to concurrently create an overall ranking of the items while getting data for clustering. Instead of the pairs of items being random, they will be chosen to create the overall ranking in as few comparisons as possible.

Dynasty Football Trade Calculator - I created a trade value caluclator for dynasty fantasy football using actual trade data from Sleeper. I include a page that explains the theory behind the calculations.

Fantasy Football Target Variables - The value of a fantasy football player comes from their ability to win you the championship. This value comes from the amount of yards, touchdowns, etc. that they gain each week and the number of games they play and the number of games they were started. I transform all of these data points into their uncorrelated aspects that can each be used as target variables when building models to predict future fantasy value. I also supply the transformation that takes the predicted amount of production and translates it into fantasy points, then likelihood of winning a game, then likelihood of winning the championship based on scoring settings, roster requirements, number of teams, and playoff structure. 

Conquer Wordle - I explain how I optimized Alpha-beta pruning to find the optimal words for two different metric of Wordle play: minimizing the average number of guesses and maximizing the percent of words that take three guesses. I then link to a Google Colab sheet with these algorithms.

Future Projects:
- Use a Markov decision process to build a new version of EPA from models that predict state transitions.
- Predict winners of NFL games using a Bayesian Network that takes in past outcomes and metrics derived from nflfastR data and potentially PFF grades and other advanced metrics.
- Build models to predict distributions for 'Fantasy Football Target Variables' for redraft and dynasty. 