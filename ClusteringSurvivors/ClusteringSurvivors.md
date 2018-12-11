Clustering Survivor Contestants - Performance, Strategy, and Demographics
================
Caitlin Pinkard

CBS's hit reality show, Survivor, is deemed one of the "greatest social experiments of all time." Each season, 16 to 20 people are stranded in a desolate location with only rice, the clothes on their backs, and each other. Pitting both brain and brawn against twist and turn, survivors compete in a 5 to 6 week-long power struggle to "outwit, outplay, and outlast" their competitors in some of the most-unforgiving climates in the world. Every episode (roughly 2 to 3 days in "Survivor time") features an immunity challenge and a tribal counsel. The goal is simple: win the immunity challenge, and you're safe at tribal counsel - lose, and you could be going home. Because survivors vote on who to send packing at the end of each tribal counsel, alliances, strategy, and backstabs rule the game. But, it's not always as simple as win challenges, win the $1M prize. Those who win a lot early on are seen as a threat and often voted out in the beginning stages of the game. However, lose many challenges, and you may be seen as easy prey.

Aside from immunity challenges, players may earn safety by finding hidden immunity idols the production crew stows away at each camp. A hidden immunity idol can save its owner from being voted out at ONE tribal counsel. However, the contestant must choose to play the idol AFTER the votes have been cast for its power to count, and after it has been used, its power is gone. Players can choose to keep the idol a secret, or they can broadcast this immunity to anyone who will listen. The nuanced nature of this piece of the Survivor puzzle leaves room for many different strategies and game-playing styles.

In this analysis, I look at how players from seasons 11 to 20 chose to approach the game. I have included variables such as number of times the contestant has played Survivor (there are many players who get invited back), their demographics, and different ways of measuring their game play (number of votes against them, number of idols found and played, challenge wins, etc.). I will perform both k-means clustering and hierarchical clustering to see if different strategies emerge ("challenge beasts," "coat-tail riders","social players"). I'll also use the results to determine if certain clusters seem more likely to win and claim the $1M prize.

Note: I chose Seasons 11 through 20 because the Hidden Immunity Idol was not introduced until the 11th season and more recent seasons of Survivor have introduced more twists that would make player-to-player comparisons harder and less accurate. This data was compiled from the following sources: a Google doc containing immunity idol use (<https://docs.google.com/spreadsheets/d/1jTtpv3pdivUWo3oF3nGBWcDnG69cTw63QHfXgsfXgTI/edit#gid=0>) and the Survivor Wiki Page (<https://survivor.fandom.com/wiki/List_of_Survivor_contestants>). Many analyses have been performed on Survivor contestants, but I hope to contribute new insights to the space of reality show strategy investigation.

``` r
setwd("~/Desktop/Data Mining/Final")

data <- read.csv("SurvivorFinal.csv")
data <- data[,c(1:15)]

data$Hometown <- as.character(data$Hometown)
data$Hometown <- as.factor(substr(data$Hometown, nchar(data$Hometown)-1, nchar(data$Hometown)))

#NY and CA were the two most frequent home states
data$CA <- ifelse(data$Hometown=="CA",1,0)
data$NY <- ifelse(data$Hometown=="NY",1,0)

data$Season <- NULL
data$Hometown <- NULL
data$Male <- ifelse(data$Sex=="M",1,0)
data$Sex <- NULL
data <- data[complete.cases(data),]

data$ChallengeWinAjd <- data$ChallengeWins/data$DaysLasted
data$ChallengeWins <- NULL
```

I have compiled the following information for each contestant that competed during these seasons: name, number of times played, do they have a social media presence, age at time of filming, rank (where did they finish), number of challenge wins, total number of votes against, number of days lasted (usually out of 39), number of idols found, number of idols played, number of votes voided by idols, number of times they avoided elimination by correctly playing an idol, their hometown, and their gender. Note that if a player returned for another season, they will a row for each appearance (so each row represents a "player-season").

I found that about half of the contestants in the dataset were either from California or New York. Because I couldn't have categorical data in clustering analysis, I generated two "one-hot encoded" variables to represent hometown: CA and NY - each a binary variable indicating whether the contestant was from either of the two most common states. I also changed the Sex variable to "Male" with a "1" if the contestant is male, and "0" if the contestant is female. Finally, I adjusted challenge wins by the number of days a contestant lasted to eliminate player longevity's influence on the number of wins that they earned.

I am left with 181 rows, or player-seasons.

``` r
set.seed(1234)
fit <- kmeans(data[,2:15], 3)
#fit$center 

SSEs <- rep(NA,10) 
SSEs[1] <- fit$totss 
for(k in 2:10){
    fit <- kmeans(data[,2:15],k)
    SSEs[k] <- fit$tot.withinss
}
par(mar=c(4,4,1,1))

plot(1:10,SSEs,type="b",xlab="Number of Clusters")
```

![](ClusteringSurvivors_files/figure-markdown_github/k-means%20clustering-1.png)

``` r
fit <- kmeans(data[,2:15], 3)
fit$center 
```

    ##   X.TimesPlayed TwitterHandle SeasonAge      Rank VotesAgainst DaysLasted
    ## 1      1.244186     0.5348837  31.06977  4.860465     5.534884   35.06977
    ## 2      1.173913     0.3913043  28.46377 13.666667     6.188406   14.66667
    ## 3      1.230769     0.2307692  48.23077 14.961538     5.500000   10.80769
    ##   IdolsFound IdolsPlayed VotesVoided BootAvoided        CA         NY
    ## 1 0.25581395  0.16279070   0.3255814  0.04651163 0.3604651 0.04651163
    ## 2 0.04347826  0.00000000   0.0000000  0.00000000 0.3333333 0.13043478
    ## 3 0.03846154  0.03846154   0.1153846  0.00000000 0.2692308 0.03846154
    ##        Male ChallengeWinAjd
    ## 1 0.5348837       0.2395728
    ## 2 0.4347826       0.2377177
    ## 3 0.5384615       0.2789932

### K-Means Clustering Results

According to the scree plot, which shows essentially the tightness of each cluster versus the number of clusters chosen, 3 clusters suit this data well.

Cluster 1 ("The Youngbloods", 69 Survivors): These contestants are predominently younger, first-time Survivor players who make it to about the halfway point in the game, on average. They don't tend to find or play idols, perhaps because they don't have enough experience playing Survivor to know where to look for idols or how to play them properly. These newbies also have a middle-of-the-road social media presence, probably because they're both young and acquainted (read: addicted) to technology and also not famous enough Survivor players to have an entourage of fans hoping to follow their Twitter accounts.

Cluster 2 ("The Sole Survivors", 86 Survivors): These are the real Survivors, those that make it to the end (or close to the end) by finding and playing idols, winning enough challenges without necessarily getting labeled the "Challenge Beasts," and playing what seems to be a strong social game (e.g. they have a fairly low average number of votes cast against them). They also have the biggest Twitter presence, which is likely a result of their success in the game and resulting fanbase.

Cluster 3 ("The Villians", 26 Survivors): These contestants have, on average, more experience playing Survivor than the newbies. Perhaps as a result, they also have more adjusted challenge wins. However, they do also tend to go home earliest of all the groups. Maybe tribe-mates see them as a threat and aim to get them out sooner than later. Finally, they are older on average and have the weakest social media presence.

``` r
dataScaled <- scale(data[,2:15])

set.seed(1234)
d <- dist(dataScaled, method = "euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit) 

data$clusterID <- cutree(fit, k=3) 

rect.hclust(fit, k=3, border="red") 
```

![](ClusteringSurvivors_files/figure-markdown_github/hierarchical%20clustering-1.png)

``` r
ClusTable <- as.data.frame(matrix(NA,nrow=14,ncol=6))
row.names(ClusTable) <- names(data)[2:15]
colnames(ClusTable) <- c("C1.Mean","C1.sd","C2.Mean","C2.sd","C3.Mean","C3.sd")


for(i in 1:3){
    cluster.data <- data[data$clusterID==i,2:15]
    ClusTable[,2*i-1] <- round(colMeans(cluster.data),2)
    ClusTable[,2*i]   <- round(apply(cluster.data,2,sd),2)
}

ClusTable
```

    ##                 C1.Mean C1.sd C2.Mean C2.sd C3.Mean C3.sd
    ## X.TimesPlayed      1.04  0.19    1.29  0.58    1.50  0.58
    ## TwitterHandle      0.25  0.44    0.51  0.50    0.75  0.50
    ## SeasonAge         35.54 11.32   31.00  7.94   37.25 12.74
    ## Rank              14.96  2.70    7.45  4.60    2.75  0.96
    ## VotesAgainst       6.32  2.64    5.50  2.91    6.75  2.63
    ## DaysLasted        11.05  6.03   29.21  9.59   38.75  0.50
    ## IdolsFound         0.00  0.00    0.16  0.37    1.75  0.96
    ## IdolsPlayed        0.00  0.00    0.07  0.29    1.50  0.58
    ## VotesVoided        0.00  0.00    0.10  0.58    4.75  1.50
    ## BootAvoided        0.00  0.00    0.00  0.00    1.00  0.00
    ## CA                 0.48  0.50    0.27  0.45    0.25  0.50
    ## NY                 0.00  0.00    0.12  0.32    0.00  0.00
    ## Male               0.32  0.47    0.57  0.50    0.75  0.50
    ## ChallengeWinAjd    0.23  0.18    0.25  0.10    0.21  0.06

### Hierarchical Clustering Results

Hierarchical analysis on this same dataset yields some unexpected results. From the dendrogram, it seems like selecting 3 clusters may, again, be optimal. However, no matter how many clusters you select, there will always be 4 Survivors that stick out. These 4 warrant closer investigation.

``` r
plot(jitter(data$DaysLasted,2), jitter(data$VotesAgainst,2), col=c("red","blue","green")[data$clusterID], pch = 19, xlab = "Days Lasted",ylab = "Votes Against")
```

![](ClusteringSurvivors_files/figure-markdown_github/4%20Outliers%20investigation-1.png)

``` r
data[which(data$clusterID==3),]
```

    ##              Name X.TimesPlayed TwitterHandle SeasonAge Rank VotesAgainst
    ## 7   Amanda Kimmel             2             1        23    2            4
    ## 147 Russell Hantz             1             1        36    2            9
    ## 148 Russell Hantz             2             1        36    3            5
    ## 180  Yau-Man Chan             1             0        54    4            9
    ##     DaysLasted IdolsFound IdolsPlayed VotesVoided BootAvoided CA NY Male
    ## 7           39          1           1           4           1  0  0    0
    ## 147         39          3           2           7           1  0  0    1
    ## 148         39          2           2           4           1  0  0    1
    ## 180         38          1           1           4           1  1  0    1
    ##     ChallengeWinAjd clusterID
    ## 7         0.2051282         3
    ## 147       0.1282051         3
    ## 148       0.2564103         3
    ## 180       0.2368421         3

These 4 player-seasons feature 3 distinct Survivors, Amanda Kimmel (whose second Survivor season is captured in this cluster), Russell Hantz (whose first and second appearance on the show is captured in this cluster), and Yau-Man Chan. All four of these player-seasons made it to the last (or second-to-last) day of their respective seasons, so clearly they know how to make it far in the game, but don't seem to pull through during that final tribal counsel, where the winner is decided.

As shown by the scatterplot above, these players survive far into the game with more votes cast against them than many of their finalist peers. Therefore, what I believe sets them apart is their ability to find idols and play them appropriately. Each player-season featured has at least 4 votes voided by a hidden immunity idol. This indicates not only that these players found many idols, but also had Survivor prowess to know when to use them effectively. It is important to note that an idol used correctly can save a player's game, but an idol wasted simply shows your tribemates that you're willing to keep secrets from them and that you see yourself in a position of vulnerability when it comes time to vote.

### Takeaways

Based on this analysis, use of hidden immunity idols can affect a player's game drastically. As in the case of Russell Hantz, Amanda Kimmel, and Yau-Man Chan, using an idol when your name is indeed on the chopping block can clearly save your game. However, these decisive moves can come back to haunt you when votes are cast to award the $1M. It may be best to establish a strong alliance near the beginning of the season and play a quiet, social game to ensure those final jury votes.
