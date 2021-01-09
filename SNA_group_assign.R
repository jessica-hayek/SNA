install.packages("netrankr")
install.packages("qgraph")

library(igraph)
library(netrankr)
library(dplyr)
library(tidyverse)
require(igraph)
library(ggplot2)
library(qgraph)

#loading dataset
prostitution_df <- read.csv2("/Users/Apple/Downloads/Prostution\ Network\ Dataset.csv")

#referencing female and male nodes by adding "F" before female node ID number and "M" before male node ID number
prostitution_df$female_id <- paste0("F", prostitution_df$female_id)
prostitution_df$male_id <- paste0("M", prostitution_df$male_id)

# loading dataframe into graph with attributes
prostitution_network <- graph.data.frame(prostitution_df, directed=F)

#exploring graph, vertices and edges
prostitution_network
V(prostitution_network)
E(prostitution_network)

# count the multiples and plot their frequency
repeated_partners <- count.multiple(prostitution_network)
ggplot() + aes(as.factor(repeated_partners[repeated_partners<21])) + geom_bar(colour="black", fill="white") + theme_bw()  #change design a bit

# how many total sexual encounters do females have
degrees_females = degree(prostitution_network, v= V(prostitution_network)[startsWith(V(prostitution_network)$name, "F")])
ggplot() + aes(as.factor(degrees_females[degrees_females<11]))+ geom_bar(colour="black", fill="white") + theme_bw() #change design a bit

# summary stats of female number of partners
summary(degrees_females)

# top 10 prostitutes with biggest number of enounters over the 6 years
top_10_females <- names(sort(degrees_females, decreasing = T)[1:10])
top_10_females

# how many total sexual encounter do males have
degrees_males <- degree(prostitution_network, v= V(prostitution_network)[startsWith(V(prostitution_network)$name, "M")])
ggplot() + aes(as.factor(degrees_males[degrees_males<21]))+ geom_bar(colour="black", fill="white") + theme_bw()

# summary of male partners
summary(degrees_males)

# from the summary, males dont have too many encounters, mean is 5 over the 5 years - or 1 per year

# top 10 most frequent male sex buyers -- we can get their IDs and check their attributes
top_10_males <- names(sort(degrees_males, decreasing = T)[1:10])

# subgraph network for top 10 guys - 673 nodes and 1163 edges
top_10_male_graph <- induced_subgraph(prostitution_network,unlist(ego(prostitution_network, nodes=top_10_males)))
V(top_10_male_graph)
E(top_10_male_graph)


# get edge attributes for top 10 guys - see what are the reasons that keep them on coming back
attr_list_males <- as.data.frame(edge_attr(top_10_male_graph))
attr_list_males$date <- NULL

# plot multiples for top 10 most frequent male buyers - they come back more for the same women than the average buyer?
count.multiple(top_10_male_graph)
ggplot() + aes(as.factor(count.multiple(top_10_male_graph))) + geom_bar(colour="black", fill="white") + theme_bw()

# avg grade given by top 10 males
# comparing it to the avg of all the sexual encounters, there seems to be no difference - theyre not having better sex
mean(attr_list_males$female_grade)
mean(prostitution_df$female_grade)

# compare anal sex, oral sex and mouth kissing instances for frequent buyers - what do the frequent buyers like perform the most?
(length(which(attr_list_males$anal_sex == 1))/length(attr_list_males$anal_sex))*100     # percentage of anal sex - 53%
(length(which(attr_list_males$oral_sex == 1))/length(attr_list_males$oral_sex))*100    # percentage of oral sex - 84%
(length(which(attr_list_males$mouth_kiss == 1))/length(attr_list_males$mouth_kiss))*100   # percentage of mouth kiss - 77%

# compare with the rest of males from all of the network - frequent buyers have a preference for oral sex
(length(which(prostitution_df$anal_sex == 1))/length(prostitution_df$anal_sex))*100     # 53%
(length(which(prostitution_df$oral_sex == 1))/length(prostitution_df$oral_sex))*100    # 79%
(length(which(prostitution_df$mouth_kiss == 1))/length(prostitution_df$mouth_kiss))*100  # 75%


# subgraph into each year to see how prostitutes popularity evolves over time - do the most popular ones get even more popular?
year1 <- graph.data.frame(prostitution_df[prostitution_df$date >= 0 & prostitution_df$date <= 365,], directed=F)
year2 <- graph.data.frame(prostitution_df[prostitution_df$date >= 366 & prostitution_df$date <= 730,], directed=F)
year3 <- graph.data.frame(prostitution_df[prostitution_df$date >= 731 & prostitution_df$date <= 1095,], directed=F)
year4 <- graph.data.frame(prostitution_df[prostitution_df$date >= 1096 & prostitution_df$date <= 1460,], directed=F)
year5 <- graph.data.frame(prostitution_df[prostitution_df$date >= 1461 & prostitution_df$date <= 1825,], directed=F)
year6 <- graph.data.frame(prostitution_df[prostitution_df$date >= 1826 & prostitution_df$date <= max(prostitution_df$date),], directed=F)

# get top 5 women by number of neighbors
#simplify all graphs to get the number of neighbors - I did this shortcut because I was having problems with neighbors and its functions
top_5_y1 <- sort(degree(simplify(year1), v= V(simplify(year1))[startsWith(V(simplify(year1))$name, "F")]), decreasing = T)[1:5]
top_5_y2 <- sort(degree(simplify(year2), v= V(simplify(year2))[startsWith(V(simplify(year2))$name, "F")]), decreasing = T)[1:5]
top_5_y3 <- sort(degree(simplify(year3), v= V(simplify(year3))[startsWith(V(simplify(year3))$name, "F")]), decreasing = T)[1:5]
top_5_y4 <- sort(degree(simplify(year4), v= V(simplify(year4))[startsWith(V(simplify(year4))$name, "F")]), decreasing = T)[1:5]
top_5_y5 <- sort(degree(simplify(year5), v= V(simplify(year5))[startsWith(V(simplify(year5))$name, "F")]), decreasing = T)[1:5]
top_5_y6 <- sort(degree(simplify(year6), v= V(simplify(year6))[startsWith(V(simplify(year6))$name, "F")]), decreasing = T)[1:5]


# compare if these women are the same over the years
colSums(vapply(names(top_5_y1), FUN = function(x) x == names(top_5_y2), FUN.VALUE = logical(length(names(top_5_y1)))))
colSums(vapply(names(top_5_y2), FUN = function(x) x == names(top_5_y3), FUN.VALUE = logical(length(names(top_5_y2)))))
colSums(vapply(names(top_5_y3), FUN = function(x) x == names(top_5_y4), FUN.VALUE = logical(length(names(top_5_y3)))))
colSums(vapply(names(top_5_y4), FUN = function(x) x == names(top_5_y5), FUN.VALUE = logical(length(names(top_5_y4)))))
colSums(vapply(names(top_5_y5), FUN = function(x) x == names(top_5_y6), FUN.VALUE = logical(length(names(top_5_y5)))))

# not the same women throughout the years - the top women dont get more clients over time

#aggregate the dataframe by female_id to get the average grade given to a female
prostitution_df_aggregate <- aggregate(list(avg_grade = prostitution_df$female_grade), by = list(female_id = prostitution_df$female_id), FUN = mean)

# add the grade as a node attribute for the females
V(prostitution_network)[startsWith(V(prostitution_network)$name, "F")]$avg_grade <- prostitution_df_aggregate$avg_grade

# correlation between grade and number of sexual encounters - there is a weak positive correlation with a correlation coefficient of 0.12
cor.test(as.vector(V(prostitution_network)[startsWith(V(prostitution_network)$name, "F")]$avg_grade), degrees_females)

# running cluster louvain to find communities - possible brothels
communities <- cluster_louvain(year6)
membership(communities)
modularity(communities)
plot(communities, year6)

# small world index - not sure what it's supposed to mean
smallworldness(prostitution_network, B = 10, up = 0.995, lo = 0.005)
# smallworldness is less than 3, which means it is not a small world?



  