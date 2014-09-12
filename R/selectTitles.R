# selectTitles.R
# Organization and random selection of titles from literature review

library(lmisc)
p_root <- full_path(proj_root(proj.dir = "lit_review"), "lit_review")
p_data <- full_path(p_root, "data")

setwd(p_data)

# Read in downloaded citations and merge
batch1 <- read.csv("top30_from1982_set1.csv")  
batch2 <- read.csv("top30_from1982_set2.csv")
articles <- rbind(batch1, batch2) 

# Full list
set.seed(234)
draw <- sample(1:nrow(articles), size = nrow(articles), replace = FALSE)
article.shuffle <- articles[draw, ]

# 1982 set (superseded now)
# # draw subsets for each author
# drawsize <- (round(nrow(articles) * 0.015 / 5) * 5) / 5  # number divisible by 5
# set.seed(432)
# sset <- sample(1:nrow(article.shuffle), size = drawsize * 5, replace = FALSE)
# inds <- seq(1, length(sset) + 1, by = length(sset) / 5)
# ind.mat <- cbind(inds[-length(inds)], c(inds[-c(1, length(inds))], length(sset)))
# 
# fnames <- paste(c("Chang", "Treuer", "Socolar", "Daskin", "Estes"), "draw.csv", sep = "_")
# for(i in 1:nrow(ind.mat)) write.csv(article.shuffle[sset[ind.mat[i, 1]:ind.mat[i, 2]], ], file = fnames[i])

# 10-year list\set.seed(234)
articles.2004 <- articles[articles$Year >=2004, ]

set.seed(234)
draw <- sample(1:nrow(articles.2004), size = nrow(articles.2004), replace = FALSE)
article.2004.shuffle <- articles.2004[draw, ]

# draw subsets for each reviewer - note: as long as drawsize and random seed are the same, new reviewers can 
# be appended to the group without affecting the draw for existing reviewers
reviewers <- c("Chang", "Treuer", "Socolar", "Daskin", "Elsen", "Estes")
nauth <- length(reviewers)
#drawsize <- (round(nrow(articles.2004) * 0.025 / nauth) * nauth) / nauth  # number divisible by 
drawsize <- 400
set.seed(432)
sset <- sample(1:nrow(article.2004.shuffle), size = drawsize * nauth, replace = FALSE)
inds <- seq(1, length(sset) + 1, by = length(sset) / nauth)
ind.mat <- cbind(inds[-length(inds)], c(inds[-c(1, length(inds))], length(sset)))

# New calibration set randomly selected
# sel.set[[1]][1:10, ]  # same as previous version
# article.2004.shuffle.red <- article.2004.shuffle[-sset, ]
# set.seed(124)
# cal.set <- article.2004.shuffle.red[sample(1:nrow(article.2004.shuffle.red), size = 20, replace = FALSE), ]
# # which(rownames(cal.set) %in% rownames(article.2004.shuffle[sset, ]))  # 0
# write.csv(cal.set, file = "new_calibration_set.csv")

# Then draw sets for each reviewer
fnames <- paste(reviewers, "2004_draw2.csv", sep = "_")
sel.set <- vector("list", length = length(fnames))
names(sel.set) <- reviewers
for(i in 1:nrow(ind.mat)) {
  sel.set[[i]] <- article.2004.shuffle[sset[ind.mat[i, 1]:ind.mat[i, 2]], ]
  write.csv(sel.set[[i]], file = fnames[i])
}





