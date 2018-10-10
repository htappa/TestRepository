Adata <- read.csv(file="C:/Users/Henry Tappa/Google Drive/Github/TestRepository/LogReg.csv", header=TRUE, sep=",")

Adata[,1] <- as.factor(Adata[,1])
Adata[,4] <- as.factor(Adata[,4])

str(Adata)

# Construct t-intervals for the gre and gpa data for all applicants at 0.80 level and 0.99 level.

t.test(Adata$gre, conf.level = 0.80)
t.test(Adata$gre, conf.level = 0.99)

t.test(Adata$gpa, conf.level = 0.80)
t.test(Adata$gpa, conf.level = 0.99)


# Repeat the same calculation in Question 1 but for admitted (1) and rejected (0) separately.

admitted <- subset(Adata, admit == 1)
rejected <- subset(Adata, admit == 0)

t.test(admitted$gre, conf.level = .80)
t.test(admitted$gpa, conf.level = .99)

t.test(rejected$gre, conf.level = .80)
t.test(rejected$gpa, conf.level = .99)


# Make (box) plots showing the gre distribution among applicants from different school rankings.

boxplot(gre~rank, data = Adata, main="gre distribution by school rankings", xlab="rank", ylab="gre")


# A two sample t-test can be run to find if the mean are different. The null hypothesis is that 
# there is no difference between the means.

t.test(admitted$gre, rejected$gre)

# Given that we have a high t-score (3.8292) and a very low p-value (0.0001611) we can reject the
# null hypothesis.


# Repeat process for the gpa data.

boxplot(gpa~rank, data = Adata, main="gpa distribution by school rankings", xlab="rank", ylab="gpa")

t.test(admitted$gpa, rejected$gpa)

# Again, the two sample t-test show the means are different and that given the high t-score (3.8292) 
# and low p-value (0.0001611) we can reject the null hypothesis. Even though the means appear to be
# very similar, the scale is smaller than the gre scores, so the results are reasonable.


# Construct a contingency table between admit and rank.

tbl <- table(Adata$admit, Adata$rank)
tbl


# To find out if the chance of being admitted is affected by the rank of the undergraduate 
# institution, a Chi-squared test is most appropriate. The null hypothesis is that admission is 
# independent of the ranking of the undergraduate institution.

chisq.test(tbl)

# As the p-value 1.374 is greater than the .05 significance level, we do not reject the null 
# hypothesis that admission is independent of the ranking of the undergraduate institution.


# NEW: Since this is now a test file, we will read in a sample text file:

text <- read.table(file="C:/Users/Henry Tappa/Google Drive/Github/TestRepository/text.txt", 
                  header=TRUE, sep=",")
text
