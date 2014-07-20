setwd("~/Eigene Dokumente/Coursera/reproducible_research/PeerAssessMent1")


# temp <- tempfile()
# download.file("http://www.newcl.org/data/zipfiles/a1.zip",temp)
# data <- read.table(unz(temp, "a1.dat"))
# unlink(temp)

data <- read.csv(unzip("activity.zip"))
summary(data)

#GRoup by date and sum up the steps
z<-aggregate(steps~date, data, sum)

# Group by date and perform means on that day
mean(z$steps)

# Group by date and perform median on that day
median(z$steps)

hist(z$steps)