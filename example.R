###############################################################################
# Quantified Weight Loss
# John Schrom, @johnschrom, http://john.mn
# What foursquare venues are associated with my weight loss/gain?

###############################################################################
# Libraries and functions...
source('rpi.R');
library('arules');

# Rolling average:
# Inputs:   Matrix w/ date in first column, value in second
#           Offset of days from min date in data
#           Number of days for average
# Outputs:  Average value for time period centered around start date (or NA)
rollingAverage <- function(data, offset, days) {
  startDate <- as.Date(min(data[,1])) + offset - ((days-1)/2);
  points <- which(data[,1] > startDate & data[,1] < (startDate + days));
  if (length(points) > 0 ) {
    return(mean(as.numeric(data[points, 2])));
  }
  return(NA);
}

# Calculate Change:
# Inputs:   Matrix w/ date in first column, value in second
#           index value to calculate change
#           normConstant: expected time period between points
# Outputs:  Average <<normConstant>> day(s) difference between points
calcChange <- function(i, data, normConstant) {
  if (i == 0) {
    return(NA);
  }
  return(as.numeric((data[i,2]-data[i-1,2])*
                      (normConstant/(data[i,1]-data[i-1, 1]))));
}

###############################################################################
# Gather Data
checkins <- getFoursquare('YOUR-ACCESS-TOKEN');
weights <- read.csv(file="YOUR-WITHINGS-DATA-DUMP.csv", 
                    head=TRUE);

###############################################################################
# Weight

# Format Weight
weights.parsed <- data.frame(date = as.Date(weights$DATE),
                             weight = as.numeric(weights$WEIGHT));
weights.xm <- as.matrix(weights.parsed);
startDate <- min(weights.xm[,1]);
diff <- as.Date(max(weights.xm[,1])) - as.Date(startDate);

# Calculate 7 and 3 day rolling averages
weights.roll.7 <- sapply(1:diff, rollingAverage, data=weights.xm, days=7);
#weights.roll.3 <- sapply(1:diff, rollingAverage, data=weights.xm, days=3);

# Only one data point per week
weights.roll <- cbind(as.Date(startDate) + 1:diff, weights.roll.7);
weights.roll <- weights.roll[which(weights.roll[,1]%%7 == 0 & 
                                     !is.na(weights.roll[,2])),];

# Calculate weekly weight change
weights.roll.chng <- cbind(weights.roll[2:nrow(weights.roll), 1],
                           unlist(sapply(1:nrow(weights.roll), 
                                         calcChange, data=weights.roll, 
                                         normConstant=7)));

###############################################################################
# Foursquare

# Convert dates from epoch to date, and group by week
checkins[,"createdat"] <- as.Date(as.POSIXct(as.numeric(checkins[,"createdat"]),
                                             origin="1970-01-01"));
checkins <- cbind(checkins,
                  week=(7*ceiling(as.numeric(checkins[,"createdat"])/7)));

# Generate cross-tabs
checkin.aggr <- xtabs(~week + venue_cat, data=checkins);
checkin.aggr.names <- xtabs(~week + venue_name, data=checkins);

###############################################################################
# Data Pre-processing

# this is assuming that every week where there's 4sq data, there's also wt data
xd <- cbind(weights.roll.chng, checkin.aggr[
  which(as.numeric(rownames(checkin.aggr)) %in% weights.roll.chng[,1]),]);
xd.names <- cbind(weights.roll.chng, checkin.aggr.names[
  which(as.numeric(rownames(checkin.aggr.names)) %in% weights.roll.chng[,1]),]);

# Create binarized matricies/vectors
wt.gain <- ifelse(xd[,2] > 0.3, 1, 0);
wt.loss <- ifelse(xd[,2] < -0.3, 1, 0);
xb <- ifelse(xd[,-which(colnames(xd) == "")] > 0, 1, 0);
xb.names <- ifelse(xd.names[,-which(colnames(xd.names) == "")] > 0, 1, 0);

###############################################################################
# Association Rule Mining

# Foursquare categories
rules <- apriori(cbind(wt.gain, wt.loss, xb), 
                 parameter = list(minlen=2, supp=4/length(wt.gain), conf=0.8),
                 appearance = list(rhs=c("wt.gain", "wt.loss"), default="lhs"));

#inspect(rules)

# Foursquare venue names
rules.names <- apriori(cbind(wt.gain, wt.loss, xb.names), 
                 parameter = list(minlen=2, supp=5/length(wt.gain), conf=0.7),
                 appearance = list(rhs=c("wt.gain", "wt.loss"), default="lhs"));

#inspect(rules.names)
