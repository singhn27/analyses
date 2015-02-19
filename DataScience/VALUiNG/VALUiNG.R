# VALUiNG - Veterans Association Lung Cancer Data Analysis using Bayesian Networks
# Data provided by http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/valung.csv
# Input:           http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/Cvalung.html (description)
# Output:          Bayesian network graph, analysis of conditional probabilities, plots

# Libraries #

require(bnlearn)

# Analysis #

# Read data from CSV file
data <- read.csv("valung.csv", header = TRUE)

# Load data into data frame
df <- data.frame(data)

# Transform data in data frame to numeric type
tfd <- transform(df, therapy = as.numeric(therapy), cell = as.numeric(cell), t = as.numeric(t), dead = as.numeric(dead), kps = as.numeric(kps), diagtime = as.numeric(diagtime), age = as.numeric(age), prior = as.numeric(prior))

# Utilize Hill Climbing algorithm to determine Bayesian Network
bn_tfd <- hc(tfd)

# Plot Bayesian Network
plot(bn_tfd, main="Bayesian Network of VA Lung data")

# Fit data to determine conditional probabilities
fitted <- bn.fit(bn_tfd, data = tfd)

# Some analyses involving conditional probabilities
# Conditional probability of standard treatment given adenocarcinoma
cpquery(fitted, event = (therapy < 2), evidence = (cell < 2))

# Conditional probability of test treatment given adenocarcinoma
cpquery(fitted, event = (therapy > 1), evidence = (cell < 2))

# Conditional probability of standard treatment given large cell carcinoma
cpquery(fitted, event = (therapy < 2), evidence = (cell > 1 & cell < 3))

# Conditional probability of test treatment given large cell carcinoma
cpquery(fitted, event = (therapy > 1), evidence = (cell > 1 & cell < 3))

# Conditional probability of standard treatment given small cell carcinoma
cpquery(fitted, event = (therapy < 2), evidence = (cell > 2 & cell < 4))

# Conditional probability of test treatment given small cell carcinoma
cpquery(fitted, event = (therapy > 1), evidence = (cell > 2 & cell < 4))

# Conditional probability of standard treatment given squamous cell carcinoma
cpquery(fitted, event = (therapy < 2), evidence = (cell > 3))

# Conditional probability of test treatment given squamous cell carcinoma
cpquery(fitted, event = (therapy > 1), evidence = (cell > 3))

# Conditional probability of poor KPS given prior treatment
cpquery(fitted, event = (kps < 50), evidence = (prior > 1))

# Conditional probability of poor KPS given no prior treatment
cpquery(fitted, event = (kps < 50), evidence = (prior < 2))

# Conditional probability of lasting longer than 90 days in treatment
# given that the treatment starts within 1 month of diagnosis
cpquery(fitted, event = (t > 90), evidence = (diagtime < 2))

# Conditional probability of lasting longer than 90 days in treatment
# given that the treatment starts within 6 months of diagnosis
cpquery(fitted, event = (t > 90), evidence = (diagtime < 7))

# Conditional probability of lasting longer than 90 days in treatment
# given that the treatment starts within 1 year of diagnosis
cpquery(fitted, event = (t > 90), evidence = (diagtime < 13))

# Conditional probability of lasting longer than 90 days in treatment
# given that the treatment starts within 2 years of diagnosis
cpquery(fitted, event = (t > 90), evidence = (diagtime < 25))

# Conditional probability of lasting longer than 180 days in treatment
# given that the treatment starts within 1 month of diagnosis
cpquery(fitted, event = (t > 180), evidence = (diagtime < 2))

# Conditional probability of lasting longer than 180 days in treatment
# given that the treatment starts within 6 months of diagnosis
cpquery(fitted, event = (t > 180), evidence = (diagtime < 7))

# Conditional probability of lasting longer than 180 days in treatment
# given that the treatment starts within 1 year of diagnosis
cpquery(fitted, event = (t > 180), evidence = (diagtime < 13))

# Conditional probability of lasting longer than 180 days in treatment
# given that the treatment starts within 2 years of diagnosis
cpquery(fitted, event = (t > 180), evidence = (diagtime < 25))

# Conditional probability of lasting longer than 360 days in treatment
# given that the treatment starts within 1 month of diagnosis
cpquery(fitted, event = (t > 360), evidence = (diagtime < 2))

# Conditional probability of lasting longer than 360 days in treatment
# given that the treatment starts within 6 months of diagnosis
cpquery(fitted, event = (t > 360), evidence = (diagtime < 7))

# Conditional probability of lasting longer than 360 days in treatment
# given that the treatment starts within 1 year of diagnosis
cpquery(fitted, event = (t > 360), evidence = (diagtime < 13))

# Conditional probability of lasting longer than 360 days in treatment
# given that the treatment starts within 2 years of diagnosis
cpquery(fitted, event = (t > 360), evidence = (diagtime < 25))

# Create histogram of patient age
hist(tfd$age, col="blue", main="Patient Age Breakdown", xlab="Age (Years)")

# Create a dark blue to light blue gradient,
# used to clarify and beautify the plot below
gradient_blue <- colorRampPalette(c("darkblue", "lightblue"))

# Plot patient KPS by diagnosis time
plot(tfd$kps, tfd$diagtime, col=gradient_blue(5)[findInterval(tfd$diagtime, seq(3:6))], main="Patient Karnofsky Performance Status by Diagnosis Time", ylab="Time from Diagnosis to Entry (Months)", xlab="Karnofsky Performance Status (0-99)")
