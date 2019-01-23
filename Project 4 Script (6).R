# Author: Michelle Puglio
# MSBC5030 Project 4

# Part A
# Q1
# Download the Lending Clud Data for the year 2014, and read the
# data into a dta frame in R
data <- read.csv("~/Desktop/LoanStats3c.csv", header = FALSE, stringsAsFactors= FALSE)
colnames(data) <- data[1, ]
data <- data[-1, ]
rownames(data) <- seq(length=nrow(data))



# Q2a
# Restrict the data frame to the following variables: grades,
# sub_grade, zip_code, term, loan_amt, annual_inc, 
# verification_status, purpose, tax_liens, pct_tl_nvr_dlq,
# int_rate, loan_status. 
data <- data[c(3,6,7,9,10,14,15,17,21,23,104,107)]
colnames(data)

# Download the data dictionary from the Lending Club to learn about
# what each of these variables mean.
# grade = LC assigned loan grade
# sub_grade = LC assigned loan subgrade.
# zip_code = the first 3 numbers of the zip code provided by the
#	borrower in the loan application.
# term = the number of payments on the loan. Values are in months
#	and can be either 36 or 60.
# loan_amt = the listed amount of the loan applied for by the
#	borrower. If at some point in time, the credit departmemt reduces
#	the loan amount, then it will be reflected in this value.
# annual_inc = the self-reported annual income provided by the
#	borrower during registration.
# verification_status = indicates if the income was verified by the
#	LC, not verified, or if the income source was verified.
# purpose = a category provided by the borrower for the loan request.
# tax_liens = number of tax liens.
# pct_tl_nvr_dql = percent of trades never delinquent.
# int_rate = interest rate on the loan.
# loan_status = current status of the loan.



# Q2b 
# Inspect the characterstic of each of the variables in (a).
# This is a necessary step, but one that won't enter much into the final report.
sapply(data, class)
head(data, 20)

# Qbi
# How are each of these variiables stored with R? Is this the format
# that is most useful? For any variables stored in the "wrong" format,
# devise a strategy to convert thse to a more useful format.
data <- transform(data, loan_amnt = as.numeric(loan_amnt),
						annual_inc = as.numeric(annual_inc),
						pct_tl_nvr_dlq = as.numeric(pct_tl_nvr_dlq),
						tax_liens = as.numeric(tax_liens))
data <- transform(data, int_rate = as.numeric(sub("%", "", int_rate)))
data <- transform(data, term = as.numeric(sub(" months", "", term)))
data <- transform(data, zip_code = as.numeric(sub("xx", "", zip_code)))

data <- transform(data, grade = factor(grade))
data <- transform(data, sub_grade = factor(sub_grade))


# "character" to "numeric":
# loan_amnt, term, int_rate, annual_inc, zip_code, pct_tl_nvr_dql,
# tax_liens

# "character" to "factor":
# grade, sub_grade

# "character" to "logical":
# loan_status, purpose

sapply(data, class)


# Qbii
# For continuous variables, are there any outliers or skew? Think 
# about the likely consequences of these characteristics and any
# pratical solutions. You should address these issues in how you
# aggregate and analye the data, and describe any relevant issues in
# your report.
quantile(data$loan_amnt, na.rm = TRUE)
range(data$loan_amnt, na.rm = TRUE)
which(grepl(1000, data$loan_amnt)) 
which(grepl(35000, data$loan_amnt)) 
# Check to see if the max and mins are present in more than one row
# If so, they are not outliers.


quantile(data$annual_inc, na.rm = TRUE)
range(data$annual_inc , na.rm = TRUE)
which(grepl(3000, data$annual_inc))
which(grepl(7500000, data$annual_inc)) 
# Check to see if the max and mins are present in more than one row
# If so, they are not outliers.


quantile(data$pct_tl_nvr_dlq, na.rm = TRUE)
range(data$pct_tl_nvr_dlq, na.rm = TRUE)
which(grepl(16.7, data$pct_tl_nvr_dlq))
which(grepl(100.00, data$pct_tl_nvr_dlq)) 


quantile(data$tax_liens, na.rm = TRUE)
range(data$tax_liens, na.rm = TRUE)
which(grepl(0, data$tax_liens))
which(grepl(63, data$tax_liens)) 
# Check to see if the max and mins are present in more than one row
# If so, they are not outliers.
# "63" came back for max value at tax_liens, must be an outlier.
# Seemingly impossible. Also "39", "34", "20", "18", "17", "16", 
# "14", "13", and "12".
data <- data[-c(205547), ]

which(grepl(39, data$tax_liens)) 
data <- data[-c(188181), ]

which(grepl(34, data$tax_liens)) 
data <- data[-c(227223), ]

which(grepl(20, data$tax_liens)) 
data <- data[-c(218751), ]

which(grepl(18, data$tax_liens)) 
data <- data[-c(30407), ]

which(grepl(17, data$tax_liens)) 
data <- data[-c(3931), ]
data <- data[-c(142334), ]

which(grepl(16, data$tax_liens)) 
data <- data[-c(32221), ]
data <- data[-c(234040), ]

which(grepl(14, data$tax_liens)) 
data <- data[-c(37340), ]
data <- data[-c(67174), ]

which(grepl(13, data$tax_liens)) 
data <- data[-c(210761), ]

which(grepl(12, data$tax_liens)) 
data <- data[-c(2716), ]
data <- data[-c(233849), ]



quantile(data$int_rate, na.rm = TRUE)
range(data$int_rate, na.rm = TRUE)
which(grepl(6.00, data$int_rate))
which(grepl(26.06, data$int_rate))
# Check to see if the max and mins are present in more than one row
# If so, they are not outliers.

quantile(data$zip_code, na.rm = TRUE)
range(data$zip_code, na.rm = TRUE)
# Check to see if the max and mins are present in more than one row
# If so, they are not outliers.
# Delete all the zipcodes with two or one characters.
data <- data[-c(which(nchar(data$zip_code)==1)), ]
data <- data[-c(which(nchar(data$zip_code)==2)), ]



# Q3
# The Lending Club Data are at the loan level. Produce two 
# alternative "aggregated" data sets, which we'll examine at 
# different points throughout the analysis.
# Some guidance on how to aggregate within each group.
# For the continuous variables, comput the average within each group.
# For loan_status, compute two fractions: the fraction of loans that
# are current, and the fraction of loans that are fully paid.
# For home_ownership, compute the fraction of loans with a homeowner.
# For purpose,  compute fraction of debt consolidations.

data[(which(data$loan_status == "Fully Paid")), 8] <- 1
data[(which(data$loan_status == "Current")), 8] <- 0
data[(which(data$loan_status == "Charged Off")), 8] <- 0
data[(which(data$loan_status == NA)), 8] <- 0
data[(which(data$loan_status != 1)), 8] <- 0
data <- transform(data, loan_status = as.numeric(loan_status))

# For loan_status, 1 = "Fully Paid", 0 = else.


data[(which(data$purpose == "debt_consolidation")), 9] <- 1
data[(which(data$purpose == "car")), 9] <- 0
data[(which(data$purpose == "house")), 9] <- 0
data[(which(data$purpose == "credit_card")), 9] <- 0
data[(which(data$purpose == NA)), 9] <- 0
data[(which(data$purpose != 1)), 9] <- 0
data <- transform(data, purpose = as.numeric(purpose))

# For purpose, 1 = "debt_consolidation", 0 = else.
head(data, 10)
sapply(data, class)


# Q3i
# ZIP x Verification Status.
# A data set in which each observation is an aggregation of all the loans with 
# the same combination of (zip_code, varification_status).
aggdata1 <- data[c(1,2,3,6,7,8,9,10,11,12)]
head(aggdata1)
sapply(aggdata1, class)


ZIPxVS <- aggregate(aggdata1, by = list(aggdata1$zip_code, 
			aggdata1$verification_status), 
			FUN=mean)
ZIPxVS <- ZIPxVS[ ,-c(7,10)]
head(ZIPxVS)



# Q3ii
#  ZIP x Verification Status x Term x Subgrade.
# A data set in which each observation is an aggregation of all the
# loans with the same combination of (zip_code, verification_status,
# term, subgrade).
aggdata2 <- data[c(1,2,3,5,6,7,8,9,10,11,12)]
head(aggdata2)
ZIPxVSxTxSG <- aggregate(aggdata2, by = list(aggdata2$zip_code, 
			aggdata2$verification_status,
			aggdata2$term,
			aggdata2$sub_grade), 
			FUN=mean)
ZIPxVSxTxSG <- ZIPxVSxTxSG[ ,-c(8,10)]
head(ZIPxVSxTxSG)


# Q4
# For the ZIP-Verification_Status-Term-Subgrade level data set,
# summarize the key variables graphically and numerically. For
# this part, you should focus on what is the most informative way 
# to describe the distributions of the potential outcome variables
# (int_rate, loan_status, and loan_amnt), and their relation to
# annual income (and its verification), subgrade of the loan, the 
# fraction of debt consolidations, previous delinquencies, and tax 
# liens.
summary(ZIPxVSxTxSG)

summary(ZIPxVSxTxSG$loan_amnt)
# Average loan amount = $15,295

summary(ZIPxVSxTxSG$int_rate)
# Average interest rate = 15.04%

summary(ZIPxVSxTxSG$annual_inc)
# Average annual income of borrowers = $71,366

summary(ZIPxVSxTxSG$loan_status)
# 69.88% of loans are "Paid Off"

summary(ZIPxVSxTxSG$purpose)
# 60.91% of loans are "debt_consolidation"

summary(ZIPxVSxTxSG$pct_tl_nvr_dlq)
# Average percent of trades never delinquent = 94.46%

summary(ZIPxVSxTxSG$tax_liens)
# Average number of tax_liens = 0.05 (Most have 0)



# int_rate v. (annual_inc, verification_status, sub_grade,
#				purpose (debt_consolidations), pct_tl_nvr_dlq,
#				tax_liens)

int_rateREG <- lm(int_rate ~ annual_inc + Group.2 +
					Group.4 + purpose + pct_tl_nvr_dlq +
					tax_liens, data = ZIPxVSxTxSG)
summary(int_rateREG)




# loan_status v. (annual_inc, verification_status, sub_grade,
#				purpose (debt_consolidations), pct_tl_nvr_dlq,
#				tax_liens)

loan_statusREG <- lm(loan_status ~ annual_inc + Group.2 +
					Group.4 + purpose + pct_tl_nvr_dlq +
					tax_liens, data = ZIPxVSxTxSG)
summary(loan_statusREG)





# loan_amount v. (annual_inc, verification_status, sub_grade,
#				purpose (debt_consolidations), pct_tl_nvr_dlq,
#				tax_liens)
loan_amntREG <- lm(loan_amnt ~ annual_inc + Group.2 +
					Group.4 + purpose + pct_tl_nvr_dlq +
					tax_liens, data = ZIPxVSxTxSG)
summary(loan_amntREG)



# Q5
# Use the ZIP-Verification Status-Term-Subgrade level data to
# answer the following questions of interest:







