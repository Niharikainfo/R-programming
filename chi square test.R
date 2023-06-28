
B<-BaycoastData

#chi square test
chisq.test(B$Kitchen,B$Style)

#observed (or actual) frequencies
table(B$Kitchen,B$Style)
