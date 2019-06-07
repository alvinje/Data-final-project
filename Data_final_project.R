
print('Welcome to the final project of data schedule')

#super commentaire
library(data.table)
csv <- fread("data_DC.csv", encoding = 'UTF-8')

summary(csv)

dza