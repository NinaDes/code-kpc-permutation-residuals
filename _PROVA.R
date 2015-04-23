data = read.table(file='data.txt')
data = data[1:150,1:8]

source('kPC_Perm_Resid.R')

a = kPC_Perm_Resid(data = data, alpha = 0.5, sig = 1, numcol = 10, eps = 0.1, p = 10, test = 2, paral = 3)
