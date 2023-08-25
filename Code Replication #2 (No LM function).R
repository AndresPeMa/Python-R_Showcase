load('E:/CalPoly SLO/Fall Quarter 2022/Advanced Econometrics 1/Working directory (datafiles too)/apple.RData')
df <- data

#1st question
X = cbind(rep(1,5),df$regprc,df$educ,df$ecoprc,df$hhsize,df$male)
Y = df$reglbs

b = solve(t(X)%*%X)%*% (t(X)%*% Y)

b_1 = cbind(0,1,0,0,0,0)%*% b

b_1

#2nd question
Xt = t(X)
Yt = t(Y)
SSE = Yt %*% Y - t(b)%*% Xt %*% Y
SST = sum((Y-mean(Y))^2)

Rsquared = 1 -(SSE/SST)

#3rd question
AdjRsquared = 1 - ((1-Rsquared)%*%(660-1)/(660-5-1))


#4th question
X = cbind(rep(1,5),df$regprc,df$educ,df$ecoprc,df$hhsize,df$male,(df$regprc*df$educ))
Y = df$reglbs

b = solve(t(X)%*%X)%*% (t(X)%*% Y)

#4th question
#Yes, the sign of Beta3 is correct (+0.947) because we expect people to switch to regular apples when the price of eco apples increases which leads to an increase in purchases

#5th question
A = cbind(rep(1,5),df$regprc,df$educ,df$ecoprc,df$hhsize,df$male,(df$regprc*df$educ))
B = df$reglbs

C = solve(t(A)%*%A)%*% (t(A)%*% B)

b_6 = cbind(0,0,0,0,0,0,1)%*%C


#6th question
#If educ=20, the change in reglbs if regprc increases by $1 can be shown by 
inc_in_price = -b_1 + (b_6*20)


#7th question
#I think the more educated buyer is more sensitive to price, we can see that Beta6 in model2 shows that, for educated buyers, as the price of ecoapples increases the less ecoapples they will buy, they will substitute them with regular apples