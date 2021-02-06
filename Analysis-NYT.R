library(zoo)
library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(sandwich)
library(lmtest)
library(stargazer)
library(stats)
library(lfe)
setwd("~/Dropbox/News-comovement/codes")

ff = read_excel("F-F_Research_Data_Factors.xlsx")
ff = filter(ff, Date>=198101 & Date<=201708)
ff[,c('Mkt-RF','SMB','HML','RF')] = ff[,c('Mkt-RF','SMB','HML','RF')]/100
mm = read_excel("F-F_Momentum_Factor.xlsx")
mm = filter(mm, Date>=198101 & Date<=201708)
mm$Mom = mm$Mom/100

senti = read_excel('Wurgler_sentiment.xlsx')
senti = filter(senti,yearmo>=198012 & yearmo<=201708)
senti = as.numeric(senti$`SENT^`)
senti = (diff(senti))/senti[-length(senti)]

bab = read_excel('BAB.xlsx')
bab$Date = as.Date(bab$Date, "%m/%d/%Y")
bab = filter(bab, Date >= as.Date('1/31/1981',"%m/%d/%Y") & Date <= as.Date('8/31/2017',"%m/%d/%Y"))
bab = bab$Return

recession = read_csv('USREC.csv')
recession$DATE = sub(str_sub(recession$DATE,1,7), pattern = "-", replacement = "")
recession$DATE = as.numeric(recession$DATE)
recession = filter(recession, DATE >= 198101 & DATE <= 201708)
recession = recession$USREC

cum_rev = function(rev){
  cum_0 = c(1)
  for (i in 1:length(rev)){
    cum_0 = c(cum_0, cum_0[length(cum_0)]*(1+rev[i]))
  }
  return(cum_0[-1])
}
port_return  = function(r_total,strategy,period){
  rev_final = c()
  for ( i in 1:(ncol(strategy)-4)) {
    if (i+4+period>ncol(strategy)){
      r =matrix(0,nrow = nrow(strategy),ncol=1)
    }else{
      r = as.matrix(unlist(monthly_return[,i+4+period]))
    }
    rev_final = c(rev_final, c(t(r)%*%as.matrix(unlist(strategy[,i+4]))))
  }
  rev_final
}

normalize = function(x){
  (x-median(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
}

base = read_csv("compustat_crsp_monthly_cleaned.csv")
top_500_total = read_csv("MktCap_Top500_Unique(1981-2017)_MonthlyIndicator.csv")
Imp0 = read_csv("NYT_Importance_0_Mkt500.csv")
# Imp0 = read_csv("NYT_Importance_0_Mkt500.csv")
# for (i in 5:ncol(Imp0)){
#   Imp0[which(Imp0[,]<2)] = 0
# }

monthly_return = read_csv("NYT_RET_Mkt500.csv")
monthly_size = read_csv("NYT_SIZE_Mkt500.csv")
monthly_bm = read_csv("NYT_BM_Mkt500.csv")
monthly_beta = read_csv("NYT_BETA_Mkt500.csv")
monthly_mm = read_csv("NYT_MOM3_Mkt500.csv")
monthly_ivol = read_csv("NYT_IVOL_Mkt500.csv")
monthly_smbbeta = read_csv("NYT_SMBBETA_Mkt500.csv")
monthly_hmlbeta = read_csv("NYT_HMLBETA_Mkt500.csv")
monthly_mombeta = read_csv("NYT_MOMBETA_Mkt500.csv")

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
p = which(ff$Date %in% months)
RF = ff$RF[p]
Mkt = ff$`Mkt-RF`[p]
Mkt_lag = ff$`Mkt-RF`[p-1]
SMB = ff$SMB[p]
SMB_lag = ff$SMB[p-1]
HML = ff$HML[p]
HML_lag = ff$HML[p-1]
Mom = mm$Mom[p]
Mom_lag = mm$Mom[p-1]
BAB = bab[p]
BAB_lag = bab[p-1]
RECESSION = recession[p]
RECESSION_lag = recession[p-1]
SENTI = senti[p]
SENTI_lag = senti[p-1]

coef_estimate = c()
coef_pvalue = c()
Imp0_raw = Imp0_p1 = Imp0_p2 = Imp0
horizon = 3
for (m in 1:(ncol(Imp0)-4)) {
  if (m<horizon | m<3){
    p = which(top_500_total[,m+4]==1)
    y_here = (unlist(log(1+Imp0[p,m+4])))
    x_here1 = normalize(unlist(log(1+monthly_size[p,m+4])))
    x_here2 = normalize(unlist(abs(monthly_mm[p,m+4])))
    x_here3 = normalize(unlist((monthly_ivol[p,m+4])))
    x_here4 = normalize(unlist((monthly_bm[p,m+4])))
    
    x_here5 = normalize(unlist((monthly_beta[p,m+4])))
    x_here6 = normalize(unlist((monthly_smbbeta[p,m+4])))
    x_here7 = normalize(unlist((monthly_hmlbeta[p,m+4])))
    x_here8 = normalize(unlist((monthly_mombeta[p,m+4])))
    x_here9 = normalize(unlist((monthly_mm[p,m+4])))
    
    regression = lm(y_here ~ x_here1 + x_here2 + x_here3 + x_here4 +
                      x_here5 + x_here6 + x_here7 + x_here8 + x_here9-1)
    coef_estimate = rbind(coef_estimate,c(coefficients(regression),summary(regression)$adj.r.squared))
    coef_pvalue = rbind(coef_pvalue, (summary(regression)$coefficients[,3]))
  }else{
    p = which(top_500_total[,m+4]==1)
    index_here = c((m-horizon+1):m)+4
    #firms_here = rep(top_500_total$conml[p],horizon)
    #firms_here = as.vector(as.matrix(Imp0[p,index_here]!=0))*1+1
    firms_here = rep(as.vector(as.matrix(rowSums(Imp0[p,c((m-3+1):m)+4])!=0))*1+1,horizon)
    time_here = c()
    for (h in 1:horizon) {
      time_here = c(time_here,rep(RECESSION[m-horizon+h],length(p)))
    }
    industry_here = as.numeric(rep(str_sub(Imp0$naics[p],1,1),horizon))
    industry_here = (industry_here==5|industry_here==4|industry_here==6)*1+1
    y_here = normalize(as.vector(as.matrix(log(1+Imp0[p,index_here]))))
    x_here1 = normalize(as.vector(as.matrix(log(1+monthly_size[p,index_here]))))
    x_here2 = normalize(as.vector(as.matrix(abs(monthly_mm[p,index_here]))))
    x_here3 = normalize(as.vector(as.matrix(monthly_ivol[p,index_here])))
    x_here4 = normalize(as.vector(as.matrix(monthly_bm[p,index_here])))
    x_here5 = normalize(as.vector(as.matrix(monthly_beta[p,index_here])))
    x_here6 = normalize(as.vector(as.matrix(monthly_smbbeta[p,index_here])))
    x_here7 = normalize(as.vector(as.matrix(monthly_hmlbeta[p,index_here])))
    x_here8 = normalize(as.vector(as.matrix(monthly_mombeta[p,index_here])))
    x_here9 = normalize(as.vector(as.matrix((monthly_mm[p,index_here]))))
    regression = felm(y_here ~ x_here1 + x_here2 + x_here3 + x_here4 +
                        x_here5 + x_here6 + x_here7 + x_here8 + x_here9|firms_here)
    coef_estimate = rbind(coef_estimate,c(coefficients(regression),summary(regression)$adj.r.squared))
    coef_pvalue = rbind(coef_pvalue, (summary(regression)$coefficients[,3]))
    Imp0_p1[p,m+4] = tail(coefficients(regression)[3]*x_here3 +
                           coefficients(regression)[4]*x_here4 +
                            coefficients(regression)[9]*x_here9 +
                            coefficients(regression)[2]*x_here2,length(p))
    Imp0_p2[p,m+4] = tail(coefficients(regression)[5]*x_here5 +
                           coefficients(regression)[6]*x_here6 +
                           coefficients(regression)[7]*x_here7 +
                           coefficients(regression)[8]*x_here8,length(p))
    Imp0_raw[p,m+4] = tail(regression$residuals,length(p))
      
  }
}

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
months = as.Date(as.yearmon(months, "%Y%m"))
p1 = match(as.Date("1995-01-01"), months)
p2 = match(as.Date("2015-12-01"), months)
months = months[p1:p2]
months_crysis = which(RECESSION[p1:p2]==1)

test = lm(coef_estimate[p1:p2,] ~ 1)
coeftest(test, vcov. = NeweyWest(test, lag = 3, prewhite = F, adjust = T, verbose = T))

for (i in 2:9){
  print(cor(coef_estimate[p1:p2,i],coef_pvalue[p1:p2,i]))
}

###### coeff 画图 #####
pdf("projection_r.pdf")
y = coef_estimate[p1:p2,10]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'Adj. r-squared')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=mean(y), col=c("red"), lty=c(1), lwd=c(2))
dev.off()

pdf("significance_charcs.pdf")
par(mfcol = c(2,2))
y = coef_pvalue[p1:p2,9]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'CumRet')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=2, col=c("red"), lty=c(1), lwd=c(2))
abline(h=-2, col=c("red"), lty=c(1), lwd=c(2))

y = coef_pvalue[p1:p2,2]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = '|CumRet|')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=2, col=c("red"), lty=c(1), lwd=c(2))
abline(h=-2, col=c("red"), lty=c(1), lwd=c(2))

y = coef_pvalue[p1:p2,3]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'IVOL')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=2, col=c("red"), lty=c(1), lwd=c(2))
abline(h=-2, col=c("red"), lty=c(1), lwd=c(2))

y = coef_pvalue[p1:p2,4]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '', 
     main = 'B/M')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=2, col=c("red"), lty=c(1), lwd=c(2))
abline(h=-2, col=c("red"), lty=c(1), lwd=c(2))

dev.off()


pdf("significance_betas.pdf")
par(mfcol = c(2,2))
y = coef_pvalue[p1:p2,5]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'Market beta')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=2, col=c("red"), lty=c(1), lwd=c(2))
abline(h=-2, col=c("red"), lty=c(1), lwd=c(2))

y = coef_pvalue[p1:p2,6]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'SMB beta')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=2, col=c("red"), lty=c(1), lwd=c(2))
abline(h=-2, col=c("red"), lty=c(1), lwd=c(2))

y = coef_pvalue[p1:p2,7]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'HML beta')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=2, col=c("red"), lty=c(1), lwd=c(2))
abline(h=-2, col=c("red"), lty=c(1), lwd=c(2))

y = coef_pvalue[p1:p2,8]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '', 
     main = 'MOM beta')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=2, col=c("red"), lty=c(1), lwd=c(2))
abline(h=-2, col=c("red"), lty=c(1), lwd=c(2))
dev.off()


coef_if = (coef_pvalue>=2|coef_pvalue<=-2)*1

pdf("coeff_charcs.pdf")
par(mfcol = c(2,2))
y = coef_estimate[p1:p2,9]
plot(seq(1:length(months)), y, "l", col = 'grey', xaxt='n', xlab = '', ylab = '',
     main = 'CumRet')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=0, col=c("black"), lty=c(1), lwd=c(2))
y = coef_estimate[p1:p2,9]*coef_if[p1:p2,9]
lines(y,col = 'red')

y = coef_estimate[p1:p2,2]
plot(seq(1:length(months)), y, "l", col = 'grey', xaxt='n', xlab = '', ylab = '',
     main = '|CumRet|')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=0, col=c("black"), lty=c(1), lwd=c(2))
y = coef_estimate[p1:p2,2]*coef_if[p1:p2,2]
lines(y,col = 'red')

y = coef_estimate[p1:p2,3]
plot(seq(1:length(months)), y, "l", col = 'grey', xaxt='n', xlab = '', ylab = '',
     main = 'IVOL')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=0, col=c("black"), lty=c(1), lwd=c(2))
y = coef_estimate[p1:p2,3]*coef_if[p1:p2,3]
lines(y,col = 'red')

y = coef_estimate[p1:p2,4]
plot(seq(1:length(months)), y, "l", col = 'grey', xaxt='n', xlab = '', ylab = '',
     main = 'B/M')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=0, col=c("black"), lty=c(1), lwd=c(2))
y = coef_estimate[p1:p2,4]*coef_if[p1:p2,4]
lines(y,col = 'red')
dev.off()

pdf("coeff_betas.pdf")
par(mfcol = c(2,2))
y = coef_estimate[p1:p2,5]
plot(seq(1:length(months)), y, "l", col = 'grey', xaxt='n', xlab = '', ylab = '',
     main = 'Beta')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=0, col=c("black"), lty=c(1), lwd=c(2))
y = coef_estimate[p1:p2,5]*coef_if[p1:p2,5]
lines(y,col = 'red')

y = coef_estimate[p1:p2,6]
plot(seq(1:length(months)), y, "l", col = 'grey', xaxt='n', xlab = '', ylab = '',
     main = 'SMB beta')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=0, col=c("black"), lty=c(1), lwd=c(2))
y = coef_estimate[p1:p2,6]*coef_if[p1:p2,6]
lines(y,col = 'red')

y = coef_estimate[p1:p2,7]
plot(seq(1:length(months)), y, "l", col = 'grey', xaxt='n', xlab = '', ylab = '',
     main = 'HML beta')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=0, col=c("black"), lty=c(1), lwd=c(2))
y = coef_estimate[p1:p2,7]*coef_if[p1:p2,7]
lines(y,col = 'red')

y = coef_estimate[p1:p2,8]
plot(seq(1:length(months)), y, "l", col = 'grey', xaxt='n', xlab = '', ylab = '',
     main = 'MOM beta')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(h=0, col=c("black"), lty=c(1), lwd=c(2))
y = coef_estimate[p1:p2,8]*coef_if[p1:p2,8]
lines(y,col = 'red')
dev.off()

####### projection ########
strategy_cph = Imp0
strategy_cph[,5:ncol(Imp0)] = 0
strategy_ncph = strategy_cpl = strategy_ncpl = strategy_cph
strategy_nc = strategy_c = strategy_crl = strategy_crh = strategy_ncrl = strategy_ncrh = strategy_cph
strategy_ph = strategy_pl = strategy_rh = strategy_rl = strategy_cph
Imp0_p = Imp0_p1
Imp0_p[,5:443] = Imp0_p[,5:443] + Imp0_p2[,5:443]
Imp0_r = Imp0_raw
#Imp0_r[,5:443] = Imp0_r[,5:443] + Imp0_p2[,5:443]
for (i in 1:(ncol(Imp0)-4)){
  # non-covered
  p = which(top_500_total[,i+4]==1 & Imp0[,i+4] == 0)
  strategy_nc[p,i+4] = monthly_size[p,i+4]/sum(monthly_size[p,i+4])
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_ncpl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_ncph[p1,i+4] = 1/length(p1)
  }
  temp = quantile(unlist(Imp0_r[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_r[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_ncrl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_r[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_ncrh[p1,i+4] = 1/length(p1)
  }
  
  # covered
  p = which(top_500_total[,i+4]==1 & Imp0[,i+4] != 0)
  strategy_c[p,i+4] = 1/length(p)
  temp = quantile(unlist(Imp0_r[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_r[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_crl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_r[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_crh[p1,i+4] = 1/length(p1)
  }
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_cpl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_cph[p1,i+4] = 1/length(p1)
  }
  
  #pooled
  p = which(top_500_total[,i+4]==1)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_pl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_ph[p1,i+4] = 1/length(p1)
  }
  
  temp = quantile(unlist(Imp0_r[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_r[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_rl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_r[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_rh[p1,i+4] = 1/length(p1)
  }
  
}

rev_crh = port_return(monthly_return,strategy_crh,1)
rev_crl = port_return(monthly_return,strategy_crl,1)
rev_cph = port_return(monthly_return,strategy_cph,1)
rev_cpl = port_return(monthly_return,strategy_cpl,1)
rev_ncph = port_return(monthly_return,strategy_ncph,1)
rev_ncpl = port_return(monthly_return,strategy_ncpl,1)
rev_ncrh = port_return(monthly_return,strategy_ncrh,1)
rev_ncrl = port_return(monthly_return,strategy_ncrl,1)
rev_nc = port_return(monthly_return,strategy_nc,1)
rev_c = port_return(monthly_return,strategy_c,1)
rev_ph = port_return(monthly_return,strategy_ph,1)
rev_pl = port_return(monthly_return,strategy_pl,1)
rev_rh = port_return(monthly_return,strategy_rh,1)
rev_rl = port_return(monthly_return,strategy_rl,1)

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
months = as.Date(as.yearmon(months, "%Y%m"))
p1 = match(as.Date("1995-01-01"), months)
p2 = match(as.Date("2015-12-01"), months)
months = months[p1:p2]
months_crysis = which(RECESSION[p1:p2]==1)

pdf("baseline.pdf")
par(mfcol = c(2,1))
y = cum_rev(rev_cph[p1:p2])
plot(seq(1:length(months)), y, "l", col = 'red', xaxt='n', xlab = '', ylab = '', ylim = c(0,22),
     main = 'Sort on EP')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(1))
y = cum_rev(rev_cpl[p1:p2])
lines(seq(1:length(months)),y, "l", col = 'orange')
y = cum_rev(rev_ncph[p1:p2])
lines(seq(1:length(months)),y, "l", col = 'blue')
y = cum_rev(rev_ncpl[p1:p2])
lines(seq(1:length(months)),y, "l", col = 'green')
legend("topleft", legend=c("Covered high","Covered low","Non-covered high", "Non-covered low"),
       col=c("red","orange", "blue", "green"), lty=1, cex=0.8,
       box.lty=1)

# y = cum_rev(rev_crh[p1:p2])
# plot(seq(1:length(months)), y, "l", col = 'red', xaxt='n', xlab = '', ylab = '', ylim = c(0,20),
#      main = 'Sort on attention')
# axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
# abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(2))
# y = cum_rev(rev_crl[p1:p2])
# lines(seq(1:length(months)),y, "l", col = 'orange')
# y = cum_rev(rev_nc[p1:p2])
# lines(seq(1:length(months)),y, "l", col = 'blue')
# #y = cum_rev(rev_ncrl[p1:p2])
# #lines(seq(1:length(months)),y, "l", col = 'green')
# legend("topleft", legend=c("Covered high","Covered low", "Non-covered"),
#        col=c("red","orange","blue"), lty=1, cex=0.8,
#        box.lty=1)

y = cum_rev(rev_ph[p1:p2])
plot(seq(1:length(months)), y, "l", col = 'red', xaxt='n', xlab = '', ylab = '', ylim = c(0,22),
     main = 'Sort on EP')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(1))
y = cum_rev(rev_pl[p1:p2])
lines(seq(1:length(months)),y, "l", col = 'orange')
legend("topleft", legend=c("Pooled high","Pooled low"),
       col=c("red","orange"), lty=1, cex=0.8,
       box.lty=1)
dev.off()

y = cum_rev(rev_rh[p1:p2])
plot(seq(1:length(months)), y, "l", col = 'blue', xaxt='n', xlab = '', ylab = '', ylim = c(0,15),
     main = 'Sort on partial residual')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(2))
y = cum_rev(rev_rl[p1:p2])
lines(seq(1:length(months)),y, "l", col = 'green')


test = rev_cph - RF
regress1 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress1, vcov. = NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T))
temp1 = sqrt(diag(NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_cph - rev_cpl
regress2 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress2, vcov. = NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T))
temp2 = sqrt(diag(NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph - RF
regress3 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress3, vcov. = NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T))
temp3 = sqrt(diag(NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph - rev_ncpl
regress4 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress4, vcov. = NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T))
temp4 = sqrt(diag(NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_cph - rev_ncpl
regress5 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress5, vcov. = NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T))
temp5 = sqrt(diag(NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T)))

panel = stargazer(regress1,regress2, regress3,regress4,regress5,
                  type="latex",
                  se = list(temp1,temp2,temp3,temp4,temp5),
                  dep.var.labels = '',
                  star.cutoffs = c(0.1, 0.05, 0.01),digits = 4)

alpha = cum_rev((rev_cph - rev_ncpl)[p1:p2])
y = alpha
plot(seq(1:length(months)), y, "l", col = 'red', xaxt='n', xlab = '', ylab = '',
     main = 'Sort on EP')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(1))
mean(alpha)/sd(alpha)

test = rev_crh - RF
regress1 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress1, vcov. = NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T))
temp1 = sqrt(diag(NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_crh - rev_crl
regress2 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress2, vcov. = NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T))
temp2 = sqrt(diag(NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_crh - rev_nc
regress3 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress3, vcov. = NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T))
temp3 = sqrt(diag(NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T)))





test = port_return(monthly_return,strategy_cph,1) - port_return(monthly_return,strategy_ncpl,1)
regress3 = lm(test[p1:p2] ~ 1)
coeftest(regress3, vcov. = NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T))
temp3 = sqrt(diag(NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = port_return(monthly_return,strategy_cph,3) - port_return(monthly_return,strategy_ncpl,3)
regress4 = lm(test[p1:p2] ~ 1)
coeftest(regress4, vcov. = NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T))
temp4 = sqrt(diag(NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = port_return(monthly_return,strategy_cph,6) - port_return(monthly_return,strategy_ncpl,6)
regress5 = lm(test[p1:p2] ~ 1)
coeftest(regress5, vcov. = NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T))
temp5 = sqrt(diag(NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T)))

panel = stargazer(regress3, regress4, regress5,
                  type="latex",
                  se = list(temp3,temp4,temp5),
                  dep.var.labels = '',
                  star.cutoffs = c(0.1, 0.05, 0.01),digits = 4)
######### market condition #########
bears = rep(0,2)
for (i in 3:length(Mkt)){
  if (Mkt[i-1]<0 &Mkt[i-2]<0){
    bears = c(bears,1)
  }else{
    bears = c(bears,0)
  }
}

#p3 = which(coef_pvalue[,5]<=-2)
p3 = which(bears[] == 1|coef_pvalue[,5]<=-2)

test = rev_cph - rev_ncpl
test[p3] = -test[p3]
regress5 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress5, vcov. = NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T))
temp5 = sqrt(diag(NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T)))

residual = residuals(regress5)
#mean((rev_ncph - rev_ncpl)[p1:p2])/sd((rev_ncph - rev_ncpl)[p1:p2])*sqrt(12)
(0.0137+mean(residual))/sd(residual)*sqrt(12)

turnover = c()
for (i in 2:(ncol(strategy_cph)-4)){
  total_position = length(which(strategy_cph[,i+3]!=0)) + length(which(strategy_ncpl[,i+3]!=0))
  changed_position = length(which(strategy_cph[,i+3]!=strategy_cph[,i+4])) + 
    length(which(strategy_ncpl[,i+3]!=strategy_ncpl[,i+4]))
  turnover = c(turnover,min(1,changed_position/total_position))
}

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
months = as.Date(as.yearmon(months, "%Y%m"))
p1 = match(as.Date("1995-01-01"), months)
p2 = match(as.Date("2015-12-01"), months)
months = months[p1:p2]

pdf('cph.pdf')
y = port_return(monthly_return,strategy_cph,1)
y = cum_rev(y[p1:p2])
#y = turnover[p1:p2]
plot(seq(1:length(months)),  y, "l", col = 'black', xaxt='n', xlab = '', ylab = '', 
     main = 'Cumulative return')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(1))
dev.off()

####### which factor drives the most ########
strategy_cph = Imp0
strategy_cph[,5:ncol(Imp0)] = 0
strategy_ncph = strategy_ncpl = strategy_cpl = strategy_cph

# Imp0_p = Imp0
# for (m in 1:(ncol(Imp0)-4)) {
#   p = which(top_500_total[,m+4]==1)
#   Imp0_p[p,m+4] = normalize(as.matrix((monthly_mombeta[p,m+4])))*coef_estimate[m,8]
# }

Imp0_p = Imp0
horizon = 3
for (m in 1:(ncol(Imp0)-4)) {
  if (m>=horizon){
    p = which(top_500_total[,m+4]==1)
    index_here = c((m-horizon+1):m)+4
    #firms_here = as.vector(as.matrix(Imp0[p,index_here]!=0))*1+1
    firms_here = rep(as.vector(as.matrix(rowSums(Imp0[p,index_here])!=0))*1+1,horizon)
    time_here = c()
    for (h in 1:horizon) {
      time_here = c(time_here,rep(RECESSION[m-horizon+h],length(p)))
    }
    industry_here = rep(str_sub(Imp0$naics[p],1,2),horizon)
    y_here = normalize(as.vector(as.matrix(log(1+Imp0[p,index_here]))))
    x_here1 = normalize(as.vector(as.matrix(log(1+monthly_size[p,index_here]))))
    x_here2 = normalize(as.vector(as.matrix(abs(monthly_mm[p,index_here]))))
    x_here3 = normalize(as.vector(as.matrix(monthly_ivol[p,index_here])))
    x_here4 = normalize(as.vector(as.matrix(monthly_bm[p,index_here])))
    x_here5 = normalize(as.vector(as.matrix(monthly_beta[p,index_here])))
    x_here6 = normalize(as.vector(as.matrix(monthly_smbbeta[p,index_here])))
    x_here7 = normalize(as.vector(as.matrix(monthly_hmlbeta[p,index_here])))
    x_here8 = normalize(as.vector(as.matrix(monthly_mombeta[p,index_here])))
    x_here9 = normalize(as.vector(as.matrix((monthly_mm[p,index_here]))))
    regression = felm(y_here ~ x_here1 + x_here2 + x_here3 + x_here4 +
                        x_here5 + x_here6 + x_here7 + x_here8 + x_here9|firms_here)
    Imp0_p[p,m+4] = tail(coefficients(regression)[3]*x_here3 +
                           coefficients(regression)[9]*x_here9 +
                            coefficients(regression)[4]*x_here4 +
                            coefficients(regression)[7]*x_here7 +
                           coefficients(regression)[8]*x_here8 +
                           coefficients(regression)[6]*x_here6 -
                           coefficients(regression)[1]*x_here1 +
                           coefficients(regression)[5]*x_here5 +
                            coefficients(regression)[2]*x_here2,length(p))
  }
}


for (i in 1:(ncol(Imp0)-4)){
  # non-covered
  p = which(top_500_total[,i+4]==1 & Imp0[,i+4] == 0)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_ncpl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_ncph[p1,i+4] = 1/length(p1)
  }
  
  # covered
  p = which(top_500_total[,i+4]==1 & Imp0[,i+4] != 0)
  strategy_c[p,i+4] = 1/length(p)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_cpl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_cph[p1,i+4] = 1/length(p1)
  }
}

rev_cph = port_return(monthly_return,strategy_cph,1)
rev_cpl = port_return(monthly_return,strategy_cpl,1)
rev_ncph = port_return(monthly_return,strategy_ncph,1)
rev_ncpl = port_return(monthly_return,strategy_ncpl,1)

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
months = as.Date(as.yearmon(months, "%Y%m"))
p1 = match(as.Date("1995-01-01"), months)
p2 = match(as.Date("2015-12-01"), months)
months = months[p1:p2]
months_crysis = which(RECESSION[p1:p2]==1)

test = rev_cph - RF
regress1 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress1, vcov. = NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T))
temp1 = sqrt(diag(NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_cph - rev_cpl
regress2 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress2, vcov. = NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T))
temp2 = sqrt(diag(NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph - RF
regress3 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress3, vcov. = NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T))
temp3 = sqrt(diag(NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph - rev_ncpl
regress4 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress4, vcov. = NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T))
temp4 = sqrt(diag(NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_cph - rev_ncpl
regress5 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress5, vcov. = NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T))
temp5 = sqrt(diag(NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T)))

panel = stargazer(regress1,regress2, regress3,regress4,regress5,
                  type="latex",
                  se = list(temp1,temp2,temp3,temp4,temp5),
                  dep.var.labels = '',
                  star.cutoffs = c(0.1, 0.05, 0.01),digits = 4)

###### static sorting #########
strategy_cph = Imp0
strategy_cph[,5:ncol(Imp0)] = 0
strategy_ncph = strategy_ncpl = strategy_cpl = strategy_pl = strategy_ph = strategy_cph

coef_estimate1 = mean(coef_estimate[,1],na.rm = T)
coef_estimate2 = mean(coef_estimate[,2],na.rm = T)
coef_estimate3 = mean(coef_estimate[,3],na.rm = T)
coef_estimate4 = mean(coef_estimate[,4],na.rm = T)
coef_estimate5 = mean(coef_estimate[,5],na.rm = T)
coef_estimate6 = mean(coef_estimate[,6],na.rm = T)
coef_estimate7 = mean(coef_estimate[,7],na.rm = T)
coef_estimate8 = mean(coef_estimate[,8],na.rm = T)
coef_estimate9 = mean(coef_estimate[,9],na.rm = T)

Imp0_p = Imp0
horizon = 3
for (m in 1:(ncol(Imp0)-4)) {
  if (m>=horizon){
    p = which(top_500_total[,m+4]==1)
    index_here = c((m-horizon+1):m)+4
    y_here = normalize(as.vector(as.matrix(log(1+Imp0[p,index_here]))))
    x_here1 = normalize(as.vector(as.matrix(log(1+monthly_size[p,index_here]))))
    x_here2 = normalize(as.vector(as.matrix(abs(monthly_mm[p,index_here]))))
    x_here3 = normalize(as.vector(as.matrix(monthly_ivol[p,index_here])))
    x_here4 = normalize(as.vector(as.matrix(monthly_bm[p,index_here])))
    x_here5 = normalize(as.vector(as.matrix(monthly_beta[p,index_here])))
    x_here6 = normalize(as.vector(as.matrix(monthly_smbbeta[p,index_here])))
    x_here7 = normalize(as.vector(as.matrix(monthly_hmlbeta[p,index_here])))
    x_here8 = normalize(as.vector(as.matrix(monthly_mombeta[p,index_here])))
    x_here9 = normalize(as.vector(as.matrix((monthly_mm[p,index_here]))))
    Imp0_p[p,m+4] = tail(coef_estimate3*x_here3 +
                           coef_estimate9*x_here9 +
                           coef_estimate4*x_here4 +
                           coef_estimate7*x_here7 +
                           coef_estimate8*x_here8 +
                           coef_estimate6*x_here6 +
                           coef_estimate5*x_here5 +
                           coef_estimate2*x_here2,length(p))
  }
}

for (i in 1:(ncol(Imp0)-4)){
  # non-covered
  p = which(top_500_total[,i+4]==1 & Imp0[,i+4] == 0)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_ncpl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_ncph[p1,i+4] = 1/length(p1)
  }
  
  # covered
  p = which(top_500_total[,i+4]==1 & Imp0[,i+4] != 0)
  strategy_c[p,i+4] = 1/length(p)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_cpl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_cph[p1,i+4] = 1/length(p1)
  }
  
  #pooled
  p = which(top_500_total[,i+4]==1)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_pl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_ph[p1,i+4] = 1/length(p1)
  }
}

rev_cph = port_return(monthly_return,strategy_cph,1)
rev_cpl = port_return(monthly_return,strategy_cpl,1)
rev_ncph = port_return(monthly_return,strategy_ncph,1)
rev_ncpl = port_return(monthly_return,strategy_ncpl,1)
rev_ph = port_return(monthly_return,strategy_ph,1)
rev_pl = port_return(monthly_return,strategy_pl,1)

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
months = as.Date(as.yearmon(months, "%Y%m"))
p1 = match(as.Date("1995-01-01"), months)
p2 = match(as.Date("2015-12-01"), months)
months = months[p1:p2]
months_crysis = which(RECESSION[p1:p2]==1)

test = rev_cph - RF
regress1 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress1, vcov. = NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T))
temp1 = sqrt(diag(NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph - RF
regress2 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress2, vcov. = NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T))
temp2 = sqrt(diag(NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_cph - rev_cpl
regress3 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress3, vcov. = NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T))
temp3 = sqrt(diag(NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph - rev_ncpl
regress4 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress4, vcov. = NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T))
temp4 = sqrt(diag(NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_cph - rev_ncpl
regress5 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress5, vcov. = NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T))
temp5 = sqrt(diag(NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T)))

panel = stargazer(regress1,regress2, regress3,regress4,regress5,
                  type="latex",
                  se = list(temp1,temp2,temp3,temp4,temp5),
                  dep.var.labels = '',
                  star.cutoffs = c(0.1, 0.05, 0.01),digits = 4)

test = rev_ph - RF
regress1 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress1, vcov. = NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T))
temp1 = sqrt(diag(NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ph - rev_pl
regress2 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress2, vcov. = NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T))
temp2 = sqrt(diag(NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T)))

panel = stargazer(regress1,regress2,
                  type="latex",
                  se = list(temp1,temp2),
                  dep.var.labels = '',
                  star.cutoffs = c(0.1, 0.05, 0.01),digits = 4)

###### trading in different sub-group #####
Imp0_p = Imp0_p1
Imp0_p[,5:443] = Imp0_p[,5:443] + Imp0_p2[,5:443]

strategy_h = Imp0
strategy_h[,5:ncol(Imp0)] = 0
strategy_l = strategy_h
feature = monthly_beta

for (i in 1:(ncol(Imp0)-4)){
  p = which(top_500_total[,i+4]==1)
  temp = median(unlist(feature[p,i+4]))
  p = which(top_500_total[,i+4]==1 & feature[,i+4] < temp)
  temp = quantile(unlist(Imp0_r[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_l[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_h[p1,i+4] = 1/length(p1)
  }
}
rev_h = port_return(monthly_return,strategy_h,1)
rev_l = port_return(monthly_return,strategy_l,1)

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
months = as.Date(as.yearmon(months, "%Y%m"))
p1 = match(as.Date("1995-01-01"), months)
p2 = match(as.Date("2015-12-01"), months)
months = months[p1:p2]

test = rev_h - rev_l
regress1 = lm(test[p1:p2] ~ Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress1, vcov. = NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T))
temp1 = sqrt(diag(NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T)))

panel = stargazer(regress1,
                  type="latex",
                  se = list(temp1),
                  dep.var.labels = '',
                  covariate.labels=c("Mkt-RF","SMB","HML","MOM"), 
                  star.cutoffs = c(0.1, 0.05, 0.01),digits = 4)


###### horse-racing #######
library(glmnet)

coeff_lasso_EP = c()
coeff_lasso_COVERAGE = c()
r_test = c()
for (i in 1:(ncol(Imp0)-4)){
  p = which(top_500_total[,i+4]==1)
  RZ = mean(unlist(monthly_return[p,i+5]))
  y_here = as.vector(as.matrix(monthly_return[p,i+5]-RZ))
  x_here1 = (unlist((monthly_size[p,i+4])))
  x_here2 = (unlist(monthly_bm[p,i+4]))
  x_here3 = (unlist(monthly_beta[p,i+4]))
  x_here4 = (unlist(monthly_mm[p,i+4]))
  x_here5 = (unlist(monthly_ivol[p,i+4]))
  x_here6 = (unlist(monthly_smbbeta[p,i+4]))
  x_here7 = (unlist(monthly_hmlbeta[p,i+4]))
  x_here8 = (unlist(monthly_mombeta[p,i+4]))
  x_here9 = (unlist(abs(monthly_mm[p,i+4])))
  ep = (unlist(Imp0_p[p,i+4]))
  coverage = (unlist(Imp0_r[p,i+4]))
  x = as.matrix(cbind(x_here1,x_here2,x_here3,x_here9,x_here4,x_here5,x_here6,x_here7,
            x_here8,ep))
  lambdas <- 10^seq(2, -3, by = -.1)
  ridge_reg = cv.glmnet(x, y_here, nlambda = 25, alpha = 0, family = 'gaussian', 
                        type.measure = "mse", lambda = lambdas,intercept=FALSE,standardize = TRUE)
  test = coef(ridge_reg,s=c(ridge_reg$lambda.min))[2:11]
  coeff_lasso_EP = rbind(coeff_lasso_EP,test)
  r_test = c(r_test,max(1 - ridge_reg$cvm/var(y_here)))
  
  x = as.matrix(cbind(x_here1,x_here2,x_here3,x_here4,x_here5,x_here6,x_here7,
                      x_here8,x_here9,coverage))
  ridge_reg = cv.glmnet(x, y_here, nlambda = 25, alpha = 0.5, family = 'gaussian', 
                        type.measure = "mse",lambda = lambdas,intercept=FALSE,standardize = TRUE)
  coeff_lasso_COVERAGE = rbind(coeff_lasso_COVERAGE,coef(ridge_reg)[2:11])
}

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
months = as.Date(as.yearmon(months, "%Y%m"))
p1 = match(as.Date("1995-01-01"), months)
p2 = match(as.Date("2015-12-01"), months)
months = months[p1:p2]
months_crysis = which(RECESSION[p1:p2]==1)

pdf("lasso.pdf")
par(mfcol = c(2,1))
y = coeff_lasso_EP[p1:p2,10]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'EP coefficient')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(1))
abline(h=0, col=c("red"), lty=c(1), lwd=c(1))

y = coeff_lasso_COVERAGE[p1:p2,10]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'Coverage coefficient')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(1))
abline(h=0, col=c("red"), lty=c(1), lwd=c(1))

y = r_test[p1:p2]
plot(seq(1:length(months)), y, "l", col = 'black', xaxt='n', xlab = '', ylab = '',
     main = 'R-sqaured')
axis(1, at = seq(1:length(months))[seq(1, length(months), by=5)], format(months, "%b %Y")[seq(1, length(months), by=5)],las=2)
abline(v=months_crysis, col=c("grey"), lty=c(1), lwd=c(1))
abline(h=mean(y), col=c("red"), lty=c(1), lwd=c(1))
dev.off()

test = lm(coeff_lasso_EP[p1:p2,] ~ 1)
coeftest(test, vcov. = NeweyWest(test, lag = 3, prewhite = F, adjust = T, verbose = T))

test = lm(coeff_lasso_COVERAGE[p1:p2,] ~ 1)
coeftest(test, vcov. = NeweyWest(test, lag = 3, prewhite = F, adjust = T, verbose = T))

months_ep_negative = which(coeff_lasso_EP[,10]<0)

###### expand to top 1000-3000-5000 #######
top_500_total = read_csv("MktCap_Top5000_Unique(1981-2017)_MonthlyIndicator.csv")
Imp0 = read_csv("NYT_importance_0_Mkt5000.csv")
Imp0 = Imp0[complete.cases(Imp0), ]
top_500_total = top_500_total[complete.cases(top_500_total),]

monthly_return = read_csv("NYT_RET_Mkt5000.csv")
monthly_size = read_csv("NYT_SIZE_Mkt5000.csv")
monthly_bm = read_csv("NYT_BM_Mkt5000.csv")
monthly_beta = read_csv("NYT_BETA_Mkt5000.csv")
monthly_mm = read_csv("NYT_MOM3_Mkt5000.csv")
monthly_ivol = read_csv("NYT_IVOL_Mkt5000.csv")
monthly_smbbeta = read_csv("NYT_SMBBETA_Mkt5000.csv")
monthly_hmlbeta = read_csv("NYT_HMLBETA_Mkt5000.csv")
monthly_mombeta = read_csv("NYT_MOMBETA_Mkt5000.csv")

Imp0_p = Imp0
horizon = 3
for (m in 1:(ncol(Imp0)-4)) {
  if (m>=horizon){
    p = which(top_500_total[,m+4]==1)
    index_here = c((m-horizon+1):m)+4
    y_here = normalize(as.vector(as.matrix(log(1+Imp0[p,index_here]))))
    x_here1 = normalize(as.vector(as.matrix(log(1+monthly_size[p,index_here]))))
    x_here2 = normalize(as.vector(as.matrix(abs(monthly_mm[p,index_here]))))
    x_here3 = normalize(as.vector(as.matrix(monthly_ivol[p,index_here])))
    x_here4 = normalize(as.vector(as.matrix(monthly_bm[p,index_here])))
    x_here5 = normalize(as.vector(as.matrix(monthly_beta[p,index_here])))
    x_here6 = normalize(as.vector(as.matrix(monthly_smbbeta[p,index_here])))
    x_here7 = normalize(as.vector(as.matrix(monthly_hmlbeta[p,index_here])))
    x_here8 = normalize(as.vector(as.matrix(monthly_mombeta[p,index_here])))
    x_here9 = normalize(as.vector(as.matrix((monthly_mm[p,index_here]))))
    Imp0_p[p,m+4] = tail(coef_estimate[m,3]*x_here3 +
                           coef_estimate[m,9]*x_here9 +
                           coef_estimate[m,4]*x_here4 +
                           coef_estimate[m,7]*x_here7 +
                           coef_estimate[m,8]*x_here8 +
                           coef_estimate[m,6]*x_here6 +
                           coef_estimate[m,5]*x_here5 +
                           coef_estimate[m,2]*x_here2,length(p))
  }
}

strategy_cph = Imp0
strategy_cph[,5:ncol(Imp0)] = 0
strategy_ncph = strategy_ncpl = strategy_cpl = strategy_pl = strategy_ph = strategy_cph

for (i in 1:(ncol(Imp0)-4)){
  # non-covered
  p = which(top_500_total[,i+4]==1 & Imp0[,i+4] == 0)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_ncpl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_ncph[p1,i+4] = 1/length(p1)
  }
  
  # covered
  p = which(top_500_total[,i+4]==1 & Imp0[,i+4] != 0)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_cpl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_cph[p1,i+4] = 1/length(p1)
  }
  
  #pooled
  p = which(top_500_total[,i+4]==1)
  temp = quantile(unlist(Imp0_p[p,i+4]), probs = c(0.3,0.7), na.rm = TRUE)
  c1 = temp[1]
  c2 = temp[2]
  p1 = intersect(p,which(Imp0_p[,i+4]<=c1))
  if (length(p1)!=0){
    strategy_pl[p1,i+4] = 1/length(p1)
  }
  p1 = intersect(p,which(Imp0_p[,i+4]>=c2))
  if (length(p1)!=0){
    strategy_ph[p1,i+4] = 1/length(p1)
  }
}

rev_cph = port_return(monthly_return,strategy_cph,1)
rev_cpl = port_return(monthly_return,strategy_cpl,1)
rev_ncph = port_return(monthly_return,strategy_ncph,1)
rev_ncpl = port_return(monthly_return,strategy_ncpl,1)
rev_ph = port_return(monthly_return,strategy_ph,1)
rev_pl = port_return(monthly_return,strategy_pl,1)

months = as.character(colnames(monthly_return)[6:ncol(monthly_size)])
months = as.Date(as.yearmon(months, "%Y%m"))
p1 = match(as.Date("1995-01-01"), months)
p2 = match(as.Date("2015-12-01"), months)
months = months[p1:p2]
months_crysis = which(RECESSION[p1:p2]==1)

test = rev_cph - RF
regress1 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress1, vcov. = NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T))
temp1 = sqrt(diag(NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph - RF
regress2 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress2, vcov. = NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T))
temp2 = sqrt(diag(NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_cph - rev_cpl
regress3 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress3, vcov. = NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T))
temp3 = sqrt(diag(NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph - rev_ncpl
regress4 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress4, vcov. = NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T))
temp4 = sqrt(diag(NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_cph - rev_ncpl
regress5 = lm(test[p1:p2] ~ 1 + Mkt[p1:p2] + SMB[p1:p2] + HML[p1:p2] + Mom[p1:p2] + BAB[p1:p2])
coeftest(regress5, vcov. = NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T))
temp5 = sqrt(diag(NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T)))

panel = stargazer(regress1,regress2, regress3,regress4,regress5,
                  type="latex",
                  se = list(temp1,temp2,temp3,temp4,temp5),
                  dep.var.labels = '',
                  covariate.labels=c("Mkt-RF","SMB","HML","Mom","BAB"), 
                  star.cutoffs = c(0.1, 0.05, 0.01),digits = 4)

###### longer holding or news formation ######
for (i in 6:(ncol(Imp0)-4)){
  temp_here = Imp0[,i+4] + Imp0[,i+3] + Imp0[,i+2] + Imp0[,i+1] + Imp0[,i] + Imp0[,i-1]
  Imp0[,i+4] = temp_here/6
}


rev_cph = c(1)
rev_cpl = c(1)
rev_ncph = c(1)
rev_ncpl = c(1)
RF_temp = c(1)
Mkt_temp = c(1)
SMB_temp = c(1)
HML_temp = c(1)
Mom_temp = c(1)
BAB_temp = c(1)
for (i in 1:1){
  rev_cph = rev_cph*(1+port_return(monthly_return,strategy_cph,i))
  rev_cpl = rev_cpl*(1+port_return(monthly_return,strategy_cpl,i))
  rev_ncph = rev_ncph*(1+port_return(monthly_return,strategy_ncph,i))
  rev_ncpl = rev_ncpl*(1+port_return(monthly_return,strategy_ncpl,i))
  RF_temp = RF_temp*(1+RF[(p1+i-1):(p2+i-1)])
  Mkt_temp = Mkt_temp*(1+Mkt[(p1+i-1):(p2+i-1)])
  SMB_temp = SMB_temp*(1+SMB[(p1+i-1):(p2+i-1)])
  HML_temp = HML_temp*(1+HML[(p1+i-1):(p2+i-1)])
  Mom_temp = Mom_temp*(1+Mom[(p1+i-1):(p2+i-1)])
  BAB_temp = BAB_temp*(1+BAB[(p1+i-1):(p2+i-1)])
}
Mkt_temp = Mkt_temp-1
SMB_temp = SMB_temp-1
HML_temp = HML_temp-1
Mom_temp = Mom_temp-1
BAB_temp = BAB_temp-1
# rev_cph = port_return(monthly_return,strategy_cph,3)
# rev_cpl = port_return(monthly_return,strategy_cpl,3)
# rev_ncph = port_return(monthly_return,strategy_ncph,3)
# rev_ncpl = port_return(monthly_return,strategy_ncpl,3)

test = rev_cph[p1:p2] - RF_temp
regress1 = lm(test ~ 1 + Mkt_temp + SMB_temp + HML_temp + Mom_temp + BAB_temp)
coeftest(regress1, vcov. = NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T))
temp1 = sqrt(diag(NeweyWest(regress1, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = rev_ncph[p1:p2] - RF_temp
regress2 = lm(test ~ 1 + Mkt_temp + SMB_temp + HML_temp + Mom_temp + BAB_temp)
coeftest(regress2, vcov. = NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T))
temp2 = sqrt(diag(NeweyWest(regress2, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = (rev_cph - rev_cpl)[p1:p2]
regress3 = lm(test ~ 1 + Mkt_temp + SMB_temp + HML_temp + Mom_temp + BAB_temp)
coeftest(regress3, vcov. = NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T))
temp3 = sqrt(diag(NeweyWest(regress3, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = (rev_ncph - rev_ncpl)[p1:p2]
regress4 = lm(test ~ 1 + Mkt_temp + SMB_temp + HML_temp + Mom_temp + BAB_temp)
coeftest(regress4, vcov. = NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T))
temp4 = sqrt(diag(NeweyWest(regress4, lag = 3, prewhite = F, adjust = T, verbose = T)))

test = (rev_cph - rev_ncpl)[p1:p2]
regress5 = lm(test ~ 1 + Mkt_temp + SMB_temp + HML_temp + Mom_temp + BAB_temp)
coeftest(regress5, vcov. = NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T))
temp5 = sqrt(diag(NeweyWest(regress5, lag = 3, prewhite = F, adjust = T, verbose = T)))

panel = stargazer(regress1,regress2, regress3,regress4,regress5,
                  type="latex",
                  se = list(temp1,temp2,temp3,temp4,temp5),
                  dep.var.labels = '',
                  star.cutoffs = c(0.1, 0.05, 0.01),digits = 4)

residual = residuals(regress4)
mean((rev_ncph - rev_ncpl)[p1:p2])/sd((rev_ncph - rev_ncpl)[p1:p2])*sqrt(12)
(0.007+mean(residual))/sd(residual)*sqrt(12)

