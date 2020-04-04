library(MTS)
library(plotly)
library(rtweet)
library(TSstudio)
library(utils)
library(httr)
library(dynlm)
library(dlm)
library(xts)

rm(list = ls())


##########################


data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

dataAUS = subset(data, Country.Region == "Australia")
dataAUSsum = apply(dataAUS[, 5:dim(dataAUS)[2]], 2, sum)
dataAUSsum = dataAUSsum[5:length(dataAUSsum)]

daysAUS = length(dataAUSsum)
datesAUS = seq(as.Date("2020-01-26"), length = daysAUS, by = "days")

dataAUSsum_xts = xts(dataAUSsum, order.by = datesAUS)

dataAUSlog = log(dataAUSsum_xts)
dataAUSg = diff(dataAUSlog)

attr(dataAUSg, 'frequency') = 365.25
dataAUSg_ts = as.ts(dataAUSg)

# ts_plot(dataAUSsum_xts, title = "COVID-19 Cases in Australia")
# ts_plot(dataAUSlog, title = "COVID-19 Cases in Australia, Log Scale")
# ts_plot(dataAUSg, title = "Daily Growth rate in COVID-19 Cases in Australia")

g_est1 = mean(dataAUSg, na.rm = TRUE)

trunc_s = which("2020-02-29" == datesAUS)
dataAUSg_trunc = dataAUSg[trunc_s:length(dataAUSg)]
# ts_plot(dataAUSg_trunc)

g_est2 = mean(dataAUSg_trunc, na.rm = TRUE)

trunc_s2 = which("2020-03-22" == datesAUS)
dataAUSg_trunc2 = dataAUSg[trunc_s:trunc_s2]

g_est3 = mean(dataAUSg_trunc2)

dataAUSg_trunc3 = dataAUSg[trunc_s2:length(dataAUSg)]
g_est4 = mean(dataAUSg_trunc3)

g_est5 = dataAUSg[length(dataAUSg)]

print(cbind(g_est2,g_est3, g_est4, g_est5))

predHorizon = 7

logAusPred1 = rep(0, length(dataAUS) + predHorizon)
logAusPred2 = rep(0, length(dataAUS) + predHorizon)
logAusPred3 = rep(0, length(dataAUS) + predHorizon)


logAusPred1[1:length(dataAUSsum)] = dataAUSlog
logAusPred2[1:trunc_s2] = dataAUSlog[1:trunc_s2]
logAusPred3[1:length(dataAUSsum)] = dataAUSlog



for (j in (length(dataAUSsum) + 1):length(logAusPred1)) {

    logAusPred1[j] = logAusPred1[j-1] + g_est4

}

for (j in (length(dataAUSsum) + 1):length(logAusPred1)) {

    logAusPred3[j] = logAusPred3[j - 1] + g_est5

}

datesAUS_pred = seq(as.Date("2020-01-26"), length = length(logAusPred1), by = "days")
logAUSPred1_xts = xts(logAusPred1, order.by = datesAUS_pred)

# ts_plot(logAUSPred1_xts)

for (j in (trunc_s2 + 1):length(logAusPred2)) {

    logAusPred2[j] = logAusPred2[j - 1] + g_est3

}

logAUSPred2_xts = xts(logAusPred2, order.by = datesAUS_pred)

# ts_plot(logAUSPred2_xts)

join = cbind(logAusPred1, logAusPred3, logAusPred2)
join_xts = xts(join, order.by = datesAUS_pred)
names(join_xts) = c("Growth Rate of 12.34%", "Growth Rate of 4.10%", "Counterfactual - Growth Rate of 20.16%")
ts_plot(join_xts, title = "Effect of Social Distancing Measures announced 22/03 in AU (Log Scale)", type = "single")

# ts_plot(join_xts)

Pred = exp(join)
Pred_xts = xts(Pred, order.by = datesAUS_pred)

diff_policy = Pred[,2] - Pred[, 1]
# final = cbind(Pred, diff_policy)
final = Pred
final_xts = xts(final, order.by = datesAUS_pred)

names(final_xts) = c( "Social Distancing using Average Growth Rate", "Social Distancing using Latest Growth Rate", "No Social Distancing")

ts_plot(final_xts, title = "Effect of Social Distancing Measures announced 22/03 in AU", type = "single")

data_final = data.frame(datesAUS_pred,final)
