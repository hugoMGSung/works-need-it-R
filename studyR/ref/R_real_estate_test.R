getwd()

load(file='./result_sales_dt.RData')

## 패키지
library(data.table)
library(dplyr)
install.packages('ggplot2')
library(ggplot2)
install.packages('lubridate')
library(lubridate)
install.packages('stringr')
library(stringr)

install.packages('forecast')
library(forecast)
install.packages('randtests')
library(randtests)
install.packages('snpar')

## 데이터 검색
glimpse(result_sales_dt, width=60)

#월별 지역별 매매량
region_cnts <- result_sales_dt[,.N,.(yyyymm,region)]

#대표지역 추출
regions <- unique(region_cnts$region)

#각 지역별로 매매량의 랜덤성 검정 결과를 runs_p 변수에 추가
runs_p <- c()
for(reg in regions){
  runs_p <- c(runs_p, runs.test(region_cnts[region %chin% reg,N])$p.value)
}

runs_p
ggplot(data.table(regions, runs_p), 
       aes(x=regions, y=runs_p, group=1)) +
          geom_line() + geom_point() +
          ylab('P-value') + xlab('지역')

## 시계열분할(서울 지역)
seoul_cnts <- result_sales_dt[yyyymm != '201504' &
                                region %chin% '서울',.N,.(yyyymm)]
seoul_cnts

tot_ts <- ts(seoul_cnts$N,start = c(2006,1), frequency = 12)
plot(stl(tot_ts,s.window = 'periodic'))
