# G20峰会环境规制对杭州市空气质量的影响
——基于断点回归设计

***
## 摘要
本文利用杭州市2016年的空气质量数据和气象数据，运用断点回归设计，对G20峰会期间环境规制对杭州市空气质量的影响进行分析。首先运用普通最小二乘法显示环境规制有很强的效果，在运用断点回归解决内生性问题后，研究结果表明，环境规制政策的实施使杭州市的空气质量指数发生显著降低，在改变带宽及拟合方法的情况下的估计结果也较为稳健。并由此得出建议：承办城市要结合各产业的废气来源及生活的方方面面展开环境规制政策，加大力度，提高政策执行效率，以便能在短时间内在空气质量治理方面产生显著的效果。

***
## 模型构建
- 普通最小二乘法
- 断点回归设计

***
## 数据来源
- 中国空气质量在线监测分析平台（https://www.aqistudy.cn/historydata/）
- WEATHER UNDERGROUND（https://www.wunderground.com/）

***
## 变量选取
- 被解释变量：杭州市2016年第t星期某项空气质量指标的数值
         （空气质量指标：杭州市空气质量指数（AQI）及各污染物（PM2.5,PM10,CO,S02,N02）浓度值）
- 控制变量：平均气温、平均湿度、平均气压以及平均风速

***
## 建模语言
- Stata
- R
