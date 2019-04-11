library(rdrobust)         
air<- read.csv("D:/rdd.csv")
air

#用R计算AQI，R的带宽是计算出来的最优带宽，stata的是1.2倍的最优带宽（=R最优），R用的robust，结果和直接使用rd出来的标准差是一样的，所以stata做rd时出来的就是稳健标准误。

Lee_2 <- lee[lee$margin >= -0.5 & lee$margin <= 0.5,]

# Figure 9.12 using data Lee_2 in Page218 结果变量与参考变量关系图，看出在断点处有明显跳跃
rdplot(y = air$aqi, x = air$d,       # Defult is p=4 
       col.lines ="black",
       x.label = 'Election t',
       y.label = 'AQI, Election t+1')

#stata对参考变量进行McCrary检验，跳跃在断点处不显著，说明个体没有精确控制断点的能力
#DCdensity d,breakpoint(0) generate(Xj Yj r0 fhat se_fhat) graphname(rd.> eps)

# Figure 9.8 in Page 206 and code edited by stata in Page219. 最优带宽选取
rdbwselect_2014(y = Lee_2$vote, x = Lee_2$margin, c=0, 
                kernel = "uniform", bwselect = "CV", 
                cvgrid_min = 0.1, cvgrid_max = 0.5, cvplot = TRUE)
# use rdbwselect_2014 rather rdbwselect
# Result shows the best bindwith is 0.28

# Replicat the Result in Page220限制在最优带宽内，分别进行局部平均法和局部线性回归法
Lee_1 <- lee[lee$margin >= -0.28 & lee$margin <= 0.28,]
Lee_1 
Lee_1$dd <- ifelse(Lee_1$margin>0,Lee_1$d <- 1,Lee_1$d <- 0)   # Creat Rdd
Lee_1$dd
summary(lm(vote~dd,data=Lee_1))                            #Result of Colume1
summary(lm(vote~dd+margin+I(dd*margin),data=Lee_1))        #Result in Colume2

# Replicat the Result in Page221使用全部样本，重新估计，通过建立局部多项式选择最优模型
Lee_2 <- lee[lee$margin >= -0.5 & lee$margin <= 0.5,]
air$dd <- ifelse(air$d>=0,air$x<- 1,Lee_2$x<- 0)   # Creat Rdd
summary(lm(aqi~dd+d+I(dd*d)+temp+humi+press+wind,data=air))            # Result of Colume1
AIC(lm(vote~dd+margin+I(dd*margin),data=Lee_2))
BIC(lm(vote~dd+margin+I(dd*margin),data=Lee_2))
summary(lm(vote~dd+margin+I(margin^2)+I(dd*margin)+I(dd*margin^2),data=Lee_2))  # Result of Colume2
AIC(lm(vote~dd+margin+I(margin^2)+I(dd*margin)+I(dd*margin^2),data=Lee_2))
BIC(lm(vote~dd+margin+I(margin^2)+I(dd*margin)+I(dd*margin^2),data=Lee_2))
summary(lm(vote~dd+margin+I(margin^2)+I(margin^3)+I(dd*margin)+I(dd*margin^2)+I(dd*margin^3),data=Lee_2))  # Result of Colume3
AIC(lm(vote~dd+margin+I(margin^2)+I(margin^3)+I(dd*margin)+I(dd*margin^2)+I(dd*margin^3),data=Lee_2))
BIC(lm(vote~dd+margin+I(margin^2)+I(margin^3)+I(dd*margin)+I(dd*margin^2)+I(dd*margin^3),data=Lee_2))
summary(lm(vote~dd+margin+I(margin^2)+I(margin^3)+I(dd*margin)+I(dd*margin^2)+I(dd*margin^3)+I(dd*margin^4)+I(dd*margin^4),data=Lee_2))  # Result of Colume4
AIC(lm(vote~dd+margin+I(margin^2)+I(margin^3)+I(dd*margin)+I(dd*margin^2)+I(dd*margin^3)+I(dd*margin^4)+I(dd*margin^4),data=Lee_2))
BIC(lm(vote~dd+margin+I(margin^2)+I(margin^3)+I(dd*margin)+I(dd*margin^2)+I(dd*margin^3)+I(dd*margin^4)+I(dd*margin^4),data=Lee_2))


