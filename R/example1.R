# 没安装learnasreml
# devtools::install_github("dengfei2013/learnasreml")

library(learnasreml)
data("NCII")
dat = NCII
str(dat)

# 收费软件解决方案
library(asreml)
mod = asreml(yield ~ Block, random = ~ P1 + P2 + Fam, data=dat)
summary(mod)$varcomp

# 广义遗传力
pin(mod, hb ~ (V1+V2+V3)/(V1+V2+V3+V4) )

# 狭义遗传力
pin(mod, h2 ~ (V1+V2)/(V1+V2+V3+V4))

# 配合力
coef(mod)$random

# 免费软件解决方案
library(sommer)
mod = mmer2(yield ~ Block, random = ~ P1 + P2 + Fam, data=dat)
summary(mod)
# 广义遗传力
pin(mod, hb ~ (V1+V2+V3)/(V1+V2+V3+V4) )
# 狭义遗传力
pin(mod, h2 ~ (V1+V2)/(V1+V2+V3+V4))
# 配合力
randef(mod)
