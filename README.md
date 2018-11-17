# learnasreml

这个软件包, 主要是学习asreml的使用说明包, 包括相关的代码, 数据和模型操作.

### 上手指南
这是一个R包，所以能按照R语言的平台，都可以安装learnasreml包。该包在windows10和Centos系统下测试。

### 安装步骤

```
if (!requireNamespace("devtools")) install.packages("devtools")

library(devtools)

install_github("dengfei2013/learnasreml")
```

### 测试

```
data("animalmodel.dat")

head(animalmodel.dat)

data("animalmodel.ped")

head(animalmodel.ped)
```

### 作者

姓名： 邓飞

邮箱：dengfei_2013@163.com

### 版本说明
使用RStudio进行版本更新。

### 鸣谢

该项目参考了林元震老师的AAfun包

该项目参考了nadiv包

该项目参考了VSNC公司的VSNR包

特此鸣谢。
