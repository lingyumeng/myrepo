library(ggplot2)
data("diamonds")

dim(diamonds)
str(diamonds)
str(diamonds$color)
levels(diamonds$color)
summary(diamonds$color)
?diamonds

qplot(x = diamonds$price, data = diamonds)
ggplot(aes(x = price), data = diamonds) +
  geom_histogram(binwidth = 200) +
  scale_x_continuous(limits = c(300, 20000))

summary(diamonds$price)
# 钻石数量 less than $500
dim(subset(diamonds, diamonds$price < 500))[1]
sum(ifelse(diamonds$price < 500, 1, 0))
# 钻石数量 less than $250
dim(subset(diamonds, diamonds$price < 250))[1]
sum(ifelse(diamonds$price < 250, 1, 0))
# 钻石数量 equal or more than $15000
dim(subset(diamonds, diamonds$price >= 15000))[1]
sum(ifelse(diamonds$price >= 15000, 1, 0))

# 廉价钻石
# Explore the largest peak in the
# price histogram you created earlier.

# Try limiting the x-axis, altering the bin width,
# and setting different breaks on the x-axis.

# There won’t be a solution video for this
# question so go to the discussions to
# share your thoughts and discover
# what other people find.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Submit your final code when you are ready.

# TYPE YOUR CODE BELOW THE LINE
# ======================================================================
ggplot(aes(x = price), data = diamonds) +
  geom_histogram(binwidth = 200, color = '#093e32', fill = 'red') +
  scale_x_continuous(limits = c(300, 18000), breaks = seq(300, 18000, 1200))


ggplot(aes(x = log10(price)), data = diamonds) +
  geom_histogram( color = '#093e32', fill = 'green')

# 切工-价格直方图
# Break out the histogram of diamond prices by cut.
# TYPE YOUR CODE BELOW THE LINE
# ======================================================
qplot(x = price, data = diamonds, binwidth = 400,
      color = 'black', fill = '#4323ec3') +
  scale_x_continuous(limits = c(300, 16000), 
                     breaks = seq(300, 16000, 1000)) +
  facet_wrap(~cut, ncol = 3)


ggplot(aes(x = price), data = diamonds) +
  geom_histogram(binwidth = 400, color = "black", fill = "#099AB4") +
  scale_x_continuous(breaks = seq(300, 18000, 2000)) +
  facet_wrap(~cut, ncol = 2)

# 切工-价格  
by(diamonds$price, diamonds$cut, summary)
#标尺和多直方图
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free_y")
?facet_wrap
# 由切工决定的每克拉价格
?diamonds
qplot(x = log10(price/carat), data = diamonds, binwidth = 0.05,
      color = I('black'), fill = I('#89a34e')) +
  facet_wrap(~cut, scales = 'free_y', ncol = 3)

summary(diamonds$carat)
summary(diamonds$price)

library(gridExtra)
ggplot(aes(x = price/carat), data = diamonds) +
  geom_histogram(color = 'black', fill = '#7AC934') +
  scale_x_log10() +
  facet_wrap(~cut, scales = 'free_y', ncol = 3)

ggplot(aes(x = log10(price/carat)), data = diamonds) +
  geom_histogram(color = 'black', fill = '#7AC934') +
  facet_wrap(~cut, scales = 'free_y', ncol = 3)

#grid.arrange(p1,p2,p3, ncol =3)

?geom_histogram

# 价格箱线图
by(diamonds$price, diamonds$cut, summary)
ggplot(aes(x = cut, y = price), data = diamonds) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 8000))

by(diamonds$price, diamonds$clarity, summary)
ggplot(aes(x = clarity, y = price), data = diamonds) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 7000))

by(diamonds$price, diamonds$color, summary)
ggplot(aes(x = color, y = price), data = diamonds) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 8500))
?diamonds

# 四分位数间距 — IQR
by(diamonds$price, diamonds$color, summary)
IQR(subset(diamonds, color == 'D')$price)
IQR(subset(diamonds, color == 'J')$price)
IQR(diamonds$price)

# 由颜色表示的每克拉价格箱线图

by(diamonds$price/diamonds$carat, diamonds$color, summary)
ggplot(aes(x = color, y = price/carat), data = diamonds) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 6000))

# 克拉频率多边形

qplot(x = carat, data = diamonds, binwidth = 0.5, geom = 'freqpoly') +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0,5,0.5))

qplot(x = carat, data = diamonds, binwidth = 0.2, geom = 'freqpoly') +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0,3,0.2))

qplot(x = carat, data = diamonds, binwidth = 0.1, geom = 'freqpoly') +
  scale_x_continuous(limits = c(0, 2), breaks = seq(0,2,0.1)) +
  scale_y_continuous(breaks = seq(0, 10000, 1000))

# 用 R 进行数据整理
#install.packages("dplyr")
#install.packages("tidyr")

library(dplyr)
library(tidyr)

mtcars_df = tbl_df(mtcars)
str(mtcars_df)
print(mtcars_df)
utils::View(mtcars_df)
View(mtcars)
View(mtcars_df)
#按列对行进行排序，升序
dplyr::arrange(mtcars, mpg)
#按列对行进行排序，降序
dplyr::arrange(mtcars, desc(mpg))

#Rename the columns of a data frame
#dplyr::rename(tb, y = year)

dplyr::glimpse(iris)
str(iris)
utils::View(iris)

?iris
iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)

iris %>% select(Sepal.Length)

# Summarise data into single row of values
dplyr::summarise(iris, avg=mean(Sepal.Length))
# Apply summary function to each column. 
dplyr::summarise_each(iris, funs(mean))
# Count number of rows with each unique value of variable (with or without weights).
dplyr::count(iris, Species, wt=Sepal.Length) 

dplyr::summarise(iris, first_value = first(Sepal.Width))
dplyr::summarise(iris, last_value = last(Sepal.Width))
dplyr::summarise(iris, IQR_value = IQR(Sepal.Width))
dplyr::summarise(iris, min_value = min(Sepal.Width))
dplyr::summarise(iris, max_value = max(Sepal.Width))
dplyr::summarise(iris, median_v = median(Sepal.Width))
# 矢量的方差
dplyr::summarise(iris, Variance = var(Sepal.Width))
# 矢量的标准方差
dplyr::summarise(iris, SD = sd(Sepal.Width))
########################
# Group Data
# Group by
dplyr::group_by(iris, Species) %>%
  count(Species, wt=Sepal.Length)
# 等效于
dplyr::count(iris, Species, wt=Sepal.Length) 
#######################
# Remove grouping information from data frame. 
dplyr::ungroup(iris)
# Compute separate summary row for each group.
iris %>% group_by(Species) %>% summarise(var(Sepal.Width))

# Make New Variables
# Compute and append one or more new columns. 
iris_a = dplyr::mutate(iris, sepal=Sepal.Length+Sepal.Width)
dplyr::glimpse(iris_a)
utils::View(iris_a)
# Apply window function to each column
dplyr::mutate_each(iris, funs(min_rank))
# Compute one or more new columns. Drop original columns.
iris_d_a = dplyr::transmute(iris, sepal=Sepal.Length+Sepal.Width)
dplyr::glimpse(iris_d_a)
utils::View(iris_d_a)

# 提取符合逻辑标准的行
dplyr::filter(iris, Sepal.Length > 7)
# 删除重复的行。
dplyr::distinct(iris)
# 随机选择部分行
dplyr::sample_frac(iris, 0.5, replace=TRUE)
# 随机选择N行
dplyr::sample_n(iris, 10, replace=TRUE)
# 按位置选择行
dplyr::slice(iris, 10:15)
# 选择排序前n个条目（按分组数据分组）。
dplyr::top_n(storms, 2, date)
utils::View(storms)
# 按名称选择列
dplyr::select(iris, Sepal.Width, Petal.Length, Species)
# Select columns whose name contains a character string
dplyr::select(iris, contains("."))
# Select columns whose name ends with a character string.
dplyr::select(iris, ends_with("Length"))
# Select every column.
dplyr::select(iris, everything())
# Select columns whose name matches a regular expression.
dplyr::select(iris, matches(".t."))
# Select columns named x1, x2, x3, x4, x5.
dplyr::select(iris, num_range("x", 1:5))
# Select columns whose names are in a group of names.
dplyr::select(iris, one_of(c("Species", "Genus")))
# Select columns whose name starts with a character string.
dplyr::select(iris, starts_with("Sepal"))






# Tidy Data - A foundation for wrangling in R
#install.packages("devtools")
devtools::install_github("rstudio/EDAWR")
library(EDAWR)
?storms
?EDAWR::pollution
?cases
?pollution

# tidyr包基本操作
# 宽转长：gather()

# 使用gather()函数实现宽表转长表，语法如下：
# gather(data, key, value, …, na.rm = FALSE, convert = FALSE)
# data：需要被转换的宽形表
# key：将原数据框中的所有列赋给一个新变量key
# value：将原数据框中的所有值赋给一个新变量value
# …：可以指定哪些列聚到同一列中
# na.rm：是否删除缺失值

widedata <- data.frame(person = c('Alex', 'Bob', 'Cathy'), grade = c(2,3,4),
                       score = c(78,89,88))
print(widedata)
str(widedata)

longdata <- gather(widedata, variable, value, -person)
longdata  

cases
utils::View(cases)
longcases <- gather(cases, year, n, -country)
longcases
utils::View(longcases)

# 2:4要折叠的列的数字索引
longcases2 <- gather(cases, "year", "n", 2:4)
utils::View(longcases2)
# 2:4要折叠的列的名称
longcases3 <- gather(cases, "year", "n", "2011":"2013")
longcases3
# 长转宽：spread()
# 有时，为了满足建模或绘图的要求，往往需要将长形表转换为宽形表，
# 或将宽形表变为长形表。如何实现这两种数据表类型的转换。
# 使用spread()函数实现长表转宽表，语法如下：

# spread(data, key, value, fill = NA, convert = FALSE, drop = TRUE)
# data：为需要转换的长形表
# key：需要将变量值拓展为字段的变量
# value：需要分散的值
# fill：对于缺失值，可将fill的值赋值给被转型后的缺失值

mtcarsSpread <- mtcars %>% spread(attribute, value)


#### Gapminder 数据
getwd()
?read.csv
employs <- read.csv("AboveOf15employ.csv", header=T,row.names = 1, check.names=F)


utils::View(employs)
names(employs)
str(employs)
dim(employs)
employs <-  dplyr::select(employs, 1:17)
names(employs)
str(employs)
long_employs = tidyr::gather(employs, year, employment_rate)
utils::View(long_employs)
ggplot(data=long_employs, aes(x = employment_rate)) + 
  geom_histogram(color = 'black', fill = '#7AC934')

ggplot(data=long_employs, aes(x = employment_rate)) + 
  geom_histogram(color = 'black', fill = '#7AC934') +
  facet_wrap(~year, ncol = 4)

ggplot(data = long_employs, aes(x=year, y=employment_rate)) +
  geom_boxplot()

ggplot(data = long_employs, aes(x = employment_rate )) +
  geom_freqpoly(aes(color = year))
 

te = t(employs)
utils::View(te)
names(te)
str(te)





