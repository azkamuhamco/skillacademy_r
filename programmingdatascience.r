"     Soal dari Skill Academy Ruangguru
      Course: Programming Foundation for Data Science
      Unduh file dataset csv di: https://tinyurl.com/assessmentrnpy 
      Coding from Azka Muhammad Al Fisyahri (@azkamuhamco). Selesai pada Kamis, 2 April 2020       "

#Jalankan script install packages jika belum install packages
#install.packages(dplyr)
#install.packages(lubridate)
#install.packages("ggplot2")
#install.packages("hablar")

library(dplyr)      # untuk select dan filter tabel csv
library(lubridate)  # untuk konversi date
library(ggplot2)    # untuk menampilkan scatter plot
library(hablar)     # untuk konversi jenis variabel

dataset <- read.csv('Dataset_superstore_simple.csv')  # Sumber data
View(dataset)

#1. Carilah customer_id yang memiliki sales paling besar!
no_1 <- dataset %>% 
          group_by(customer_id) %>%
          summarise(total_sales = sum(sales)) %>%
          arrange(desc(total_sales))
View(no_1)

"2. Sub-category apa saja yang ada di dalam category ‘Office Supplies’, dan
masing-masing berapa banyak total profitnya?"
no_2 <- dataset %>%
          filter(category=='Office Supplies') %>%
          group_by(sub_category) %>%
          arrange(sub_category) %>%
          summarise(total_profit = sum(profit))
View(no_2)

#3. Berapa banyak order yang men  ghasilkan profit negatif (rugi)?      filter(profit<0) %>% 
t_3a <- dataset %>% group_by(order_id) %>% summarise(total_profit = sum(profit))
t_3 <- t_3a %>% filter(total_profit<0)
no_3 <- count(t_3)
View(no_3)

"4. Antara 3 customer_id ini, mana yang total sales-nya paling banyak: JE-16165,
KH-16510, AD-10180?"
no_4 <- dataset %>% 
          group_by(customer_id) %>%
          summarise(total_sales = sum(sales)) %>%
          arrange(desc(total_sales)) %>% 
          filter(customer_id=='JE-16165' | customer_id=='KH-16510' | customer_id=='AD-10180') 
View(no_4)

"5. Buatlah data frame bernama ‘yearly_sales’ yang berisi total sales, jumlah
customers, dan total profit tiap tahun. Tahun berapa profit tertinggi diperoleh?"
yearly_sales <- dataset %>%
                      group_by(order_year = year(order_date)) %>%
                      summarise(total_cust = n_distinct(customer_id), 
                                total_sales = sum(sales), total_profit = sum(profit)) %>%
                      arrange(desc(total_profit))
View(yearly_sales)

"6. Buatlah scatterplot antara sales dan profit untuk data di tahun 2014-2015 saja, bedakan
warnanya antara tahun 2014 dan tahun 2015. Beri judul ‘Sales vs Profit 2014-2015’!"
tno_6 <- dataset %>%
            select(c(order_date, sales, profit)) %>%
            mutate(tahun=year(order_date)) %>%
            convert(chr(tahun)) %>%
            filter(as.Date(order_date)>='2014-01-01' & as.Date(order_date)<'2016-01-01') %>%
            group_by(order_date, tahun) %>%
            summarise(total_sales = sum(sales), total_profit = sum(profit))

ggplot(tno_6, aes(x=total_sales, y=total_profit)) + 
  geom_point(aes(color=tahun), size = 3, shape = 18) + 
  geom_smooth(method = 'lm') + labs(title='Sales vs Profit 2014-2015') +
  theme(
    legend.position = c(0.9,0.1),
    legend.title = element_text(color = 'blue', face='bold')
  )

"7. Buatlah barchart yang berisi total profit dari 10 customer dengan total sales 
tertinggi di tahun 2015!"
t7 <- dataset %>%
  filter(as.Date(order_date)>='2015-01-01' & as.Date(order_date)<'2016-01-01') %>%
  group_by(customer_id) %>%
  summarise(total_profit = sum(profit), total_sales = sum(sales)) %>%
  arrange(customer_id)
tno_7 <- subset(t7, total_sales %in% head(unique(total_sales),10))

ggplot(tno_7, aes(x = customer_id, y = total_profit)) + 
  geom_bar(stat = 'identity', width = 0.75, fill='blue')
