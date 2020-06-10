library(data.table)

database_dt <- fread("cheese1.csv")
database_dt_pivot <- dcast(database_dt, Производитель ~ Год_месяц, fun.aggregate = sum, value.var = c("Продажи в кг", "Продажи в руб", "Offtake в кг"))
setorder(database_dt_pivot, -"Продажи в кг_MAT FEB 2020")
database_top10man <- database_dt_pivot[,list(Производитель, volume_share_20 = `Продажи в кг_MAT FEB 2020`/sum(`Продажи в кг_MAT FEB 2020`)*100, 
                                     volume_share_19 = `Продажи в кг_MAT FEB 2019`/sum(`Продажи в кг_MAT FEB 2019`)*100,
                                     value_share_20 = `Продажи в руб_MAT FEB 2020`/sum(`Продажи в руб_MAT FEB 2020`)*100,
                                     value_share_19 = `Продажи в руб_MAT FEB 2019`/sum(`Продажи в руб_MAT FEB 2019`)*100,
                                     `Offtake в кг_MAT FEB 2020`, `Offtake в кг_MAT FEB 2019`)][1:10,]
db_top10 <- database_top10man[,list(Производитель, volume_share_20, vol_div = volume_share_20-volume_share_19,
                                     value_share_20, val_div = value_share_20-value_share_19,
                                     `Offtake в кг_MAT FEB 2020`, 
                                     off_div = `Offtake в кг_MAT FEB 2020`-`Offtake в кг_MAT FEB 2019`, digits = 1)]
round(db_top10[,2:ncol(db_top10)], digits = 1)

database_pl_19 <- database_dt[Бренд %in% c("PL")][Год_месяц %in% c("MAT FEB 2019")]

database_pl_20 <- database_dt[Бренд %in% c("PL")][Год_месяц %in% c("MAT FEB 2020")]

database_dpl19_pivot <- dcast(database_pl_19, Производитель ~ Год_месяц, fun.aggregate = sum, 
                              value.var = c("Продажи в кг", "Продажи в руб", "Offtake в кг"))

setorder(database_dpl19_pivot, -"Продажи в кг_MAT FEB 2019")

database_dpl20_pivot <- dcast(database_pl_20, Производитель ~ Год_месяц, fun.aggregate = sum, 
                              value.var = c("Продажи в кг", "Продажи в руб", "Offtake в кг"))

setorder(database_dpl20_pivot, -"Продажи в кг_MAT FEB 2020")

database_dpl19_pivot[,list(Производитель = c("Private label"), 
                                        volume_share_19 = sum(`Продажи в кг_MAT FEB 2019`),
                                        value_share_19 = sum(`Продажи в руб_MAT FEB 2019`),
                                        offtake_19 = sum(`Offtake в кг_MAT FEB 2019`))]

database_dpl20_pivot[,list(Производитель = c("Private label"), 
                                        volume_share_20 = sum(`Продажи в кг_MAT FEB 2020`),
                                        value_share_20 = sum(`Продажи в руб_MAT FEB 2020`),
                                        offtake_20 = sum(`Offtake в кг_MAT FEB 2020`))]
