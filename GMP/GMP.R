library(data.table)

database_dt <- fread("cheese1.csv")

###Топ-10 производителей

db_pl_19 <- database_dt[Год_месяц %in% c("MAT FEB 2019")][,list(Год_месяц,Клиент,Подкатегория,СЕГМЕНТ,`Формат ТТ`,`Способ обработки`,
                                                             `Наименование Товара в Сети`,Производитель,Бренд,
                                                             `Тип упаковки`,Масса,Наполнитель,`Сквозное Имя`,
                                                    volume_share_19 = `Продажи в кг`/sum(`Продажи в кг`)*100,
                                                    value_share_19 = `Продажи в руб`/sum(`Продажи в руб`)*100,
                                                    offtake_19 = `Offtake в кг`)][Бренд %in% c("PL")]

db_pl_19_pivot <- dcast(db_pl_19, Производитель ~ Год_месяц, fun.aggregate = sum, 
                              value.var = c("volume_share_19", "value_share_19", "offtake_19"))

db_pl_19_aggregate <- db_pl_19_pivot[,list(Производитель = c("PRIVATE LABEL"),
                                                        volume_share_19 = sum(`volume_share_19_MAT FEB 2019`),
                                                        value_share_19 = sum(`value_share_19_MAT FEB 2019`),
                                                        offtake_19 = sum(`offtake_19_MAT FEB 2019`))]

db_pl_20 <- database_dt[Год_месяц %in% c("MAT FEB 2020")][,list(Год_месяц,Клиент,Подкатегория,СЕГМЕНТ,`Формат ТТ`,`Способ обработки`,
                                                                         `Наименование Товара в Сети`,Производитель,Бренд,
                                                                         `Тип упаковки`,Масса,Наполнитель,`Сквозное Имя`,
                                                                         volume_share_20 = `Продажи в кг`/sum(`Продажи в кг`)*100,
                                                                         value_share_20 = `Продажи в руб`/sum(`Продажи в руб`)*100,
                                                                         offtake_20 = `Offtake в кг`)][Бренд %in% c("PL")]

db_pl_20_pivot <- dcast(db_pl_20, Производитель ~ Год_месяц, fun.aggregate = sum, 
                        value.var = c("volume_share_20", "value_share_20", "offtake_20"))

db_pl_20_aggregate <- db_pl_20_pivot[,list(Производитель = c("PRIVATE LABEL"),
                                                        volume_share_20 = sum(`volume_share_20_MAT FEB 2020`),
                                                        value_share_20 = sum(`value_share_20_MAT FEB 2020`),
                                                        offtake_20 = sum(`offtake_20_MAT FEB 2020`))]

db_pl_aggregate <- db_pl_20_aggregate[db_pl_19_aggregate, on = .(Производитель=Производитель)]

db_other_19 <- database_dt[Год_месяц %in% c("MAT FEB 2019")][,list(Год_месяц,Клиент,Подкатегория,СЕГМЕНТ,`Формат ТТ`,`Способ обработки`,
                                                                         `Наименование Товара в Сети`,Производитель,Бренд,
                                                                         `Тип упаковки`,Масса,Наполнитель,`Сквозное Имя`,
                                                                         volume_share_19 = `Продажи в кг`/sum(`Продажи в кг`)*100,
                                                                         value_share_19 = `Продажи в руб`/sum(`Продажи в руб`)*100,
                                                                         offtake_19 = `Offtake в кг`)][!Бренд %in% c("PL")]

db_other_19_pivot <- dcast(db_other_19, Производитель ~ Год_месяц, fun.aggregate = sum, 
                        value.var = c("volume_share_19", "value_share_19", "offtake_19"))

db_other_20 <- database_dt[Год_месяц %in% c("MAT FEB 2020")][,list(Год_месяц,Клиент,Подкатегория,СЕГМЕНТ,`Формат ТТ`,`Способ обработки`,
                                                                            `Наименование Товара в Сети`,Производитель,Бренд,
                                                                            `Тип упаковки`,Масса,Наполнитель,`Сквозное Имя`,
                                                                            volume_share_20 = `Продажи в кг`/sum(`Продажи в кг`)*100,
                                                                            value_share_20 = `Продажи в руб`/sum(`Продажи в руб`)*100,
                                                                            offtake_20 = `Offtake в кг`)][!Бренд %in% c("PL")]

db_other_20_pivot <- dcast(db_other_20, Производитель ~ Год_месяц, fun.aggregate = sum, 
                           value.var = c("volume_share_20", "value_share_20", "offtake_20"))

db_other_aggregate <- db_other_20_pivot[db_other_19_pivot, on = .(Производитель=Производитель)]

setnames(db_other_aggregate, c("volume_share_20_MAT FEB 2020","value_share_20_MAT FEB 2020",
                               "offtake_20_MAT FEB 2020","volume_share_19_MAT FEB 2019",
                               "value_share_19_MAT FEB 2019","offtake_19_MAT FEB 2019"),
         c("volume_share_20","value_share_20","offtake_20","volume_share_19",
           "value_share_19","offtake_19"))

db_aggregate <- rbind(db_other_aggregate, db_pl_aggregate)

setorder(db_aggregate, volume_share_20)

db_aggregate_top10 <- db_aggregate[(nrow(db_aggregate)-9):nrow(db_aggregate),]

setorder(db_aggregate_top10, -volume_share_20)

db_aggregate_top10_right <- db_aggregate_top10[,list(Производитель,volume_share_20,volume_share_19,value_share_20,value_share_19,
                                      offtake_20,offtake_19,price_index_20=value_share_20/volume_share_20*100,
                                      price_index_19=value_share_19/volume_share_19*100)]

db_aggregate_top10_full <- db_aggregate_top10_right[,list(Производитель,volume_share_20,vs.YA1 = volume_share_20-volume_share_19,
                                      value_share_20,vs.YA2 = value_share_20-value_share_19,
                                      offtake_20,vs.YA3 = offtake_20-offtake_19,
                                      price_index_20,vs.YA4 = price_index_20-price_index_19)]
                                      
db_aggregate_top10_full

###Бренды топ-10 производителей

db_brands_19 <- database_dt[Год_месяц %in% c("MAT FEB 2019")][,list(Год_месяц, Производитель, Бренд, volume_share_19 = `Продажи в кг`/sum(`Продажи в кг`)*100,
                                                                         value_share_19 = `Продажи в руб`/sum(`Продажи в руб`)*100,
                                                                         offtake_19 = `Offtake в кг`)][Производитель %in% db_aggregate_top10_full$Производитель]

db_brands_19_pivot <- db_brands_19[order(Производитель),.(sum_volume_19 = sum(volume_share_19),
                                    sum_value_19 = sum(value_share_19),
                                    sum_offtake_19 = sum(offtake_19)), by = c('Производитель', 'Бренд')]

db_brands_20 <- database_dt[Год_месяц %in% c("MAT FEB 2020")][,list(Год_месяц, Производитель, Бренд, volume_share_20 = `Продажи в кг`/sum(`Продажи в кг`)*100,
                                                                             value_share_20 = `Продажи в руб`/sum(`Продажи в руб`)*100,
                                                                             offtake_20 = `Offtake в кг`)][Производитель %in% db_aggregate_top10_full$Производитель]

db_brands_20_pivot <- db_brands_20[order(Производитель),.(sum_volume_20 = sum(volume_share_20),
                                                          sum_value_20 = sum(value_share_20),
                                                          sum_offtake_20 = sum(offtake_20)), by = c('Производитель', 'Бренд')]

db_brands_pivot <- merge(db_brands_20_pivot, db_brands_19_pivot, all.x = T, all.y = T)

db_brands_right <- db_brands_pivot[,list(Производитель, Бренд, volume_share_20 = sum_volume_20, value_share_20 = sum_value_20,
                                   offtake_20 = sum_offtake_20, price_index_20 = sum_value_20/sum_volume_20*100,
                                   volume_share_19 = sum_volume_19, value_share_19 = sum_value_19,
                                   offtake_19 = sum_offtake_19, price_index_19 = sum_value_19/sum_volume_19*100)]

db_brands_full <- db_brands_right[,list(Производитель, Бренд, volume_share_20, vs.YA1 = volume_share_20-volume_share_19,
                                                     value_share_20, vs.YA2 = value_share_20-value_share_19,
                                                     offtake_20, vs.YA3 = offtake_20-offtake_19,
                                                     price_index_20, vs.YA4 = price_index_20-price_index_19)]



###Топ-20 СКЮ

db_sku_19 <- database_dt[Год_месяц %in% c("MAT FEB 2019")][,list(Производитель,Бренд,Масса,`Сквозное Имя`,
                                                                 volume_share_19 = `Продажи в кг`/sum(`Продажи в кг`)*100,
                                                                 value_share_19 = `Продажи в руб`/sum(`Продажи в руб`)*100,
                                                                 offtake_19 = `Offtake в кг`)][!Бренд %in% c("Не определен")]

db_sku_19_aggregate <- db_sku_19[,.(sum_volume_19 = sum(volume_share_19),
                                                           sum_value_19 = sum(value_share_19),
                                                           sum_offtake_19 = sum(offtake_19)), by = c('Производитель', 'Бренд', 'Масса', 'Сквозное Имя')]

db_sku_20 <- database_dt[Год_месяц %in% c("MAT FEB 2020")][,list(Производитель,Бренд,Масса,`Сквозное Имя`,
                                                                              volume_share_20 = `Продажи в кг`/sum(`Продажи в кг`)*100,
                                                                              value_share_20 = `Продажи в руб`/sum(`Продажи в руб`)*100,
                                                                              offtake_20 = `Offtake в кг`)][!Бренд %in% c("Не определен")]

db_sku_20_aggregate <- db_sku_20[,.(sum_volume_20 = sum(volume_share_20),
                                    sum_value_20 = sum(value_share_20),
                                    sum_offtake_20 = sum(offtake_20)), by = c('Производитель', 'Бренд', 'Масса', 'Сквозное Имя')]

merge(db_sku_20_aggregate, db_sku_19_aggregate, all = T)
