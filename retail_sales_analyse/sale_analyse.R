library(dplyr)
library(data.table)
library(ggplot2)

sales <- fread("sale_analyse.csv", stringsAsFactors = T, dec = ",")
head(sales)
str(sales)

# ������� ������ ������� ������������
sales <- sales[,-6]

# ����������� ��������� ������������� ���������� � ���������
sales$`�������� ���������` <- as.factor(sales$`�������� ���������`)
sales$������� <- as.factor(sales$�������)

# ������� ������ ������� * � ������� �� ������� ������������
sales$������������ <- as.character(sales$������������)
s <- as.matrix(sales$������������)
s <- apply(s, 1, function(x) ifelse(grep("*", x)==1, gsub("[*]", " ",x), x))
s <- trimws(s, which = c("right"))
s <- trimws(s, which = c("left"))
s <- as.data.frame(s)
colnames(s) <- "������������"
sales <- cbind(sales, s)
sales <- sales[,-8]
sales <- sales[,c(1:7,20,8:19)]

# �������������� � dplyr
sales_dp <- as_tibble(sales)
sales_dp <- sales_dp %>% 
  filter(������� != "AUCHAN.UA-MAG") %>%
  mutate("����_�����" = `������������ � ��� (���.)`/sum(`������������ � ��� (���.)`,
                                                        na.rm = T)*100,
         "��������_������" = `���������� ������� (��.)` * `����� ������� (���.)`) %>% 
  rename("������������" = `�������� ������ ������������`, 
         "��������" = `������ ��������`,
         "���������" = `�������� ���������`,
         "������� GICA" = `������ �������� � GICA`)

# ��������� � ���������� ��������������
sales_dp %>% 
  group_by(������������) %>% 
  summarise("������������_���_���" = sum(`������������ � ��� (���.)`, na.rm = T) / 1000,
            "����������_������" = n(), 
            "����_�����" = sum(����_�����, na.rm = T),
            "�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100) %>% 
  arrange(desc(������������_���_���)) %>% 
  head(10)

sales_dp_cat <- sales_dp %>% 
  group_by(������������) %>% 
  summarise("������������_���_���" = sum(`������������ � ��� (���.)`, na.rm = T) / 1000,
            "����������_������" = n(), 
            "����_�����" = sum(����_�����, na.rm = T),
            "�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100) %>% 
  arrange(desc(������������_���_���)) %>% 
  head(10)

sales_dp_cat$������������ <- reorder(sales_dp_cat$������������, sales_dp_cat$������������_���_���, fun = max)

ggplot(sales_dp_cat, aes(x = ������������, y = ������������_���_���, fill = ������������_���_���))+
  geom_col()+
  xlab("�������� ���������")+
  ylab("������������ (���. ���.)")+
  labs(fill = "������������ (���. ���.)")+
  ggtitle("���-10 ��������� �� �������������")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.75)))

# �������� ����������� ���������
sales_dp %>% 
  group_by(������������) %>% 
  summarise("����������_������" = n(), 
            "������������_���_���" = sum(`������������ � ��� (���.)`, na.rm = T) / 1000,
            "����_�����" = sum(����_�����, na.rm = T),
            "�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100) %>% 
  arrange(desc(����������_������)) %>% 
  head(10)

sales_dp_sal <- sales_dp %>% 
  group_by(������������) %>% 
  summarise("����������_������" = n(), 
            "������������_���_���" = sum(`������������ � ��� (���.)`, na.rm = T) / 1000,
            "����_�����" = sum(����_�����, na.rm = T),
            "�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100) %>% 
  arrange(desc(����������_������)) %>% 
  head(10)

sales_dp_sal$������������ <- reorder(sales_dp_sal$������������, sales_dp_sal$����������_������, fun = max)

ggplot(sales_dp_sal, aes(x = ������������, y = ����������_������, fill = ����������_������))+
  geom_col()+
  xlab("�������� ���������")+
  ylab("���������� ������")+
  labs(fill = "���������� ������")+
  ggtitle("���-10 ��������� �� ���������� ������")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))

# �������� ������������ ���������
sales_dp %>% 
  group_by(������������) %>% 
  summarise("�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100,
            "����������_������" = n(), 
            "������������_���_���" = sum(`������������ � ��� (���.)`, na.rm = T) / 1000,
            "����_�����" = sum(����_�����, na.rm = T)) %>% 
  arrange(desc(`�������_�����_%`)) %>% 
  head(10)

sales_dp_mar <- sales_dp %>% 
  group_by(������������) %>% 
  summarise("�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100,
    "����������_������" = n(), 
            "������������_���_���" = sum(`������������ � ��� (���.)`, na.rm = T) / 1000,
            "����_�����" = sum(����_�����, na.rm = T)) %>% 
  arrange(desc(`�������_�����_%`)) %>% 
  head(10)

sales_dp_mar$������������ <- reorder(sales_dp_mar$������������, sales_dp_mar$`�������_�����_%`, fun = max)

ggplot(sales_dp_mar, aes(x = ������������, y = `�������_�����_%`, fill = `�������_�����_%`))+
  geom_col()+
  xlab("�������� ���������")+
  ylab("������� ��������������, %")+
  labs(fill = "������� ��������������, %")+
  ggtitle("���-10 ��������� �� ������� ��������������")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))

#�������������� ����
sales_dp_cor <- sales_dp %>% 
  group_by(������������) %>% 
  summarise("�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100,
            "����������_������" = n()) %>% 
  arrange(desc(`�������_�����_%`))

cor.test(sales_dp_cor$`�������_�����_%`, sales_dp_cor$����������_������)

# ������� �� ��������
sales_dp %>% 
  group_by(������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����������_������" = n(), 
            "�������_���" = (sum(`������������ � ��� (���.)`, 
                                na.rm = T)) / n()) %>% 
  arrange(desc(������������_���))

sales_dp_per <- sales_dp %>% 
  group_by(������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����������_������" = n(), 
            "�������_���" = (sum(`������������ � ��� (���.)`, 
                                 na.rm = T)) / n()) %>% 
  arrange(desc(������������_���))

sales_dp_per$������ <- reorder(sales_dp_per$������, sales_dp_per$������������_���, fun = max)

ggplot(sales_dp_per, aes(x = ������, y = ������������_���, fill = ������))+
  geom_col(width = 0.5)+
  xlab("������")+
  ylab("������������ (���.)")+
  labs(fill = "������")+
  ggtitle("��������� �� ��������")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))

# ������� �� ���������
sales_dp %>% 
  group_by(�������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                                 na.rm = T),
            "����_�����" = sum(����_�����, na.rm = T)) %>% 
  arrange(desc(������������_���))

sales_dp_shop <- sales_dp %>% 
  group_by(�������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����_�����" = sum(����_�����, na.rm = T)) %>% 
  arrange(desc(������������_���))

sales_dp_shop$����_����� <- round(sales_dp_shop$����_�����, digits = 2)
sales_dp_shop$������������_���� <- cumsum(sales_dp_shop$����_�����)
sales_dp_shop$ymin <- c(0, head(sales_dp_shop$������������_����, n=-1))
sales_dp_shop$labelPosition <- (sales_dp_shop$������������_���� + sales_dp_shop$ymin) / 2
sales_dp_shop$label <- paste0(sales_dp_shop$�������, "\n ����: ", sales_dp_shop$����_�����, "%")

ggplot(sales_dp_shop, aes(ymax=������������_����, ymin=ymin, xmax=4, xmin=3, fill=�������)) +
  geom_rect() +
  geom_text( x=2.25, aes(y=labelPosition, label=label, color=�������), size=3.5) + # x here controls label position (inner / outer)
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  ggtitle("���� ��������� � ���������� �������������")+
  theme(legend.position = "none")

# ������ ��������� "��������" � "��������"
sales_dt <- data.table(sales_dp)
sales_dt[, ������������ := as.character(������������)]
sales_dt_v <- sales_dt[������������ == "��������",]
sales_dt_p <- sales_dt[������������ == "������Ͳ",]
sales_dp_pv <- as_tibble(rbind(sales_dt_p, sales_dt_v))
sales_dp_v <- as_tibble(sales_dt_v)
sales_dp_p <- as_tibble(sales_dt_p)
sales_dp_pv <- mutate_each(sales_dp_pv, funs(as.factor(.)), ������������)
sales_dp_v <- mutate_each(sales_dp_v, funs(as.factor(.)), ������������)
sales_dp_p <- mutate_each(sales_dp_p, funs(as.factor(.)), ������������)

sales_dp_pv %>% 
  group_by(������������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����_�����" = sum(����_�����, na.rm = T),
            "����������_������" = n(),
            "�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100) %>%
  arrange(desc(������������_���))

sales_dp_pv_d <- sales_dp_pv %>% 
  group_by(������������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����_�����" = sum(����_�����, na.rm = T),
            "����������_������" = n(),
            "�������_�����_%" = mean(`����� FIFO`, na.rm = T) * 100) %>%
  arrange(desc(������������_���))

ggplot(sales_dp_pv_d, aes(x = ������������, y = ������������_���, fill = ������������))+
  geom_col(width = 0.5)+
  xlab("���������")+
  ylab("������������ (���.)")+
  labs(fill = "���������")+
  ggtitle("��������� ���������")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))

sales_dp_p %>% 
  group_by(������������, ������������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����_�����" = sum(����_�����, na.rm = T)) %>%
  arrange(desc(������������_���)) %>% 
  top_n(10, ����_�����)

sales_dp_p_d <- sales_dp_p %>% 
  group_by(������������, ������������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����_�����" = sum(����_�����, na.rm = T)) %>%
  arrange(desc(������������_���)) %>% 
  top_n(10, ����_�����)

sales_dp_p_d$������������ <- reorder(sales_dp_p_d$������������, sales_dp_p_d$������������_���, fun = max)

ggplot(sales_dp_p_d, aes(x = ������������, y = ������������_���, fill = ������������_���))+
  geom_col()+
  xlab("������������")+
  ylab("������������ (���.)")+
  labs(fill = "������������ (���.)")+
  ggtitle("���-10 ������� � ��������� ��������")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.8)))

sales_dp_v %>% 
  group_by(������������, ������������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����_�����" = sum(����_�����, na.rm = T)) %>%
  arrange(desc(������������_���)) %>% 
  top_n(10, ����_�����)

sales_dp_v_d <- sales_dp_v %>% 
  group_by(������������, ������������) %>% 
  summarise("������������_���" = sum(`������������ � ��� (���.)`, 
                                     na.rm = T),
            "����_�����" = sum(����_�����, na.rm = T)) %>%
  arrange(desc(������������_���)) %>% 
  top_n(10, ����_�����)

sales_dp_v_d$������������ <- reorder(sales_dp_v_d$������������, sales_dp_v_d$������������_���, fun = max)

ggplot(sales_dp_v_d, aes(x = ������������, y = ������������_���, fill = ������������_���))+
  geom_col()+
  xlab("������������")+
  ylab("������������ (���.)")+
  labs(fill = "������������ (���.)")+
  ggtitle("���-10 ������� � ��������� ��������")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)))



# ������������� ������

sales_dp_regr <- sales_dp[,c(16,18,22)]

sales_dp
sales_dp_regr <- sales_dp %>% 
  group_by(�������, ������������) %>% 
  summarise("������������" = sum(`������������ � ��� (���.)`, na.rm = T),
            "������" = sum(��������_������, na.rm = T),
            "���������_��������������" = median(`����� FIFO`, na.rm = T))
            
filter(sales_dp_regr, funs(is.na(.)))


fit <- lm(������������ ~ ., sales_dp_regr)
summary(fit)
step(fit, direction = "backward")
pairs(sales_dp[,c(16,17,18,22)])
