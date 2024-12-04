# ---------------- R Script for Credit Scorecards Development
# ---------------- Product - Retail: M1 - Employee

# Подгружаем библиотеки в текущую сессию

library(openxlsx,  quietly = TRUE, warn.conflicts = FALSE) # Импорт и экспорт Эксель файлов
library(dplyr) # Удобное средство работы с таблицами, аналог SQL
library(sqldf) # SQL запросы
library(stats) # Статистические функции
library(data.table) # Преобразование данных в формат Таблица из Дата Фрейма. Требуется для некоторых других библиотек. Например, scorecard работает с таблицами

# Установка пакета - используется при первом запуске скрипта. Вместо scorecard подставляется любое название библиотеки в случае необходимости, например, ошибка "Нет такое библиотеки"
# install.packages("scorecard", dependencies = TRUE)

# Настройки форматов шрифтов, времени, валют и прочего для RU сегмента. Требуется, например, чтобы корректно читалась кириллица
Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")

# Учтанавливаем рабочую директорию. Пример для MacOS. В Windows - копируем путь.
setwd("/Users/user/Docs DO/My/Extra-Consulting/Eskhata/Retail")

# Загружаем данные из файлов Эксель.
# В данном случае используется один файл с данными со структурой, переданной от клиента. Файл содержит 3 листа. Параметр sheet = указывает, с какого именно листа считывать данные
data_Loan <- read.xlsx("Retail Data Sample - 170823 - for R.xlsx", sheet = 1)
data_App <- read.xlsx("Retail Data Sample - 170823 - for R.xlsx", sheet = 2)
data_Beh <- read.xlsx("Retail Data Sample - 170823 - for R.xlsx", sheet = 3)

# Проверяем визуально корректность импорта данных.
# Договора, заявки, поведение
str(data_Loan)
str(data_App)
str(data_Beh)
# View(data)

# Аггрегируем тра источника данных с одну таблицу с помощью оператора из библиотеки dplyr.
# Объединение left_join эквивалентно левому объединению LEFT JOIN в SQL. Мы присоединяем К договорам заявки, затем - поведенческие данные
RES <- data_Loan %>% left_join(data_App, by = c("Account.ID" = "Account.ID")) %>% left_join(data_Beh, by = c("Account.ID" = "Account.ID")) %>% as.data.frame


# View(RES)
# str(RES)

# Проверяем наличие дублей.
# При запуске вывод должен быть равен 0
sum(duplicated(RES$Account.ID))
sum(duplicated(RES$Customer.ID.x))

# Если есть дубли - выводим по каким именно клиентам.
# RES  %>% group_by(Customer.ID.x) %>% summarise(count = n()) %>% as.data.frame()
# ИЛИ
# sqldf("select [Customer.ID.x], count(*) from RES group by [Customer.ID.x]")

# RES[duplicated(RES$Account.ID)|duplicated(RES$Account.ID, fromLast=TRUE),]


# Присваиваем data frame RES объекту df. 
# Можно сразу объединять таблицы в df. Здесь такое дублирование предложено, поскольку далее производятся модификации данных в таблице, и в случае необходимости отката проще обратиться к загруженному объекту RES, чем перегружать данные снова.
df <- RES

# Данные переменные загрузились не с числовом формате. Преобразовываем в Numeric.
df$Date.of.birth <- as.numeric(df$Date.of.birth)
df$Net.main.income <- as.numeric(df$Net.main.income)
df$Additional.income  <- as.numeric(df$Additional.income)
df$Months.with.bank  <- as.numeric(df$Months.with.bank)
df$Reported.expenses <- as.numeric(df$Reported.expenses)
df$Deposit <- as.numeric(df$Deposit)
df$Current.exposure <- as.numeric(df$Current.exposure)

# Проблема чтения даты из формата Эксель. Необходимо следующее преобразование
df$Application.date <- as.Date(df$Application.date, origin = "1899-12-30")
df$Date.of.birth <- as.Date(df$Date.of.birth, origin = "1899-12-30")

# Названия переменных содержат специальный символ в Эксель файле. Вручную переназываем поля для корректной обработки в дальнейшем
df$Dependants <- df$`#.Dependants`
df$Months.at.current.address <- df$`#.months.at.current.address`
df$Months.at.job <- df$`#.months.at.job`

# Количество строк в таблице df
nrow(df)
ncol(df)

#  Feature Engineering


# Расчитываем возраст как разницу между датой рождения и датой заявки.
# Библиотека lubridate содердит функцию time_length для расчета разницы между датами в днях, месяцах, годах. В данном случае используем параметр "years".
library(lubridate)
df$Age = floor(time_length(difftime(df$Application.date, df$Date.of.birth), "years" ))

# Проверяем распределение по возрасту на логику
# df %>% group_by(Age) %>% summarise(count = n())

# Создаем поле LoanMonth - Год и Месяц договора для удобства обработки по периодам. Например, 202105, 202106.
df$LoanMonth <- year(df$Application.date)*100 + month(df$Application.date)


# Создаем переменные на основе других переменных.
# Логическая переменная CityOfLivingEqRegistration - принимает значение 1, если город проживание совпадает с городом регистрации, и 0 в противном случае 
df$CityOfLivingEqRegistration <- ifelse(as.character(df$City.of.Living) == as.character(df$City.of.registration), 1, 0)
# Логическая переменная IsCollateral - принимает значение 0, если поле Collateral.type не заполнено (нет залога), в противном случае 1
df$IsCollateral <- ifelse(is.na(df$Collateral.type),0,1)

# --- Create Behavioural Features - New/Exist and Previous DPD -----------
# Создаем переменные на основе поведения клиента по предыдущим договорам

# sqldf("select count(*) from (select distinct [Customer.ID.x] from df) t1")

# Создаем рабочую таблицу: связка договор - все предыдущие договора клиента
temp_df <- 
  sqldf("select t1.[Account.ID] as [Account.ID], 
              t1.[Customer.ID.x] as [Customer.ID.x], 
              t1.[Application.date] as [Application.date], 
              t2.[Customer.ID.x] as CustID_2, 
              t2.[Application.date] as AppDate_2,
              t2.[Maximum.days.past.due.lifetime]
      from df t1 
      left join df t2 
      on t1.[Customer.ID.x] =  t2.[Customer.ID.x] and t1.[Application.date] >  t2.[Application.date]
      order by t1.[Customer.ID.x]")

nrow(df)
nrow(temp_df)
# View(temp_df)

# Расчитываем переменные Максимальное количество дней в просрочке по предыдущим договорам и количество предыдущих договоров. Если текущий договор первый - возвращаем NULL
Acc_Numb_before <-
  sqldf("select t1.[Account.ID], count(*) as cnt, max([Maximum.days.past.due.lifetime]) as Max_MaxDaysPastDue
      from temp_df t1
      where CustID_2 is not null
      group by t1.[Account.ID]
      order by t1.[Account.ID]
      ")

# View(Acc_Numb_before)
# Создаем переменную "Существующий клиент". Если нет договоров до текущего договора - клиент новый, ExistingClient = 0
Acc_Numb_before$ExistingClient <- ifelse(Acc_Numb_before$cnt>1, 1, 0)

# Присоединяем к основной таблице. Как и ранее - через служебную таблицу, чтобы не перезатирать
df_new <-
  sqldf("select t1.*, t2.cnt as PreviousLoans_Count, t2.Max_MaxDaysPastDue as PreviousLoans_MaxDPD
from df as t1 left join Acc_Numb_before as t2 on t1.[Account.ID] = t2.[Account.ID]")

# Проверяем кол-во строк. Не должно увеличиться
nrow(df_new)
# View(df_new)

# Создаем переменную Кол-во предыдущих договоров
df_new$PreviousLoans_Count <- ifelse(is.na(df_new$PreviousLoans_Count), 0, df_new$PreviousLoans_Count)

# Присваиваем в df объединенную таблицу
df <- df_new

# Целевые переменные
# Создаем целевые переменные GB_ на основе счетчиков дней просрочки Maximum.days.past.due.lifetime и CumulativeDelinquency, и 
# Если количество дней просрочки превышает пороговое значение - ставим 1 (плохой договор, событие), в противном случае - 0
df$GB_90Ever <- ifelse(df$Maximum.days.past.due.lifetime >= 90, 1, 0)
df$GB_CumDlq90 <- ifelse(df$CumulativeDelinquency >= 90, 1, 0)
df$GB_60Ever <- ifelse(df$Maximum.days.past.due.lifetime >= 60, 1, 0)


# Data Investigation - Исследование и анализ качества данных и распределений
# Пример распределений по числовым характеристикам
summary(df$Net.main.income)
summary(df$Age)

# > summary(df$Net.main.income)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#        0     1950     2787     5173     4000 90044313       36 
# > summary(df$Age)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -1.00   29.00   37.00   38.83   48.00   94.00       1 

# Пример распределений по категориальным характеристикам
summary(df$Marital.status)

# > summary(df$Marital.status)
# Вдова      Женат    Замужем Не замужем   Разведен  разведена   Сожитель     Холост       NA's 
#       8112      99528      79208      14012       3571       4504        226      26262        275 
#       

# Пример использование функции describe из пакета scorecard
library(scorecard)
descr_df <- describe(df)


# 
# Экспортируем результаты распределений по всем переменным в файл descr_df_all.xlsx
# write.xlsx(descr_df, "descr_df_all.xlsx")

#  --------- Replace char with factor
# Рабочий момент для библиотеки scorecard. Чтобы использовать категориальные переменные, имеющие тип character сейчас, нужно преоборазовать их в тип factor

for (col in names(df))  
  if(class(df[,col])[1] == "character")
    df[,col] <- as.factor(df[,col])

str(df) 
names(df)


# Убираем ненужные переменные
# Создаем список переменных, которые в дальнейшем используются для анализа
# Переменные, которые были убраны сразу, согласно логике. Например, Branch.ID, Product.ID - нам не нужны ID, Date.of.birth - мы уже использовали дату рождения, City.of.Living - используем регион,
# Переменные, которые были убраны после анализа по технически причинам. Например, Source.of.additional.income - очень много значений, необходимо использовать справочник классификации, но нет фактов, подтверждающих, ценность переменной. Employment.sector - используем сегмент.
include_vars_1 <-
  c("Customer.ID.x",
    "Account.ID",
    # "Branch.ID",
    # "Product.ID",
    "Loan.Amount",
    # "Collateral.type",
    "IsCollateral",
    "Salary.payment.in.bank.account",
    # "Date.of.birth",
    "Age",
    "Gender",
    # "City.of.Living",
    "Region.of.living",
    # "City.of.registration",
    "Region.of.registration",
    "CityOfLivingEqRegistration",
    "Education",
    "Marital.status",
    "Dependants",
    "Months.at.current.address",
    "Employment.type",
    # "Employment.sector",
    "Employment.segment",
    "Months.at.job",
    "Net.main.income",
    "Source.of.main.income",
    "Additional.income",
    # "Source.of.additional.income",
    "Reported.expenses",
    "Months.with.bank",
    # "Current.exposure",
    "Client.type",
    "Property.object",
    "Eskhata.Online",
    "Plastic.Cards",
    "Deposit",
    "GB_90Ever",
    "GB_CumDlq90",
    "GB_60Ever",
    "BKI.Rating",
    "BKI.Number.of.Loans",
    "LoanMonth",
    "PreviousLoans_Count",
    "PreviousLoans_MaxDPD"
  )

# Оставляем только нужные переменные для дальнейшего анализа
df2 <- df[,names(df) %in% include_vars_1]

df <- df2

# Обработка общего набора данных закончена


# Отчеты по распределениям целевой переменной

# Выводим общее количество наблюдений, количество "плохих", и процент плохих от общего количества в таблице df, сгруппированное по месяцу выдачи
# Для критерия "плохой": Maximum.days.past.due.lifetime >= 90
repGB_90Ever_Month <-
  df %>%
  group_by(LoanMonth)  %>%
  summarise(count = n(), bad_num = sum(GB_90Ever), bad_rate = sum(GB_90Ever)/n()) %>% as.data.frame()

# Для критерия "плохой": CumulativeDelinquency >= 90
repGB_CumDlq90_Month <-
  df %>%
  group_by(LoanMonth)  %>%
  summarise(count = n(), bad_num = sum(GB_CumDlq90), bad_rate = sum(GB_CumDlq90)/n()) %>% as.data.frame()

# Для критерия "плохой": Maximum.days.past.due.lifetime >= 60
repGB_60Ever_Month <-
  df %>%
  group_by(LoanMonth)  %>%
  summarise(count = n(), bad_num = sum(GB_60Ever), bad_rate = sum(GB_60Ever)/n()) %>% as.data.frame()
 
nrow(sample_Empl)

# Экспортируем в Эксель файлы
# write.xlsx(repGB_90Ever_Month, "repGB_90Ever_Month.xlsx")
# write.xlsx(repGB_CumDlq90_Month, "repGB_CumDlq90_Month.xlsx")
# write.xlsx(repGB_60Ever_Month, "repGB_60Ever_Month.xlsx")



# df <- as.data.frame(data)



# View(df2)

#  ------------- Select Product Segment Here!!!!!! -------------------

# Разделяем выборку данных на три подвыборки согласно сегментам по Типу занятости: "Работает в организации", "Собственный бизнес", "Имеет другой источник дохода".
# Для каждого из трех сегментов обучается и внедряется своя скоринговая модель


sample_Empl <- df[df$Employment.type == "Работает в организации"
                  &
                    df$Employment.segment %in%
                    c("Мед. работник",
                      "Работник в сфере образования",
                      "Работник госструктур",
                      "Работник НПО (Ташкилоти Чамъияти)",
                      "Работник производства",
                      "Работник сельского хозяйство",
                      "Работник частной организации",
                      "Строитель",
                      "Экономист") 
                  ,]

sample_Bus <- df[df$Employment.type == "Собственный бизнес"
                 &
                   df$Employment.segment %in%
                   c("Агро",
                     "Производство",
                     "Торговля",
                     "Услуги",
                     "Услуги Мастера",
                     "Услуги транспорта")
                 ,]

sample_Other <- df[df$Employment.type == "Имеет другой источник дохода"
                   &
                     is.na(df$Employment.segment),]

# unique(sample_Other$Employment.segment)

nrow(sample_Empl)
nrow(sample_Bus)
nrow(sample_Other)



# --------- Start with Scoring Development ---------------
# --------- Create Data Sample ---------------------------

# sample <- data.frame(df[, names(df) %in% include_vars_1])

# ---------- SELECT the SEGMENT ------------------------------

# Выбираем выборку для анализа
# В дальнейшем - процесс и логика построения скоринговой модели, элементы скрипта остаются теми же самыми для каждой из моделей. 
# Меняются - наборы перменных, отобранные в соответствии с их уровнем значимости, результаты биннинга переменных, значения коэффициентов и наборы предикторов, вошедших в модели, значения метрик валидации моделей

# Демонстрируем процесс построения на примере 1-ой выборки - Сотрудники организаций.
# Sample - название объекта дата фрейм, который в дальнейшем используется как входной набор данных для анализа. Присваиваем ему выборку sample_Empl

sample <- sample_Empl
# sample <- sample_Bus
# sample <- sample_Other

str(sample)

# dev <- sample[sample$LoanMonth >=  202110 & sample$LoanMonth < 202210,]
# oot <- sample[sample$LoanMonth >=  202210 & sample$LoanMonth < 202303,]
# oot2 <- sample[sample$LoanMonth >=  202207 & sample$LoanMonth < 202210,]

# sample %>% group_by(LoanMonth) %>% summarise(count = n()) %>% as.data.frame()
# sample %>% filter(LoanMonth >= 202210 & LoanMonth < 202303) %>% group_by(LoanMonth) %>% summarise(count = n()) %>% as.data.frame()


#  -------------------- Target Definition ------------------------------------
# Проведем анализ распределения целевой переменной по месяцам выдачи кредита. 
# На основе анализа определим, 
# 1) какую из целевых характеристик: Max DPD 90+, Max DPD 60+, или Cumulative Delinquency > 90 будем использовать как определение дефолта для модели
# 2) какие периоды выберем для обучающей и тестовых выборок.
# Критерий для выбора целевой характеристики: количество событий (плохих договоров) должно быть достаточным для построения модели. Под достаточностью понимаются количество и процент от общего количества. 
# Например, 20-30 плохих договоров - явно нерепрезентативно для любого объема выборки. 100 - возможно, но для большого количества наблюдений (например, 10 тыс. договоров) дает низкий уровень дефолта и становится сложным либо нерепрезентативным расчет соотношения хороших и плохих наблюдений для отдельных бинов (сегментов значений предикторов). Например, 50 к 500 (~10%) является статистичеки значимым соотношением, но 5 к 50 не явлюяется статистически значимым и дает высокое отклонение (чувствительность) в оценке отношения при изменении количесва наблюдений: +1 = 6/50=0.12, -1 = 4/50=0.08. 
# Чем большее количество "плохих" кейсов, тем большее количество сегментов мы можем использовать. Например, для 50 плохих кейсов разбиение на 3 сегмента может дать 30,15,5, где 5 уже малорепрезентативно, и рекомендуется сократить кол-во сегментов до 2-х.
# Критерий для выбора месяца разбиения на обучающую и тестовую (Out-of-time) выборки: 
# а) в тестовой выборке должно быть достаочное количество наблюдений с событием 
# б) количество наблюдений должно быть репрезентативным 
# в) период для тестовой выборки должен включать самые актуальные, последние договора.


repGB_90Ever_Month <-
  sample %>%
  group_by(LoanMonth)  %>%
  summarise(count = n(), bad_num = sum(GB_90Ever), bad_rate = sum(GB_90Ever)/n()) %>% as.data.frame()

repGB_CumDlq90_Month <-
  sample %>%
  group_by(LoanMonth)  %>%
  summarise(count = n(), bad_num = sum(GB_CumDlq90), bad_rate = sum(GB_CumDlq90)/n()) %>% as.data.frame()

repGB_60Ever_Month <-
  sample %>%
  group_by(LoanMonth)  %>%
  summarise(count = n(), bad_num = sum(GB_60Ever), bad_rate = sum(GB_60Ever)/n()) %>% as.data.frame()

# Выбираем вариант GB_60Ever_Month - паритет достаточного кол-ва событий и привязанности критерия к индикатору плохого кредита для отчетности, резервирования

# write.xlsx(repGB_90Ever_Month, "M1_Employee_repGB_90Ever_Month.xlsx")
# write.xlsx(repGB_CumDlq90_Month, "M1_Employee_repGB_CumDlq90_Month.xlsx")
# write.xlsx(repGB_60Ever_Month, "M1_Employee_repGB_60Ever_Month.xlsx")

# Создаем переменную GB и присваиваем ей значения из GB_60Ever. В дальнейшем можем провести подмену на другую целевую переменную, оставив весь дальнейший алгоритм без изменения
sample$GB <- sample$GB_60Ever
# sample$GB <- sample$GB_CumDlq90


# ------- Sampling ---------------------------------------------------

# Семплинг - создание выборок для обучения и тестирования
# Development Sample - выборка, на которой будем обучать модель
dev <- sample %>% filter(LoanMonth >= 202110 & LoanMonth < 202210) %>% mutate(LoanMonth = NULL) %>% as.data.frame() 
# Out-of-time - выборка, на которой будем тестировать модель на временную устойчивость и способность прогнозировать на последних доступных периодах
oot <- sample %>% filter(LoanMonth >= 202210 & LoanMonth < 202303) %>% as.data.frame()
# Out-of-time 2 - выборка, на которой будем тестировать модель на временную устойчивость и способность прогнозировать на периоде внутри обучающей выборки. Фактически, реализация кросс-валидации в ручном режиме. Можем менять диапазоны для этого периода множество раз
oot2 <- sample %>% filter(LoanMonth >= 202207 & LoanMonth < 202210) %>% as.data.frame()
# Development Sample - выборка, на которой будем обучать финальную модель для внедрения на продакшн ПОСЛЕ тестирования на временную устойчивость. Делается для того, чтобы не терять информацию из out-of-time выборки, и добавить ее в модель
dev_final <- sample %>% filter(LoanMonth >= 202110 & LoanMonth < 202302) %>% mutate(LoanMonth = NULL) %>% as.data.frame() 

# [Изучаем распределения]
dev %>% summarise(count = n(), bad_num = sum(GB_60Ever), bad_rate = sum(GB_60Ever)/n()) %>% as.data.frame()
oot %>% summarise(count = n(), bad_num = sum(GB_60Ever), bad_rate = sum(GB_60Ever)/n()) %>% as.data.frame()
oot2 %>% summarise(count = n(), bad_num = sum(GB_60Ever), bad_rate = sum(GB_60Ever)/n()) %>% as.data.frame()
dev_final %>% summarise(count = n(), bad_num = sum(GB_60Ever), bad_rate = sum(GB_60Ever)/n()) %>% as.data.frame()


# str(dev)
# str(oot)
# str(oot2)

# dev <- sample

nrow(dev)
nrow(oot)
nrow(oot2)
nrow(dev_final)

#  ------------ Start with Scoring --------------

# Начинаем посторение скоринговой модели
# Подключаем библиотеку

library(scorecard)

# Траннсформируем выборки в тип объекта "таблица" (это нужно для использования в пакете scorecard)
dev <- data.table(dev)
oot <- data.table(oot)
oot2 <- data.table(oot2)
dev_final <- data.table(dev_final)

# Расчитываем метрику Information Value на первоначальных значениях перменных. Цель - отсеять то, что априори не является значимым
# Функция iv возвразает таблицу со списком предикторов и значениями Information Value для целевой GB.
# iv_values <- iv(dev, y = "GB")
iv_values_final <- iv(dev_final, y = "GB")

# Отбрасываем переменные со значением информационной значимости менее 0.01 и количеством пропущенных занчений более 95%
# Create table for Scoring - DROP oV < 0.02 and Missing > 0.8
dt_sel = var_filter(dev, "GB", iv_limit = 0.01, missing_limit = 0.95, 
                     var_rm = c("Customer.ID.x","Account.ID", "GB_90Ever", "GB_60Ever", "GB_CumDlq90", "Current.exposure"))

dev_final_sel = var_filter(dev_final, "GB", iv_limit = 0.01, missing_limit = 0.95, 
                    var_rm = c("Customer.ID.x","Account.ID", "GB_90Ever", "GB_60Ever", "GB_CumDlq90", "Current.exposure"))

# names(dev_final_sel)

# View(dt_sel)

# bins5 <- woebin(dt_sel,
#                 y="GB",
#                 method = "tree",
#                 # breaks_list = breaks,
#                 # save_breaks_list = "break_list_woe5",
#                 count_distr_limit = 0.05,
#                 bin_num_limit = 5
# )

# unique(dev_final_sel$Marital.status)
# dev_final_sel$Employment.segment

# Проводим трансформацию переменных в категориальный формат - биннинг (cource classing)
# Применяется алгоритм оптимального разбиения на классы с ограничением по указанным критериям. Критерий оптимизации хорошо работате для интервальных переменных, но не является оптимальным для категориальных переменных.
# Под оптимальным разбиением подразумевается найти такие диапазоны И количество классов, которые давали бы максимальное IV переменной с учетом ограничений на кол-во бинов и значимость
# breaks = list - создаем список и определение значений бинов для переменных, разбиваемых вручную

breaks = list(
  Education = c("Высшее%,%Ученая степень%,%2 и более высших",
                "Среднее%,%Начальное",
                "Среднее специальное",
                "Неоконченное высшее"),
  Region.of.living  = c(
    "Вилояти Сугд", 
    "Вилояти Хатлон%,%ВМКБ",
    "Душанбе", 
    "Нохияхои тобеи Чумхури"),
  Marital.status  = c(
  "Женат%,%Сожитель",
  "Замужем%,%Вдова%,%разведена",
  "Холост", 
  "Не замужем%,%Разведен"
  ),
  Employment.segment = c(
    "Мед. работник%,%Работник в сфере образования%,%Экономист%,%Работник НПО (Ташкилоти Чамъияти)",
    "Работник госструктур%,%Строитель",
    "Работник производства%,%Работник сельского хозяйство",
    "Работник частной организации"),
  Source.of.main.income = c(
    "Доход семьи",
    "Зарплата по основному месту работы",
    "Предпринимательство",
    "Пенсия%,%Алименты%,%Прочее%,%missing"
  ),
  Months.at.job = c(31,61,145,277),
  Months.with.bank = c(10,25,79),
  Net.main.income = c("missing%,%1600",3400)
)

# unique(dev_final$Source.of.main.income)

# Запускаем алгоритм биннинга и расчтеа Woe (Weight of Evidence) для каждой переменной в выборке по целевой GB.
# Используем следующие настройки:
# method = "tree", (Метод оптимизации биннинга, возможно использовать Chi-squared)
# breaks_list = breaks, (подключаем списко отработки вручную)
# # save_breaks_list = "break_list_woe5", (можем сохранить диапазоны разбиения в отдельном файле)
# check_cate_num = FALSE, (проверять кол-во разных значений в категориальных переменных. Например, если более 50 разных значений, как-то города, можно пропустить для экономии времени)
# count_distr_limit = 0.05, (Доля минимальное количество наблюдений в бине от общего количества наблюдений. В данном случае, размер бина не может быть менее 5% от популяции)
# bin_num_limit = 5 (Максимальное количество бинов. В данном случае, мы используем не более 5. Для данной выборки, дефолт рейта и кол-ва плохих кейсов может использоваться и меньшее число. Это делат модель менее точной, НО более стабильной)

# Цель - заменить оригинальные переменные на значения WoE. Так мы получаем:
# 1) возможность работать только с категориальным переменными, но подавать на вход модели числовое значение,являющееся мерой отклонения соотношения "Хороших" и "плохих" наблюдений в бине от соотношения "Хороших" и "плохих" наблюдений в выборке в целом
# 2) избавляет от необходимости объяснения возможных нелинейных трендов. Линейная модель логистическая получает возможность обрабатывать нелинейные зависимости.
# 3) избавляет от необходимости обработки пропущенных занчений и выбросов (если это не было сделано отдельно специльным образом согласно бизнес-логике). Возможно работать с категорией пропущенных значений ка с отдельной категорией, либо включить ее наиболее подходящий бин

bins_final5_M1_Employee <- woebin(dev_final_sel,
                y="GB",
                method = "tree",
                breaks_list = breaks,
                # save_breaks_list = "break_list_woe5",
                check_cate_num = FALSE,
                count_distr_limit = 0.05,
                bin_num_limit = 5
)


# bins_final5_M1_Employee$Employment.segment
# bins_final5_M1_Employee$Months.with.bank
# bins_final5_M1_Employee$Months.at.job
# bins_final5_M1_Employee$Net.main.income
# bins_final5_M1_Employee$Loan.Amount
# bins_final5_M1_Employee$Source.of.main.income

# dev_final_sel %>% group_by(Employment.sector) %>% summarise(count = n(), bad_num = sum(GB), bad_rate = sum(GB)/n()) %>% as.data.frame()
# dev_final_sel %>% group_by(Employment.segment) %>% summarise(count = n()) %>% as.data.frame()

# Результат экспортируем в Эксель файл
write.xlsx(bins_final5_M1_Employee, "M1_Employee_bins5.xlsx")

woebin_plot(bins_final5_M1_Employee)

# ПРисваиваем результат объекту bins для дальнейшего использования. Переприсваивание используется для того, чтобы была возможность опробовать несколько вариантов разбиения, но не перезаписывать бины
bins <- bins_final5_M1_Employee


# Разбиение тренинговой выборки на 2 семпла: Train и Test (Out-of-sample) для анализа структуной устойчивост модели.
# Здесь применябтся пропорции 1:2, 0.67 к 0.33. seed - используется для генератора псевдослучайных чисел, может быть любое число
dt_list = split_df(dt_sel, y="GB", ratio = 0.67, seed = 30)
# Получили объект типа List, присваеваем двум таблицам подвыборки - train и test
train = dt_list$train;
test = dt_list$test;

nrow(train)
nrow(test)


# Применяем полученные бины и значения WoE для всех выборок. 
# Будем использовать значения WoE каждого диапазано категориальных переменных как предикторы модели.
# Фактически мы заменяем интервальные (числовые) переменные как то возраст, доход и прочее на категории "от - до" для ухода от нелинейных и неустойчивых из-за разного кол-ва наблюдений зависимостей между предиктором и ln(p/1-p) , а категориальные группируем в более крупные группы для устойчивости и статистической значимости.

dev_woe <- woebin_ply(dt_sel,bins)
oot_woe <- woebin_ply(oot,bins)
oot2_woe <- woebin_ply(oot2,bins)

train_woe <- woebin_ply(train,bins)
test_woe <- woebin_ply(test,bins)

# Для филнального полного обучающего семпла
dev_final_woe <- woebin_ply(dev_final_sel,bins)
# Для всего семпла
sample_woe <- woebin_ply(sample,bins)

# Рассчитываем Information Value для преобразованных переменных. Рельный IV для предикторов в модели.

# iv_values_woe <- iv(dev_woe, y = "GB")
iv_values__final_woe <- iv(dev_final_woe, y = "GB")

# Можем отбросить переменные с низким и очень высоким IV в автоматическом режиме
# cov_names <- c(iv_values[iv_values$info_value < 0.8 & iv_values$info_value > 0.015, c("variable")])

# View(iv_values__final_woe)

write.xlsx(iv_values__final_woe, "M1_Employee_iv_values_woe_5.xlsx")


# Проверка корреляции
#Correlation
corM <- cor(as.matrix(dev_final_woe))
corM <- as.data.frame(corM)
# View(corM)
write.xlsx(corM, "M1_Employee_corrM.xlsx")

# install.packages("corrplot")
# library(corrplot)
# corrplot(corM)
# # 
# corrplot(corM,
#          method = "color",
#          addCoef.col = "white",
#          #col = coll(20),
#          number.cex = 0.01,
#          tl.col = rgb(1,33,105,maxColorValue = 255)
# )


# Обучение и выбор модели

# Применяем следующий подход.
# Общий набор данных dev_final разделен на следующие выборки:
# Обучающая выборка dev, состоит из тренинговой (train) и тестовой структурной (test out-of sample).
# Тестровая временная выборка (test out-of-time).
# 
# dev = train + test (out_of_sample)
# dev_final = dev + oot

# Моделирование и выбор модели проводим по следующему алгоритму:
# 1. Запуск логистической регресси со всеми предикторами на выборке train
# 2. Запуск метода stepwise для отсеивания незначимых переменных
# 3. Отбор оставшихся параметров в ручном режиме с пересчетом параметров валидации для каждой итерации (каждого нового набора переменных) на выборках train, test, oot. 
# AUC не должен отличаться существенно для train, test, oot. Убираем незначимые, слабозначимые, переменные с Pr(>|z|) > 0.01 до тех пор, пока:
# 1) AUC train не сблизится с AUC oot, НО 
# 2) AUC train и AUC oot не начнут значимо падать.
# Такой баланс удастся соблюсти итеративным опытным путем. Процесс относительно творческий, как и биннинг переменных, поэтому единого детерминированного подхода "как получить самую лучшую модель" нет. Есть баланс между точностью и стабильностью.
# Получена стабильнная наилучшая модель на тренинговой выборке и оттестирована подвыборке из другого временного промежутка. Теперь нужно использовать максимально данные выбрки для включения самых последних данных. Добавляем OOT в обучающую выборку.
# 4. Запуск логистической регрессии со всеми предикторами на выборке dev_final.  
# 5. Запуск метода stepwise для отсеивания незначимых переменных.
# 6. Отбор оставшихся параметров в ручном режиме с пересчетом параметров валидации для каждой итерации (каждого нового набора переменных) на выборках dev_final, oot, oot2 (выборка из начального временного интервала). Таким образом мы смотрим, как модель построенная на общем прездказывает на частных подвыборках. 
# 7. Сравниваем набор парметров из модели TRAIN с набором парметров из модели DEV_FINAL. Наборы не должны отличаться, либо отличаться на 1-2 переменные, включение или исключение которых мы доожны проанализировать и объяснить. Такая разница может свидетельствовать о временной неустойчивости прогнозной силы и/или значимости отдельных переменных. 
# 8. Финализируем модель на выборке DEV_FINAL.

#  RUN Logistic Regression
# With Train Sample - (Dev * 0.67)

# Запускаем логистическую регрессию

# ln(p/1-p) = b0 + b1*x1 + b2*x2 + ... + bn*xn
# p = 1/(1+exp(-1*(b0 + b1*x1 + b2*x2 + ... + bn*xn)))
# p = (b0 + b1*x1 + b2*x2 + ... + bn*xn)/(1+exp(b0 + b1*x1 + b2*x2 + ... + bn*xn))

# Этап 1. Для всех переменных

formula <- GB ~ .

m1_train <- glm(formula, data = train_woe, family = binomial)
summary(m1_train)


# Этап 2.
# Применяем метод Stepwise для отбора значимых переменных в логистической регрессии. 
# Сущестует три метода автоматического отбора параметров: forward, backward, stepwise. 
# Forward - в модель добавляются по одному предикторы до тех пор, пока критерий, используемый при решении оптимизационной задачи (AIC), не перестает увеличиваться. 
# Backward использует обратный подход - в модель подаются все переменные, далее убираются по одной до тех пор, пока критерий оптимизации не начинает уменьшаться. 
# Метод Stepwise использует смешанный подход - добавление сильных и удаление слабых переменных, и дает наиболее оптимальный набор предикторов.  
# Может быть ресурсоемким. Для выборки на 100 тыс наблюдений и 100 предикторов может занять до нескольких часов.

library(MASS)
#Stepwise Logistic regression
m_step_train <- stepAIC(m1_train, direction = "both")
# Выводим результат - оставшиеся коэффициенты модели
m2_train <- eval(m_step_train$call)

summary(m2_train)

# Выводим формулу, чтобы скопировать ее ниже И скорректировать - убрать или добавить - предикторы в ручном режиме
m2_train$formula

# Формула первичная после обработкой методом Stepwise
final_formula_train <- 
  GB ~ 
  Loan.Amount_woe + 
  Salary.payment.in.bank.account_woe + 
  Gender_woe +
  Region.of.living_woe + 
  Education_woe + 
  Marital.status_woe + 
  Employment.segment_woe + 
  Net.main.income_woe + 
  Source.of.main.income_woe +
  Additional.income_woe +
  Reported.expenses_woe +
  Months.with.bank_woe + 
  Client.type_woe +
  Property.object_woe +
  Eskhata.Online_woe +
  BKI.Rating_woe + 
  Dependants_woe + 
  Months.at.job_woe + 
  Age_woe + 
  PreviousLoans_Count_woe +
  PreviousLoans_MaxDPD_woe

final_formula_train <- 
  GB ~ 
  Loan.Amount_woe + 
  Salary.payment.in.bank.account_woe + 
  # Gender_woe + 
  Region.of.living_woe + 
  Education_woe + 
  Marital.status_woe + 
  Employment.segment_woe + 
  Net.main.income_woe + 
  # Source.of.main.income_woe + 
  # Additional.income_woe + 
  # Reported.expenses_woe + 
  Months.with.bank_woe + 
  # Client.type_woe + 
  # Property.object_woe + 
  # Eskhata.Online_woe + 
  BKI.Rating_woe + 
  Dependants_woe + 
  Months.at.job_woe + 
  Age_woe + 
  # PreviousLoans_Count_woe + 
  PreviousLoans_MaxDPD_woe

#//
# Call:
  # glm(formula = final_formula_train, family = binomial, data = train_woe)

# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -3.84547    0.06192 -62.099  < 2e-16 ***
# Loan.Amount_woe                     1.63782    0.19572   8.368  < 2e-16 ***
# Salary.payment.in.bank.account_woe  0.65425    0.20150   3.247  0.00117 ** 
# Gender_woe                          0.28114    0.24648   1.141  0.25402    
# Region.of.living_woe                0.29800    0.15699   1.898  0.05766 .  
# Education_woe                       0.39868    0.16459   2.422  0.01542 *  
# Marital.status_woe                  0.37347    0.11590   3.222  0.00127 ** 
# Employment.segment_woe              0.40481    0.10295   3.932 8.42e-05 ***
# Net.main.income_woe                 0.46536    0.27334   1.702  0.08867 .  
# Source.of.main.income_woe           0.30921    0.33703   0.917  0.35891    
# Additional.income_woe               1.08583    0.48442   2.242  0.02499 *  
# Reported.expenses_woe               0.88670    0.36023   2.461  0.01384 *  
# Months.with.bank_woe                0.24002    0.12148   1.976  0.04819 *  
# Client.type_woe                     0.42186    0.16545   2.550  0.01078 *  
# Property.object_woe                 0.12469    0.27998   0.445  0.65605    
# Eskhata.Online_woe                  0.14938    0.24872   0.601  0.54810    
# BKI.Rating_woe                      0.65667    0.13132   5.001 5.71e-07 ***
# Dependants_woe                      0.35790    0.23299   1.536  0.12452    
# Months.at.job_woe                   0.53337    0.10531   5.065 4.09e-07 ***
# Age_woe                             0.24528    0.10912   2.248  0.02458 *  
# PreviousLoans_Count_woe            -1.08963    0.48259  -2.258  0.02395 *  
# PreviousLoans_MaxDPD_woe            0.87124    0.20384   4.274 1.92e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#//

# Этап 3, Итерации 1-N. 
# Формула финальная. Процесс итеративный. 
# Убираем (Комментим #) те переменные, при котооых стоит знак "-".
# Убираем (Комментим #) те переменные, для которых в выводе регрессии высокие значения chi-squared (p-level, t-value), более 0.1 - однозначно выбрасывем, больше 0.05 - смотрим, нужны ли они нам, более 0.01 также можем оставлять, если переменных большое кол-во, более 10 штук в модели.
# Данный процесс проводим последовательно с расчетом метрик прогнозной силы модели на train, test, out-of-time выборках. Значения не должны отличаться существенно.
# Если значения отличаются существенно, например, AUC Train = 0.78, AUC OOT = 0.68 - в модели есть незначимые оценки, т.е. оценки с большой дисперсией (гетероскедастичность). Мы получаем хороши результаты на тренинговой выборке, но плохие на тестовой из другого временного интервала
# Сокращаем кол-во предикторов до тех пор, пока AUC Train и AUC OOT не сойдутся до разницы в 0.01 - 0.03.

final_formula_train <- 
  GB ~ 
  Loan.Amount_woe + 
  Salary.payment.in.bank.account_woe + 
  Region.of.living_woe + 
  Education_woe + 
  Marital.status_woe + 
  Employment.segment_woe + 
  # Net.main.income_woe + 
  # Additional.income_woe + 
  # Reported.expenses_woe + 
  Months.with.bank_woe + 
  # Client.type_woe + 
  BKI.Rating_woe + 
  # Dependants_woe + 
  Months.at.job_woe + 
  Age_woe + 
  # PreviousLoans_Count_woe + 
  PreviousLoans_MaxDPD_woe

m2_train <- glm(final_formula_train, data = train_woe, family = binomial)
summary(m2_train)

# Output
# Call:
#   glm(formula = final_formula_train, family = binomial, data = train_woe)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -3.86971    0.06141 -63.011  < 2e-16 ***
#   Loan.Amount_woe                     1.65176    0.19492   8.474  < 2e-16 ***
#   Salary.payment.in.bank.account_woe  0.63525    0.19861   3.198 0.001381 ** 
#   Region.of.living_woe                0.35683    0.15178   2.351 0.018724 *  
#   Education_woe                       0.38819    0.16235   2.391 0.016799 *  
#   Marital.status_woe                  0.42593    0.11023   3.864 0.000112 ***
#   Employment.segment_woe              0.47176    0.09886   4.772 1.83e-06 ***
#   Months.with.bank_woe                0.45537    0.09348   4.871 1.11e-06 ***
#   BKI.Rating_woe                      0.66564    0.13104   5.080 3.78e-07 ***
#   Months.at.job_woe                   0.55582    0.10518   5.285 1.26e-07 ***
#   Age_woe                             0.25763    0.10859   2.372 0.017671 *  
#   PreviousLoans_MaxDPD_woe            0.82824    0.21540   3.845 0.000120 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Все коэффициенты модели значимы. Сокращать количество предикторов далее смысла не имеет. Значимость предиктора свидетельствует о допустимой дисперсии оценки и отстутвии гетероскедастичности.

# Валидация модели. 
# ЗАПУСКАЕМ ПРИ КАЖДОМ ИЗМЕНЕНИИ НАБОРА ПРЕДИКТОРОВ

# Запускаем расчет прогнозных вероятностей события для подвыборок - обучающая, тестовая out-of-sample, тестовая out-of-time.
# В функцию predict подаются модель, выборка, которую нужно проскорить, метод 'response' означает, что вывести нужно вероятность
train_pred <- predict(m2_train, train_woe, type = 'response')
test_pred <- predict(m2_train, test_woe, type = 'response')
oot_pred_train <- predict(m2_train, oot_woe, type = 'response')

# ----- It's Real Validation Metric Values! ------
# Используем метод perf_eva библиотеки scorecard() для рассчета основных метрик валидации модели - AUC, KS. Выводим Confusion Matrix и графики.
train_perf  <- perf_eva(label = train$GB, pred = train_pred, title = 'M1 - Employee - Train', confusion_matrix = TRUE,
                      show_plot = c("ks","roc"))
test_perf  <- perf_eva(label = test$GB, pred = test_pred, title = 'M1 - Employee - Test', confusion_matrix = TRUE,
                        show_plot = c("ks","roc"))
oot_perf_train  <- perf_eva(label = oot$GB, pred = oot_pred_train, title = 'M1 - Employee - OOT with Train', confusion_matrix = TRUE,
                        show_plot = c("ks","roc"))

# oot_perf_train  <- perf_eva(label = oot$GB, pred = oot_pred_train, title = 'M1 - Employee: OOT with Train', confusion_matrix = TRUE,
#                             show_plot = c("ks","roc"), threshold = 0.025)

# Выводим набор метрик для указанных подвыборок.
train_perf$binomial_metric
test_perf$binomial_metric
oot_perf_train$binomial_metric

# В выводе мы видим, что AUC для train, test, oot отличаются незначительно, на ~1%. 
# Переобучения модели (overfitting), т.е. подгонки параметров под обучающий набор данных, не наблюдается.
# Небольшая разница в AUC для train, test, oot свидетельствует о стабильности модели и параметров, используемых в модели.

# > train_perf$binomial_metric
# $`M1 - Employee - Train`
# MSE      RMSE    LogLoss         R2        KS       AUC      Gini
# 1: 0.02008432 0.1417191 0.09012974 0.03194956 0.4649095 0.7940628 0.5881256
# 
# > test_perf$binomial_metric
# $`M1 - Employee - Test`
# MSE      RMSE    LogLoss         R2        KS       AUC      Gini
# 1: 0.02086351 0.1444421 0.09256598 0.03766817 0.4986973 0.8030652 0.6061303
# 
# > oot_perf_train$binomial_metric
# $`M1 - Employee - OOT with Train`
# MSE       RMSE    LogLoss          R2        KS       AUC      Gini
# 1: 0.006659053 0.08160302 0.03960204 -0.04126925 0.4809681 0.8043743 0.6087487


oot_perf_train$binomial_metric$`M1 - Employee - OOT with Train`$KS
oot_perf_train$binomial_metric$`M1 - Employee - OOT with Train`$AUC
oot_perf_train$binomial_metric$`M1 - Employee - OOT with Train`$Gini

# oot_perf_train$binomial_metric
# oot_perf_train$confusion_matrix
# oot_perf_train$pic

# We've proved that the prediction is stable


#  NOW TRAIN WITH ALL DATA - FINAL MODEL

# ЭТАП 4.
# Проводим обучение модели логистической регрессии на полных данных: dev sample + oot sample = dev_final
# Запускаем модель со всеми предикторами
formula <- GB ~ .

m1 <- glm(formula, data = dev_final_woe, family = binomial)
summary(m1)

# Вывод модели

# Call:
#   glm(formula = formula, family = binomial, data = dev_final_woe)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -4.02231    0.04686 -85.838  < 2e-16 ***
#   Loan.Amount_woe                     1.32242    0.13841   9.554  < 2e-16 ***
#   Salary.payment.in.bank.account_woe  0.54129    0.15771   3.432 0.000599 ***
#   Gender_woe                          0.23999    0.18872   1.272 0.203486    
# Region.of.living_woe                0.30146    0.25495   1.182 0.237020    
# Region.of.registration_woe         -0.05152    0.27409  -0.188 0.850905    
# Education_woe                       0.50183    0.12469   4.024 5.71e-05 ***
#   Marital.status_woe                  0.30975    0.08875   3.490 0.000483 ***
#   Employment.segment_woe              0.50687    0.08085   6.270 3.62e-10 ***
#   Net.main.income_woe                 0.23765    0.20862   1.139 0.254643    
# Source.of.main.income_woe           0.57328    0.24360   2.353 0.018603 *  
#   Additional.income_woe               0.98589    0.38365   2.570 0.010177 *  
#   Reported.expenses_woe               0.74174    0.27227   2.724 0.006444 ** 
#   Months.with.bank_woe                0.55630    0.08386   6.634 3.28e-11 ***
#   Client.type_woe                     0.10842    0.12153   0.892 0.372319    
# Property.object_woe                 0.13953    0.21709   0.643 0.520389    
# Eskhata.Online_woe                  0.53561    0.19795   2.706 0.006816 ** 
#   Plastic.Cards_woe                  -0.08213    0.27352  -0.300 0.763966    
# BKI.Rating_woe                      0.82856    0.10906   7.598 3.02e-14 ***
#   BKI.Number.of.Loans_woe             0.03908    0.11674   0.335 0.737800    
# Dependants_woe                      0.38485    0.18032   2.134 0.032818 *  
#   Months.at.current.address_woe       0.12292    0.12620   0.974 0.330079    
# Months.at.job_woe                   0.45956    0.08185   5.615 1.97e-08 ***
#   Age_woe                             0.34812    0.08455   4.117 3.83e-05 ***
#   CityOfLivingEqRegistration_woe     -0.19441    0.22218  -0.875 0.381580    
# PreviousLoans_Count_woe            -0.65658    0.34954  -1.878 0.060328 .  
# PreviousLoans_MaxDPD_woe            0.84651    0.13957   6.065 1.32e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Этап 5.
library(MASS)
#Stepwise Logistic regression
m_step <- stepAIC(m1, direction = "both")
m2 <- eval(m_step$call)

summary(m2)

final_formula <- 
  GB ~ 
  Loan.Amount_woe + 
  Salary.payment.in.bank.account_woe + 
  Gender_woe + 
  Region.of.living_woe + 
  Education_woe + 
  Marital.status_woe + 
  Employment.segment_woe + 
  Net.main.income_woe +
  Source.of.main.income_woe +
  Reported.expenses_woe +
  Months.with.bank_woe + 
  Property.object_woe +
  Eskhata.Online_woe + 
  BKI.Rating_woe + 
  Dependants_woe + 
  Months.at.job_woe + 
  Age_woe + 
  PreviousLoans_Count_woe +
  PreviousLoans_MaxDPD_woe

# Call:
#   glm(formula = final_formula, family = binomial, data = dev_final_woe)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                        -4.02157    0.04675 -86.031  < 2e-16 ***
# Loan.Amount_woe                     1.31176    0.13733   9.552  < 2e-16 ***
# Salary.payment.in.bank.account_woe  0.52765    0.15420   3.422 0.000622 ***
# Gender_woe                          0.23688    0.18831   1.258 0.208418
# Region.of.living_woe                0.21684    0.12046   1.800 0.071836 .
# Education_woe                       0.50911    0.12404   4.104 4.05e-05 ***
# Marital.status_woe                  0.30347    0.08854   3.428 0.000609 ***
# Employment.segment_woe              0.50605    0.08036   6.297 3.03e-10 ***
# Net.main.income_woe                 0.23406    0.20851   1.123 0.261639
# Source.of.main.income_woe           0.57407    0.24259   2.366 0.017962 *
# Reported.expenses_woe               0.75247    0.27240   2.762 0.005738 **
# Months.with.bank_woe                0.60863    0.07450   8.169 3.10e-16 ***
# Property.object_woe                 0.12111    0.21483   0.564 0.572927
# Eskhata.Online_woe                  0.54755    0.18653   2.935 0.003331 **
# BKI.Rating_woe                      0.83663    0.10561   7.922 2.34e-15 ***
# Dependants_woe                      0.40194    0.17883   2.248 0.024603 *
# Months.at.job_woe                   0.47551    0.08147   5.837 5.32e-09 ***
# Age_woe                             0.36299    0.08399   4.322 1.55e-05 ***
# PreviousLoans_Count_woe            -0.47229    0.29674  -1.592 0.111484
# PreviousLoans_MaxDPD_woe            0.84636    0.13945   6.069 1.28e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

m2$formula

final_formula <- 
  GB ~ 
  Loan.Amount_woe + 
  Salary.payment.in.bank.account_woe + 
  Gender_woe + 
  Region.of.living_woe + 
  Education_woe + 
  Marital.status_woe + 
  Employment.segment_woe + 
  # Net.main.income_woe + 
  # Source.of.main.income_woe + 
  # Reported.expenses_woe + 
  Months.with.bank_woe + 
  # Property.object_woe + 
  Eskhata.Online_woe + 
  BKI.Rating_woe + 
  Dependants_woe + 
  Months.at.job_woe + 
  Age_woe + 
  # PreviousLoans_Count_woe + 
  PreviousLoans_MaxDPD_woe

# Этап 6. Итерации 1-N. 
# Определяем финальный набор переменных.

final_formula <- 
  GB ~ 
  Loan.Amount_woe + 
  Salary.payment.in.bank.account_woe + 
  Region.of.living_woe +
  Education_woe + 
  Marital.status_woe + 
  Employment.segment_woe + 
  # Net.main.income_woe + 
  # Source.of.main.income_woe + 
  # Additional.income_woe + 
  # Reported.expenses_woe + 
  Months.with.bank_woe + 
  # Eskhata.Online_woe + 
  BKI.Rating_woe + 
  # Dependants_woe + 
  Months.at.job_woe + 
  Age_woe + 
  # PreviousLoans_Count_woe + 
  PreviousLoans_MaxDPD_woe
  
# bins$Additional.income
# bins$Source.of.main.income
# bins$Source.of.additional.income
# bins$Property.object
# bins$Reported.expenses
# bins$Net.main.income
# bins$Region.of.living
# bins$CityOfLivingEqRegistration

# --- This is Final Model for Deployment ------

m2 <- glm(final_formula, data = dev_final_woe, family = binomial)
summary(m2)

# Финальная модель. Полученные коэффициенты логистической регресст и их значимость немного отличаются от полученных на Этапе 3 на выборке Train.
# Call:
#   glm(formula = final_formula, family = binomial, data = dev_final_woe)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                        -4.02768    0.04669 -86.259  < 2e-16 ***
#   Loan.Amount_woe                     1.31880    0.13663   9.652  < 2e-16 ***
#   Salary.payment.in.bank.account_woe  0.58873    0.15249   3.861 0.000113 ***
#   Region.of.living_woe                0.23493    0.11673   2.013 0.044167 *  
#   Education_woe                       0.53099    0.12250   4.335 1.46e-05 ***
#   Marital.status_woe                  0.35154    0.08381   4.195 2.73e-05 ***
#   Employment.segment_woe              0.55189    0.07742   7.129 1.01e-12 ***
#   Months.with.bank_woe                0.67656    0.07182   9.421  < 2e-16 ***
#   BKI.Rating_woe                      0.83694    0.10553   7.931 2.17e-15 ***
#   Months.at.job_woe                   0.47730    0.08149   5.857 4.71e-09 ***
#   Age_woe                             0.35663    0.08376   4.258 2.06e-05 ***
#   PreviousLoans_MaxDPD_woe            0.83324    0.14335   5.813 6.15e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# write.xlsx(m2, "M1_Employees_m2.xlsx")

# -------- Model Validation ---
# Валидация модели. 
# ЗАПУСКАЕМ ПРИ КАЖДОМ ИЗМЕНЕНИИ НАБОРА ПРЕДИКТОРОВ

dev_pred <- predict(m2, dev_final_woe, type = 'response')
oot_pred <- predict(m2, oot_woe, type = 'response')
oot2_pred <- predict(m2, oot2_woe, type = 'response')


# dev$pred <- predict(m2, dev_woe, type = 'response')

dev_perf  <- perf_eva(label = dev_final_sel$GB, pred = dev_pred, title = 'M1 - Employee - Dev', confusion_matrix = TRUE, 
                      show_plot = c("ks","roc"))

oot_perf  <- perf_eva(label = oot$GB, pred = oot_pred, title = 'M1 - Employee - OOT', confusion_matrix = TRUE, 
                      show_plot = c("ks","roc"))

oot2_perf  <- perf_eva(label = oot2$GB, pred = oot2_pred, title = 'M1 - Employee - OOT2', confusion_matrix = TRUE, 
                       show_plot = c("ks","roc"))


oot_perf$binomial_metric$'M1 - Employee - OOT'$KS
oot_perf$binomial_metric$'M1 - Employee - OOT'$AUC
oot_perf$binomial_metric$'M1 - Employee - OOT'$Gini


# Этап 7. Сравнение метрик для всех моделей
dev_perf$binomial_metric
oot_perf$binomial_metric
oot2_perf$binomial_metric

train_perf$binomial_metric
test_perf$binomial_metric
oot_perf_train$binomial_metric

write.xlsx(train_perf$binomial_metric, "M1 - Binomial Metric - 1 - Train.xlsx")
write.xlsx(test_perf$binomial_metric, "M1 - Binomial Metric - 2 -Test.xlsx")
write.xlsx(oot_perf_train$binomial_metric, "M1 - Binomial Metric - 3 - OOT by Train.xlsx")
write.xlsx(dev_perf$binomial_metric, "M1 - Binomial Metric - 4 -Dev.xlsx")
write.xlsx(oot_perf$binomial_metric, "M1 - Binomial Metric - 5 -OOT.xlsx")
write.xlsx(oot2_perf$binomial_metric, "M1 - Binomial Metric - 6 - OOT2.xlsx")

write.xlsx(oot_perf$confusion_matrix, "M1 - Confusion Matrix - OOT.xlsx")
write.xlsx(dev_perf$confusion_matrix, "M1 - Confusion Matrix - Dev.xlsx")


# ------ End of Validation ------------

#  -------- Scorecard ----------------

# Модель получена и записана в объект m2. Теперь применяем метод scorecard::scorecard и получаем скоринговую карту.
card = scorecard(bins, m2)

# Баллы скоринговой карты подлучаем согласно шкалы FICO. Можно использовать любую свою удобную шкалу.
# Настраиваемые параметры:
# points0 = 660 - Балл, для которого выполняется соотношение odds (условное начало координат)
# odds0 = 1/72 - соотношение odds плохих к хорошим. Здесь - на одного плохого приходится 72 хороших случая, в точке 660.
# pdo = 40 - шаг, при котором соотношение odds плохих к хорошим увеличивается или уменьшается В 2 раза

card = scorecard(bins, 
                 m2,
                 basepoints_eq0 = TRUE,
                 points0 = 660,
                 odds0 = 1/72,
                 pdo = 40)

write.xlsx(card, "M1_Employee_Scorecard.xlsx")

sample_scored <- sample

# Берем всю выборку данных и расчитываем для нее каждого договора дефолта и значение коэффициента регрессии - некалиброванный балл

sample_scored$pred <- predict(m2, sample_woe, type = 'response')
sample_scored$score <- predict(m2, sample_woe)

# Применяем полученную скоринговую карту к выборке данных. Расчитываем калиброванный балл, т.е. балл согласно шкалы.

score_cal <- scorecard_ply(sample_scored, card)

# Добавляем вектор с расчитанным калиброванным баллом к общей выборке
sample_scored$score_cal <- score_cal$score

# str(sample_scored)
# View(sample_scored)

# write.xlsx(sample_scored, "M1_Employee_sample_scored.xlsx")

# Та же самая операция для выборки dev_final. Это нам нудно для расчета распределний выборки по скорбаллу и бектестинга - логической проверки насколько логично модель определяет уровень дефолта
dev_final_scored <- dev_final

dev_final_scored$pred <- predict(m2, dev_final_woe, type = 'response')
dev_final_scored$score <- predict(m2, dev_final_woe)

score_cal_dev <- scorecard_ply(dev_final_scored, card)

dev_final_scored$score_cal <- score_cal_dev$score

# Использует функцию gains_table для получения распределений кол-ва наблюдений и дефолт рейт (вероятности события) по диапазонам скорингового балла
# Важно - Positive - это определение события, для дефолта. Таким образом, Positive = 1 подразумевает позитивный тест на событие (дефолт), но НЕ хороший (позитивный) договор
gain_table_1 <- gains_table(score = dev_final_scored$score_cal, label = dev_final_scored$GB, positive = 1)
gain_table_2 <- gains_table(score = dev_final_scored$score_cal, label = dev_final_scored$GB, bin_num = 20, positive = 1)

write.xlsx(gain_table_1, "M1_Employee_gain_table_10.xlsx")
write.xlsx(gain_table_2, "M1_Employee_gain_table_20.xlsx")




# oot_points = scorecard_ply(oot, card, only_total_score = FALSE)

# Тест стабильности популяции. 
# Тест стабильности популяции показывает смещение распределения скорингового балла тестируемой популяции относительно базовой. 
# Для оценки стабильности популяции применяется метрика Population Stability Index. 

# psi test - Stability
train_score = scorecard_ply(dev, card)
oot_score = scorecard_ply(oot, card)
oot2_score = scorecard_ply(oot2, card)

oot_score2 = scorecard_ply(oot, card, only_total_score=FALSE)
train_score2 = scorecard_ply(dev, card, only_total_score=FALSE)
# View(oot_score2)

psi = perf_psi(
  score = list(train = train_score, oot = oot_score),
  label = list(train = dev$GB, oot = oot$GB)
)
psi

psi = perf_psi(
  score = list(train = train_score, oot = oot2_score),
  label = list(train = dev$GB, oot = oot2$GB)
)
psi

psi = perf_psi(
  score = list(train = train_score2, oot = oot_score2),
  label = list(train = dev$GB, oot = oot$GB)
)
psi

# score_list2 = lapply(list(oot2,oot), function(x) scorecard_ply(x, card, only_total_score=FALSE))
# # label_list = list(train = oot2$GB, oot = oot$GB)
# label_list = lapply(list(oot2,oot), function(x) x$GB)
# 
# 
# psi2 = perf_psi(score = score_list2, label = label_list)

