
# 패키지 로드 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  readxl,
  tidytext,
  KoNLP
)


# 데이터 로드 및 확인 --------------------------------------------

# 데이터 로드
readxl::read_xlsx("NewsResult_20200228-20200504.xlsx") -> rawdata

# 데이터 살펴보기
rawdata %>% glimpse()


# 데이터 전처리 ---------------------------------------------

rawdata %>%
  select(id,
         date,
         press,
         character,
         location,
         institute,
         keyword,
         WeightTop50) %>% # 필요한 변수 선별
  mutate(
    date = lubridate::as_date(date), # 날짜변수로 변환
    press = as.factor(press), # 언론사를 factor로 변환
    id = 1:length(date) # 기사별 id 부여 (식별자 단순화)
  ) -> rawdata
rawdata

# 시기별 기사 빈도분석 ----------------------------------------

par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))

rawdata %>% 
  count(date) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_col(fill = "steelblue") +
  geom_line(color="#69b3a2", size=0.5, alpha=0.9, linetype=1) +
  geom_point(color="#69b3a2") +
  ylim(0, 100) +
  geom_abline(intercept=10.4, slope=1.75) +
  xlab("일자") + 
  ylab("기사 빈도수")

rawdata %>% 
  mutate(시기 = ifelse(month(date) == 3 & day(date) < 18, "초기",
                     ifelse(month(date) == 3 & day(date) > 18, "중기",
                            ifelse(month(date) == 4 & day(date) < 30, "중기", "후기")))) %>%
  mutate(시기 = ifelse(시기 == "중기" & month(date) == 4 & day(date) >= 16, "중기_총선이전",
                       ifelse(시기 == "중기" & month(date) == 3, "중기_총선이전", 
                                ifelse(시기 == "중기", "중기_총선이후", 시기)))) %>% 
  mutate(시기 = as.factor(시기)) %>% 
  group_by(시기) %>% 
  count(시기) %>% 
  ggplot(aes(x = 시기, y = n, fill = 시기)) +
  geom_col() +
  scale_x_discrete(limits = c("후기", "중기_총선이후", "중기_총선이전", "초기")) +
  geom_text(aes(label = n, hjust = -0.2)) +
  coord_flip() +
  ylim(0, 1300) +
  xlab("시기구분") + 
  ylab("기사 빈도수")
  


