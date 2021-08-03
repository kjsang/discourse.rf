
# 패키지 로드 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  readxl,
  tidytext,
  KoNLP, lubridate, tidylo
)


# 데이터 불러오기 --------------------------------------------
format(var, scientific = FALSE)

readxl::read_xlsx("rawdata.xlsx") -> rawdata
rawdata %>% 
  rename(text = contents) %>% 
  mutate(date = lubridate::as_date(date)) %>% 
  select(date, press, text) -> data
data %>% 
  count(word) %>% 
  arrange(desc(n))

data %>% distinct(text, .keep_all=TRUE) %>% 
  select(-word, -id) -> textdata

par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))

textdata %>% 
  filter(!actor == "unknownactor") %>% 
  
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_col(fill = "steelblue") +
  geom_line(color="#69b3a2", size=0.5, alpha=0.9, linetype=1) +
  geom_point(color="#69b3a2") +
  ylim(0, 120) +
  geom_abline(intercept=10.4, slope=1.75) +
  xlab("일자") + 
  ylab("인용 빈도수")

data %>% 
  mutate(시기 = ifelse(month(date) == 2 | month(date) == 3 & day(date) < 18, "초기",
                     ifelse(month(date) == 3 & day(date) > 18, "중기",
                            ifelse(month(date) == 4 & day(date) < 30, "중기", "후기")))) %>%
  mutate(시기 = ifelse(시기 == "중기" & month(date) == 4 & day(date) >= 16, "초기",
                       ifelse(시기 == "중기" & month(date) == 3, "초기", 
                                ifelse(시기 == "중기", "중기_총선이전", 시기)))) %>% 
    mutate(시기 = as.factor(시기)) -> data

data %>%
  unnest_tokens(
    input = text,
    output = word,
    token = SimplePos09,
    drop = F
  ) %>% 
  filter(str_detect(word, "/n")) %>%          # 명사 추출
  mutate(word = str_remove(word, "/.*$")) %>% # 형태소 정보 제거
  filter(str_length(word)>=2) -> data

data %>% 
  select(시기, word) %>% 
  group_by(시기) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(str_count(word) >= 2) %>% 
  slice_max(n, n = 20, with_ties = F) %>% 
  ggplot(aes(x = reorder(word, n),
             y = n,
             fill = 시기)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~시기, scales = "free_y") +
  ylab("단어빈도수") +
  xlab("출현단어(명사)")

data %>% 
  select(시기, word) %>% 
  group_by(시기) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(str_count(word) >= 2) %>% 
  slice_max(n, n = 20, with_ties = F) %>% 
  ggplot(aes(x = reorder(word, n),
             y = n,
             fill = 시기)) +
  geom_col() +
  coord_flip() +
  ylab("단어빈도수") +
  xlab("출현단어(명사)") +
  ylim(0, 800)

data %>%
  filter(시기 %in% c("초기", "중기_총선이전")) %>% 
  group_by(시기) %>% 
  count(word) %>%
  pivot_wider(
    names_from = 시기,
    values_from = n,
    values_fill = list(n = 0)
  ) %>%
  mutate(
    ratio_초기 = ((초기 + 1) / (sum(초기 + 1))),
    ratio_중기_총선이전 = ((중기_총선이전 + 1) / (sum(중기_총선이전 + 1))),
    odds_ratio = ratio_초기 / ratio_중기_총선이전
  ) -> ratio_시기
ratio_시기 %>% 
  filter(rank(odds_ratio) <= 20 | rank(-odds_ratio) <= 20) %>%
  arrange(-odds_ratio) %>%
  print(n = Inf)

ratio_시기 %>% 
  mutate(log_odds_ratio = log(odds_ratio)) %>% 
  group_by(시기 = ifelse(log_odds_ratio > 0, "초기", "중기_총선이전")) %>%
  slice_max(abs(log_odds_ratio), n = 20, with_ties = F) -> top10_log
top10_log %>% 
  arrange(-log_odds_ratio) %>% 
  select(word, log_odds_ratio, 시기) %>% 
  ggplot(aes(x = reorder(word, log_odds_ratio),
             y = log_odds_ratio,
             fill = 시기)) +
  theme_gray(base_family = "AppleGothic") +
  geom_col() +
  coord_flip() +
  ylab("odds비") +
  xlab("출현단어(명사)")
