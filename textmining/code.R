
# 패키지 로드 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  readxl,
  tidytext,
  KoNLP
)


# 데이터 불러오기 --------------------------------------------

readxl::read_xlsx("rawdata.xlsx") -> rawdata
rawdata %>% 
  mutate(id = 1:length(contents)) %>% 
  rename(text = contents) %>% 
  select(id, date, press, title, text) -> data
data %>% 
  unnest_tokens(words, contents) %>% 
  select(id, press, words) %>% 
  group_by(press) %>% 
  count(words) %>% 
  arrange(desc) # 데이터 중 너무 많은 수가 조사를 포함하고 있는 것을 알 수 있음.
# # Groups:   press [5]
#    press    words      n
#    <chr>    <chr>  <int>
#  1 조선일보 있다      99
#  2 중앙일보 있다      96
#  3 경향신문 코로나    75
#  4 경향신문 19        73
#  5 한겨레   있다      68
#  6 경향신문 있다      66
#  7 경향신문 한다      62
#  8 경향신문 수        59
#  9 조선일보 한다      58
# 10 조선일보 전        57
# # … with 18,133 more rows

data %>%
  group_by(id) %>%
  mutate(
    text_matched = SimplePos09(text) %>%
      unlist() %>%
      paste(collapse = ' ') %>%
      str_extract_all(regex('[^\\s]+/N')) %>% #POS에서 명사부만 뽑겠습니다.
      unlist() %>%
      paste(collapse = ' ') %>%
      str_remove_all('/N') #태그는 지우지요.
  ) %>%
  ungroup() %>%
  unnest_tokens(word, text_matched) %>%
  select(-text) -> tidy.text

stopwords

tidy.text %>% 
  select(id, press, word) %>% 
  group_by(press) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(!word %in% stop_words)
