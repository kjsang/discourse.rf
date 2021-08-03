
# 패키지 로드 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  readxl,
  tidytext,
  KoNLP, lubridate, tidylo
)


# 데이터 불러오기 --------------------------------------------
readxl::read_xlsx("keyword.xlsx") -> keyword
readxl::read_xlsx("character.xlsx") -> char
keyword %<>% 
  rename(id = `뉴스 식별자`)
char %<>% 
  rename(id = `뉴스 식별자`)
char %<>% 
  select(id, 정보원, 인용문)
keyword %>% 
  left_join(char, by = "id") %>% 
  select(id, 일자, 언론사, 제목, 인물, 키워드, 정보원, 인용문) -> rawdata

rawdata %>% 
  count(정보원) %>% 
  filter(!정보원 %in% c("unknownactor", NA)) %>% 
  arrange(desc(n)) %>% 
  as.data.frame()
