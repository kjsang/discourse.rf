
# 연관분석, Association Rule ------------------------------------------------

pacman::p_load(
  tidyverse, tidytext, magrittr, # 데이터 처리 helper packages
  tidymodels, # 모델 구축
  KoNLP, # NLP tools
  tm, SnowballC,
  igraph, ggraph,
  widyr, wordcloud,
  arules, # 연관분석 
  arulesViz, visNetwork, # 연관분석 시각화
  topicmodels # 토픽모델링
)

# 데이터 로드 ----------------------------------------------
read_csv("text.csv") -> text_tb

# 데이터 변환 및 전처리 ----------------------------------------
text_tb %<>% 
  unnest_tokens(text,
                output = word)

# 데이터 전처리 ----------------------------------------------
# 불용어 사전 만들기
st_word <- tibble(
  word = c("aaa", "bbb")
)
# 불용어 처리
text_tb %<>% 
  anti_join(
    stop_words, # 기존의 불용어사전
    st_word, # 나만의 불용어사전
    by = "word" # 기준이 되는 칼럼 
    ) %>% 
  mutate(word = word %>% 
           str_remove_all("\\d+") %>% # 숫자 제거
           wordStem()) %>% # 어간 추출 
  filter(str_count(word) >= 1) # 1글자 이상인 것만 추출

# 단어 빈도 및 tf-idf 정리 -----------------------------------
text_tb %>% 
  count(doc, word, sort = T) %>%  # 문서별 단어 빈도 체크
  bind_tf_idf(word, doc, n) %>% 
  group_by(word) %>% 
  summarise(n = sum(n, na.rm = T),
            tf_idf = sum(tf_idf, na.rm = T)) %>% 
  arrange(desc(n)) %>% 
  ungroup() -> text_count

# TDM DTM 행렬 만들기 --------------------------------------

# 단어빈도 출력
text_count %>% 
  mutate(word = reorder(word, n)) %>% # 단어를 n 수로 정렬 
  filter(n > 1) %>% 
  ggplot(aes(x = n,
             y = word,
             fill = word)) +
  geom_col()

# tf-idf 값 출력
text_count %>% 
  slice_max(tf_idf, n = 4) %>% 
  ggplot(aes(x = tf_idf,
             y = fct_reorder(word, tf_idf),
             fill = word)) +
  geom_col() +
  xlab("tf-idf") +
  ylab("word")

# tdm으로 변환
text_tb %>% 
  count(doc, word, sort = T) %>%  # 문서별 단어 빈도 체크
  bind_tf_idf(word, doc, n) %>% 
  cast_tdm(term = word,
           document = doc,
           value = n) %>% 
  inspect()

# tf-idf를 tdm으로 변환 
text_tb %>% 
  count(doc, word, sort = T) %>%  # 문서별 단어 빈도 체크
  bind_tf_idf(word, doc, n) %>% 
  cast_tdm(term = word,
           document = doc,
           value = tf_idf) %>% 
  inspect()

#dtm으로 변환 
text_tb %>% 
  count(doc, word, sort = T) %>%  # 문서별 단어 빈도 체크
  bind_tf_idf(word, doc, n) %>% 
  cast_dtm(document = doc,
           term = word,
           value = n) -> dtm

# 데이터 정리 ----------------------------------------------

dtm %>% tm::inspect()
dtm %>%   as.matrix() -> dtm_m
ifelse(dtm_m > 0, 1, 0) -> dtm_m


# transaction 변환 --------------------------------------

dtm_m %>%  as("transactions") -> text_trans
text_trans %>%  arules::inspect()
text_trans %>%  itemFrequencyPlot(support = 0.5, topN = 20)
text_trans %>%  apriori(parameter = list(
  supp = 0.5,
  conf = 0.5,
  target = "rules"
)) -> text_rules

text_rules %>% 
  inspect() %>%  # 룰이 만들어진 것을 확인할 수 있다.
  as_tibble(
    .name_repair = "unique"
    ) -> text_rules_tb
text_rules_tb %>% 
  arrange(desc(lift)) %>% 
  filter(support >= 0.5,
         confidence >= 0.7,
         lift > 1)
text_rules_tb %>% 
  arrange(desc(lift)) %>% 
  filter(rhs == "{boi}")


# 시각화 -------------------------------------------------

plot(sort(text_rules,
          by = "support"),
     method = "grouped")

plot(sort(text_rules,
          by = "lift")[1:10],
     method = "graph",
     engine = "htmlwidget")


# 토픽분석 ------------------------------------------------
dtm %>% 
  topicmodels::LDA(k = 2, 
                   control = list(seed = 123)) -> text_lda
text_lda %>% 
  tidy(matrix = "beta") -> text_topic # 타이디하게 바꾸기
text_topic %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(topic = str_c("topic", topic)) %>% 
  ggplot(aes(x = beta,
             y = fct_reorder(term, c(beta, topic)),
             fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap( ~ topic, scales = "free") +
  scale_y_reordered()

beta_wide <- text_topic %>% 
  mutate(topic = paste0("topic", topic)) %>% 
  pivot_wider(names_from = topic,
              values_from = beta) %>% 
  arrange(desc(topic1, topic2))
beta_wide

