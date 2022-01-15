
# 0.1. 패키지 로드 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  readxl,
  tidytext,
  KoNLP, lubridate, tidylo,
  gridExtra, # 화면분할
  topicmodels, tm, furrr,
  widyr, tidygraph, ggraph
)

# 0.2. 인용 (패키지 및 R 버전) -------------------------------------
citation()
citation("tidyverse")
citation("readxl")
citation("lubridate")
citation("tidylo")
citation("tidytext")
citation("KoNLP")
citation("NIADic")
RStudio.Version()

# 1.1. 데이터 불러오기 --------------------------------------------
readxl::read_xlsx("keyword.xlsx") -> keyword
readxl::read_xlsx("character.xlsx") -> char
keyword %<>%  rename(id = `뉴스 식별자`)
char %<>%  rename(id = `뉴스 식별자`)
char %<>%  select(id, 정보원, 인용문)
keyword %>% 
  left_join(char, by = "id") %>% 
  select(id, 일자, 언론사, 제목, 인물, 키워드, 정보원, 인용문) -> rawdata

# 1.2. 사전 불러오기 ---------------------------------------------

useNIADic()
my_dic <- data.frame(
  c("100만원", "재난지원금", "재난기본소득", "긴급재난지원금", "코로나19", 
    "문재인", "추가경정예산", "지원범위", "지원대상", "지원금액",
    "마중물","부화뇌동", "재정건전성"),
  c("nqq", "nqq", "nqq", "nqq", "nqq", 
    "nqq", "nqq", "nqq", "nqq", "nqq",
    "nqq","nqq", "nqq")
)
buildDictionary(ext_dic = "NIADic",
                user_dic = my_dic)

stopping_ko_end=regex("입니다$|이다$")
stopping_ko=tibble(단어=c('이','가','은','는',
                          "정부", "국민", "우리", "이번", "논의", "억원", "오늘", "이후", "취지"))

# 2.1. 데이터 전처리: 정보원 ---------------------------------------------

rawdata %<>% 
  filter(!정보원 %in% c("unknownactor", NA)) %>%
  select(일자, 정보원, 인용문) %>% 
  mutate(일자 = lubridate::as_date(일자)) %>% 
  distinct() # 중복값 제거

rawdata %>%
  mutate(
    일자 = lubridate::as_date(일자),
    정보원 = 정보원 %>%
      str_replace_all("문 대통령", "문재인대통령") %>%
      str_replace_all("강민석 청와대 대변인|청와대 강민석 대변인", "청와대_강민석") %>% 
      str_replace_all("김상조 청와대 정책실장", "청와대_김상조") %>%
      str_replace_all("강기정 청와대 정무수석", "청와대_강기정") %>%
      str_replace_all("양정철 민주연구원장", "민주당 양정철") %>% 
      str_replace_all("조 의장|조 정책위의장", "민주당 조정식") %>% 
      str_replace_all("김 의장|김 정책위의장", "통합당 김재원") %>% 
      str_replace_all("문정선 대변인|문정선 선대위 대변인", "민생당") %>% 
      str_replace_all("심 대표", "정의당 심상정") %>% 
      str_replace_all("문진영 경기도 일자리재단 대표", "경기도_문진영경기도일자리재단") %>%
      str_replace_all("홍준표 전 자유한국당 대표", "홍준표") %>% 
      str_replace_all("일 오후 홍 부총리|홍 부총리|홍남기 경제부총리|홍남기 부총리", "기획재정부_홍남기") %>% 
      str_replace_all("홍형식 소장|홍형식 한길리서치 소장", "여론조사기관_한길리서치") %>% 
      str_replace_all("이 원내대표|민주당 원내대표|민주당 이 원내대표", "민주당_이인영") %>% 
      str_replace_all("심 원내대표|심 권한대행|심 의원|당대표 권한대행|대표 권한대행", "통합당_심재철") %>% 
      str_replace_all("민주당 관계자|더불어민주당|민주당 한 의원|민주당 핵심 관계자|민주당|강훈식 수석대변인|강 대변인", "민주당") %>% 
      str_replace_all("김 위원장|총괄선거대책 위원장|당 총괄선거대책위원장|김 전 위원장|전 선대위원장|선대 위원장", "통합당_김종인") %>% 
      str_replace_all("이 전 위원장|전략기획 위원장", "민주당_이근형") %>% 
      str_replace_all("유 의원", "통합당_유승민") %>% 
      str_replace_all("이 지사", "민주당_이재명") %>% 
      str_replace_all("김 지사", "민주당_김경수") %>% 
      str_replace_all("박 시장", "민주당_박원순") %>% 
      str_replace_all("이 대표", "민주당_이해찬") %>%
      str_replace_all("황 대표", "통합당_황교안") %>% 
      str_replace_all("정 총리", "민주당_정세균")
  ) -> predata_ver1

predata_ver1 %>% 
  mutate(
    정보원 = ifelse(정보원 %>%  str_detect("문재인"), "문재인대통령", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("정세균"), "국무총리_정세균", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("이낙연"), "민주당_이낙연", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("이근형"), "민주당_이근형", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("이인영"), "민주당_이인영", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("이해찬"), "민주당_이해찬", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("조정식"), "민주당_조정식", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("이재명"), "민주당_이재명_광역자치단체", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("김경수"), "민주당_김경수_광역자치단체", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("박원순"), "민주당_박원순_광역자치단체", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("이철우"), "통합당_이철우_광역자치단체", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("조경태"), "통합당_조경태", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("문희상"), "민주당 문희상", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("박주민"), "민주당_박주민", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("전해철"), "민주당_전해철", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("임종석"), "민주당 임종석", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("김재원"), "통합당_김재원", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("김종인"), "통합당_김종인", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("박형준"), "통합당_박형준", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("황교안"), "통합당_황교안", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("심재철"), "통합당_심재철", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("유승민"), "통합당_유승민", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("오세훈"), "통합당_오세훈", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("지상욱"), "통합당 지상욱", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("장제원"), "통합당 장제원", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("이종배"), "통합당 이종배", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("이준석"), "통합당_이준석", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("나경원"), "통합당 나경원", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("안철수"), "국민의당_안철수", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("심상정"), "정의당 심상정", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("윤형중"), "전문가집단_윤형중", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("청와대"), "청와대", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("안 대표"), "국민의당_안철수", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("기재부 장관|기획재정부 장관|경제 부총리|경제 총리|겸 경제부총리"), "기획재정부_홍남기", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("기재부|기획재정부 관계자|기획재정부 2차관|기획재정부 1차관|일 기획재정부|경제부처|경제 관료"), "기획재정부", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("외교부|국가과학기술정보통신부|통일부|법무부|국방부|행정안전부|문화체육관광부|농림축산식품부|산업통상부|보건복지부|복지부|환경부|고용노동부|여성가족부|국토교통부|해양수산부|중소벤처기업부|정부 관계자|정부 고위 관계자|한국 정부|행안부"), "정부", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("교육"), "교육계", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("더불어시민당|시민당"), "더불어시민당", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("열린민주당"), "열린민주당", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("통합당 관계자|미래통합당|통합당 "), "통합당", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("민주당 | 민주당|여당"), "민주당", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("민생당"), "민생당", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("정의당"), "정의당", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("남양주|조광한|조 시장"), "기초자치단체_남양주시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("구리"), "기초자치단체_구리시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("기장"), "기초자치단체_기장군", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("횡성"), "기초자치단체_횡성군", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("부천"), "기초자치단체_부천시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("여주"), "기초자치단체_여주시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("인천시"), "광역자치단체_인천시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("제주"), "광역자치단체_제주시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("군수|시장|구청장"), paste0("기초자치단체_",정보원), 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("허태정|허 시장"), "광역자치단체_대전시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("경기도"), "광역자치단체_경기도", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("울산시"), "광역자치단체_울산시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("경남"), "광역자치단체_경상남도", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("부산|변성완 시장 권한대행"), "광역자치단체_부산시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("대구"), "광역자치단체_대구시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("총리"), "국무총리_정세균", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("경제|연구|교수|학회"), paste0("전문가집단_",정보원), 정보원),
  ) %>% 
  filter(!정보원 %>%  str_detect("김 씨|코로나|관계자|국민|김 의원|재원|제기|원내대표")) -> predata_ver2

predata_ver2 %>% 
  select(정보원) %>% 
  count(정보원) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 3) %>% # 발언 3회 이상인 행위자 추출
  select(정보원) -> data_upper3

data_upper3 %>% 
  as.data.frame()

data_upper3 %>% 
  left_join(predata_ver2, by = "정보원") -> data_tb

# 2.2. 데이터 전처리: 일자 추가 -------------------------------------------

data_tb %>% # 1,504 건 추출
  arrange(일자)

data_tb %>%
  select(일자,  정보원,  인용문) %>%
  rename(일자 = 일자) %>% 
  mutate(시기 = ifelse(month(일자) == 3 & day(일자) < 18, "초기",
                     ifelse(month(일자) == 3 | month(일자) == 4 & day(일자) < 15, "중기", "후기")) %>% 
             as.factor()) %>% 
  mutate(id = 1:length(인용문)) %>% 
  select(id, 일자, 시기,  정보원,  인용문) %>%
  filter(!인용문 %>% str_detect("경제성장률")) %>% 
  filter(!인용문 %>% str_detect("호텔")) -> data_tb # 재난지원금과 무관한 인용문 제거

data_tb %>% write_excel_csv("data.csv") # 인용문 색인용 파일 생성

data_tb %>% 
  mutate(인용문 = 인용문 %>% 
              str_replace_all("코로나19", "코로나바이러스") %>% 
              str_replace_all("100만원", "백만원") %>% 
              str_replace_all("50만원", "오십만원") %>% 
              str_replace_all("\\r", "") %>% 
              str_replace_all("\\n", "") %>% 
              str_replace_all("재난 지원금", "재난지원금") %>% 
              str_replace_all("[^가-힣 ]", " ") %>% 
              str_replace_all("하겠|하겠나|하겠다|하라고|을|를|것|했다", " ")
            ) -> data_tb # 최종 1,499 건

# 2.3. 데이터 전처리 : 기본환경 설정 -----------------------------------

par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))

# 3.1. 시기별 담론 내 행위자 출현 빈도분석: 초기  ------------------------------

data_tb %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령","민주당_이인영", "민주당"), "중립", 
                          ifelse(정보원 %in% c("민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체",
                                            "민주당_박원순_광역자치단체", "정의당", "전문가집단_윤형중"), "찬성", "반대"))) %>% 
  filter(시기 == "초기") %>% 
  group_by(연합구분) %>% 
  count(시기, 정보원) %>% 
  slice_max(n, n = 5, with_ties = T) %>% 
  ggplot(aes(x = fct_reorder(정보원, n), y = n, fill = 연합구분)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~연합구분, drop = F, scales = "free_y", ncol = 3) +
  xlab("행위자") +
  ylab("출현빈도수") +
  geom_text(aes(label = n), hjust = -1) +
  ylim(0, 18) +
  theme(legend.position="none") -> 행위자_초기
행위자_초기


# 3.2. 시기별 담론 내 행위자 출현 빈도분석: 중기 ------------------

data_tb %>% 
  filter(시기 == "중기") %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균"), "찬성", 
                          ifelse(정보원 %in% c("통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부"), "반대", "중립"))) %>% 
  count(시기, 정보원, 연합구분) %>% 
  filter(!정보원 == "정부") %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(x = fct_reorder(정보원, n), y = n, fill = 연합구분)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~연합구분, drop = F, scales = "free_y", ncol = 2) +
  theme(legend.position = "none") +
  geom_text(aes(label = n), hjust = 1.2) +
  ggtitle("행위자 출현 빈도: 중기") +
  xlab("행위자") +
  ylab("출현빈도수") -> 행위자_중기
행위자_중기

# 3.3. 시기별 담론 내 행위자 출현 빈도분석: 후기 ------------------
data_tb %>% 
  filter(시기 == "후기") %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균", "민주당_이근형", "민주당_박주민", "민주당_조정식", "정부", "더불어시민당"), "찬성", 
                          ifelse(정보원 %in% c("통합당_조경태", "통합당_심재철", "통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부"), "반대", "중립"))) %>% 
  filter(연합구분 %in% c("찬성", "반대")) %>% 
  count(시기, 정보원, 연합구분) %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(x = fct_reorder(정보원, n), y = n, fill = 연합구분)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~연합구분, drop = F, scales = "free_y", ncol = 2) +
  theme(legend.position = "none") +
  geom_text(aes(label = n), hjust = 1.2) +
  ggtitle("행위자 출현 빈도: 후기") +
  xlab("행위자") +
  ylab("출현빈도수") -> 행위자_후기
행위자_후기

# 4.0. 토큰화 및 명사 추출 -----------------------------------------

data_tb %>% 
  as_tibble() %>% 
  unnest_tokens(input = 인용문,
                output = 단어,
                token = extractNoun,
                drop = F) %>% 
  select(-인용문) -> data_tb_word

data_tb_word %>% # 47,251 데이터 추출
  filter(단어 %>% str_length() > 1) %>% # 한 글자 단어 제거 35,587 
  filter(단어 %>% str_length() <= 10) %>% # 열 글자를 초과하는  단어 제거 35,538 
  mutate(단어 = 단어 %>% 
           str_replace_all("\\.", "")
  ) -> data_tb_word # 결과적으로 35,538

# 4.1. 데이터 전처리 : 불용어 제거 및 단어통합 ------------------------

data_tb_word %>% 
  mutate(
    단어 = ifelse(단어 %>% str_detect("만명|저희|마디|생각|가운데|측면|자세|여부|사람들|운영|사실상|말씀|되기|해당|당초|그것|국면|이것|않았다|누구|전체|도내|다음|여러분|방침|이전|자료|이하|하루|이야기|아니다|일정|이상|이후|하자|만큼|모두|가능|사람|때문|들이|우리|하기|수하"), "", 단어),
    단어 = ifelse(단어 %>%  str_detect("긴급재난지원금|재난지원금"), "긴급재난지원금",
                  ifelse(단어 %>%  str_detect("정부지원금"), "정부지원금",
                           ifelse(단어 %>%  str_detect("지원금"), "지원금", 단어))), # 지원금 관련 단어 통합
    단어 = ifelse(단어 %>%  str_detect("백만원"), "백만원", 
                  ifelse(단어 %>% str_detect("오십만원"), "오십만원",
                         ifelse(단어 %>% str_detect("만원"), "", 단어))), # ~만원 단어 정리
    단어 = ifelse(단어 %>%  str_detect("건강보험료"), "건강보험료",
                  ifelse(단어 %>% str_detect("보험료"), "보험료", 단어)), # 보험료 단어 통합
    단어 = ifelse(단어 %>%  str_detect("코로나"), "코로나바이러스", 단어),
    단어 = ifelse(단어 %>%  str_detect("포퓰리즘"), "포퓰리즘", 단어),
    단어 = ifelse(단어 %>%  str_detect("현금"), "현금", 단어),
    단어 = ifelse(단어 %>%  str_detect("동의하기") & 시기 == "초기", "동의하기어려움", 단어), # 초기 동의하기 검색 결과 모든 "동의하기" 이후 어렵다가 이어져 나오기 때문에 동의하기 어려움으로 코딩하였음
    단어 = ifelse(단어 %>%  str_detect("문재인|대통령"), "문재인대통령", 단어), # 대통령 단어 통합
    단어 = ifelse(단어 %>%  str_detect("김종인"), "김종인", 단어), # 인물 단어 통합
    단어 = ifelse(단어 %>%  str_detect("기획재정부|기재부"), "기획재정부", # 기획재정부, 정부, 중앙정부, 지방정부 등 통합
                  ifelse(단어 %>%  str_detect("지방정부"), "지방정부",
                           ifelse(단어 %>%  str_detect("재정부담"), "재정부담",
                                    ifelse(단어 %>%  str_detect("정부지원금"), "정부지원금",
                                             ifelse(단어 %>%  str_detect("부정부패"), "부정부패",
                                                      ifelse(단어 %>%  str_detect("중앙정부"), "중앙정부",
                                                               ifelse(단어 %>%  str_detect("정부"), "정부", 단어))))))),
    단어 = ifelse(단어 %>%  str_detect("마디"), "", 단어), # 황교안 대표의 "한 마디로" 라는 단어가 형태소 분석 결과 표현된 형태
    단어 = ifelse(단어 %>%  str_detect("윤석열|조국"), "", 단어),
    단어 = ifelse(단어 %>%  str_detect("마중"), "마중물", 단어),
    단어 = ifelse(단어 %>%  str_detect("투자"), "투자", 단어),
    단어 = ifelse(단어 %>%  str_detect("지역화"), "지역화폐", 단어),
    단어 = ifelse(단어 %>%  str_detect("일시"), "일시적", 단어),
    단어 = ifelse(단어 %>%  str_detect("부화"), "", 
                  ifelse(단어 %>% str_detect("뇌동"), "부화뇌동", 단어))
  ) %>% 
  filter(!is.na(단어)) %>% 
  filter(str_length(단어) > 1) -> data_tb_word_prep # 33,527

# 단어확인용
data_tb_word_prep %>% 
  filter(str_length(단어) > 1) %>% 
  group_by(단어) %>% 
  count() %>% 
  arrange(desc(n))


# 4.2. 데이터 전처리 : 빈도수 기준 제거 -------------------------

data_tb_word_prep %>% 
  count(단어) %>% 
  filter(!n >= 2) -> anti_word
data_tb_word_prep %<>% anti_join(anti_word) #  30,702
data_tb_word_prep # 30,702


# 5.1.1. TF-IDF: 초기 ---------------------s----------------
행위자_초기
data_tb_word_prep %>% 
  filter(시기 == "초기") %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령","민주당_이인영", "민주당"), "중립", 
                          ifelse(정보원 %in% c("민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "민주당_박원순_광역자치단체", "정의당", "전문가집단_윤형중"), "찬성", "반대"))) %>% 
  filter(정보원 %in% c("청와대", "문재인대통령", "민주당_이인영", "민주당", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "민주당_박원순_광역자치단체", "정의당", "전문가집단_윤형중", "기획재정부_홍남기", "통합당_심재철", "기획재정부", "통합당_황교안", "국무총리_정세균", "광역자치단체_대구시")) %>% 
  count(연합구분, 단어, sort = T) %>% 
  bind_tf_idf(단어, 연합구분, n) -> data_tf_idf_초기

# 5.1.2. weighted_log_odds비: 초기 ------------------------
data_tf_idf_초기 %>% 
  bind_log_odds(set = 연합구분,
                feature = 단어,
                n = n) %>% 
  rename(log_odds = "log_odds_weighted") -> data_tf_idf_odds_초기
data_tf_idf_odds_초기

# 5.1.3. 단순빈도분석: 초기 ---------------------------------------
data_tf_idf_odds_초기 %>% # 단순빈도분석: 찬성
  filter(연합구분 == "찬성") %>% 
  mutate(단어 = reorder(단어, n)) %>%
  slice_max(n, n = 10,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.2) +
  xlab("") + ylab("") +
  ylim(0, 70) -> 빈도분석_초기_찬성

data_tf_idf_odds_초기 %>% # 단순빈도분석: 중립
  filter(연합구분 == "중립") %>% 
  mutate(단어 = reorder(단어, n)) %>%
  slice_max(n, n = 10,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.2) +
  xlab("") + ylab("") +
  ylim(0, 70) -> 빈도분석_초기_중립

data_tf_idf_odds_초기 %>% # 단순빈도분석: 반대
  filter(연합구분 == "반대") %>% 
  mutate(단어 = reorder(단어, n)) %>%
  slice_max(n, n = 10,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.2) +
  xlab("") + ylab("") +
  ylim(0, 70) -> 빈도분석_초기_반대


# 5.1.4. 가중로그승산비분석 : 초기 ----------------------------------
data_tf_idf_odds_초기 %>% # 단순빈도분석: 찬성
  filter(연합구분 == "찬성") %>% 
  mutate(단어 = reorder(단어, log_odds)) %>%
  slice_max(log_odds, n = 10,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, log_odds), y = log_odds)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(log_odds, digits = 4)), hjust = 1.2) +
  xlab("") + ylab("") -> 가중로그승산비분석_초기_찬성

data_tf_idf_odds_초기 %>% # 단순빈도분석: 중립
  filter(연합구분 == "중립") %>% 
  mutate(단어 = reorder(단어, log_odds)) %>%
  slice_max(log_odds, n = 10,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, log_odds), y = log_odds)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(log_odds, digits = 4)), hjust = 1.2) +
  xlab("") + ylab("") -> 가중로그승산비분석_초기_중립

data_tf_idf_odds_초기 %>% # 단순빈도분석: 반대
  filter(연합구분 == "반대") %>% 
  mutate(단어 = reorder(단어, log_odds)) %>%
  slice_max(log_odds, n = 10,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, log_odds), y = log_odds)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(log_odds, digits = 4)), hjust = 1.2) +
  xlab("") + ylab("") -> 가중로그승산비분석_초기_반대

grid.arrange(행위자_초기,
                   arrangeGrob(
                     빈도분석_초기_반대,
                      빈도분석_초기_중립,
                      빈도분석_초기_찬성,
                   ncol = 1),
                   arrangeGrob(
                     가중로그승산비분석_초기_반대,
                      가중로그승산비분석_초기_중립,
                      가중로그승산비분석_초기_찬성,
                   ncol = 1),
  ncol=3, widths = c(1.5,1,1))


# 5.1.5. 테이블 ------------------------------------------

data_tf_idf_odds_초기   %>%
  filter(연합구분  == "찬성") %>%
  slice_max(n, n = 20, with_ties = F) %>%
  select(단어, n) %>%
  rename(단어_초기_빈도_찬성   =   단어,   출현빈도_초기_찬성   = n) %>%
  bind_cols(
    data_tf_idf_odds_초기   %>%
      filter(연합구분  == "찬성") %>%
      slice_max(log_odds, n = 20, with_ties = F) %>%
      select(단어, log_odds) %>%
      rename(단어_초기_가중로그승산비_찬성   =   단어,   가중로그승산비_초기_찬성   = log_odds)
  ) %>%
  bind_cols(
    data_tf_idf_odds_초기   %>%
      filter(연합구분  == "중립") %>%
      slice_max(n, n = 20, with_ties = F) %>%
      select(단어, n) %>%
      rename(단어_초기_빈도_중립   =   단어,   출현빈도_초기_중립   = n)
  ) %>%
  bind_cols(
    data_tf_idf_odds_초기   %>%
      filter(연합구분  == "중립") %>%
      slice_max(log_odds, n = 20, with_ties = F) %>%
      select(단어, log_odds) %>%
      rename(단어_초기_가중로그승산비_중립   =   단어,   가중로그승산비_초기_중립   = log_odds)
  ) %>%
  bind_cols(
    data_tf_idf_odds_초기   %>%
      filter(연합구분  == "반대") %>%
      slice_max(n, n = 20, with_ties = F) %>%
      select(단어, n) %>%
      rename(단어_초기_빈도_반대   =   단어,   출현빈도_초기_반대   = n)
  ) %>%
  bind_cols(
    data_tf_idf_odds_초기   %>%
      filter(연합구분  == "반대") %>%
      slice_max(log_odds, n = 20, with_ties = F) %>%
      select(단어, log_odds) %>%
      rename(단어_초기_가중로그승산비_반대   =   단어,   가중로그승산비_초기_반대   = log_odds)
  ) %>%
  write_excel_csv("빈도_가중로그승산비_테이블_초기.csv")



# 5.2.1. TF-IDF: 중기 -------------------------------------
data_tb_word_prep %>% 
  filter(시기 == "중기") %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균"), "찬성", 
                          ifelse(정보원 %in% c("통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부"), "반대", "중립"))) %>%
  filter(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균", "통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부")) %>% 
  count(연합구분, 단어, sort = T) %>% 
  bind_tf_idf(단어, 연합구분, n) -> data_tf_idf_중기

# 5.2.2. weighted_log_odds: 중기 ------------------------
data_tf_idf_중기 %>% 
  bind_log_odds(set = 연합구분,
                feature = 단어,
                n = n) %>% 
  rename(log_odds = "log_odds_weighted") -> data_tf_idf_odds_중기
data_tf_idf_odds_중기

# 5.2.3. 단순빈도분석: 중기 ---------------------------------------
data_tf_idf_odds_중기 %>% # 단순빈도분석: 찬성
  filter(연합구분 == "찬성") %>% 
  mutate(단어 = reorder(단어, n)) %>%
  slice_max(n, n = 20,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.2) +
  xlab("") + ylab("") +
  ggtitle("찬성") -> 빈도분석_중기_찬성

data_tf_idf_odds_중기 %>% # 단순빈도분석: 중립
  filter(연합구분 == "중립") %>% 
  mutate(단어 = reorder(단어, n)) %>%
  slice_max(n, n = 20,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.2) +
  xlab("") + ylab("") +
  ggtitle("중립") -> 빈도분석_중기_중립

data_tf_idf_odds_중기 %>% # 단순빈도분석: 반대
  filter(연합구분 == "반대") %>% 
  mutate(단어 = reorder(단어, n)) %>%
  slice_max(n, n = 20,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.2) +
  xlab("") + ylab("") +
  ggtitle("반대") -> 빈도분석_중기_반대

# 5.2.4. 가중로그승산비분석 : 중기 ----------------------------------
data_tf_idf_odds_중기 %>% # 단순빈도분석: 찬성
  filter(연합구분 == "찬성") %>% 
  mutate(단어 = reorder(단어, log_odds)) %>%
  slice_max(log_odds, n = 20,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, log_odds), y = log_odds)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(log_odds, digits = 4)), hjust = 1.2) +
  xlab("") + ylab("") -> 가중로그승산비분석_중기_찬성

data_tf_idf_odds_중기_총선이 %>% # 단순빈도분석: 중립
  filter(연합구분 == "중립") %>% 
  mutate(단어 = reorder(단어, log_odds)) %>%
  slice_max(log_odds, n = 20,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, log_odds), y = log_odds)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(log_odds, digits = 4)), hjust = 1.2) +
  xlab("") + ylab("") -> 가중로그승산비분석_중기_중립

data_tf_idf_odds_중기 %>% # 단순빈도분석: 반대
  filter(연합구분 == "반대") %>% 
  mutate(단어 = reorder(단어, log_odds)) %>%
  slice_max(log_odds, n = 20,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(단어, log_odds), y = log_odds)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(log_odds, digits = 4)), hjust = 1.2) +
  xlab("") + ylab("") -> 가중로그승산비분석_중기_반대

grid.arrange(
  빈도분석_중기_찬성,
  빈도분석_중기_반대,
  가중로그승산비분석_중기_찬성,
  가중로그승산비분석_중기_반대,
  ncol=4)



# 5.2.5. 테이블 ------------------------------------------

data_tf_idf_odds_중기   %>%
  filter(연합구분  == "찬성") %>%
  slice_max(n, n = 20, with_ties = F) %>%
  select(단어, n) %>%
  rename(단어_중기_빈도_찬성   =   단어,   출현빈도_중기_찬성   = n) %>%
  bind_cols(
    data_tf_idf_odds_중기   %>%
      filter(연합구분  == "찬성") %>%
      slice_max(log_odds, n = 20, with_ties = F) %>%
      select(단어, log_odds) %>%
      rename(단어_중기_가중로그승산비_찬성   =   단어,   가중로그승산비_중기_찬성   = log_odds)
  ) %>%
  bind_cols(
    data_tf_idf_odds_중기   %>%
      filter(연합구분  == "반대") %>%
      slice_max(n, n = 20, with_ties = F) %>%
      select(단어, n) %>%
      rename(단어_중기_빈도_반대   =   단어,   출현빈도_중기_반대   = n)
  ) %>%
  bind_cols(
    data_tf_idf_odds_중기   %>%
      filter(연합구분  == "반대") %>%
      slice_max(log_odds, n = 20, with_ties = F) %>%
      select(단어, log_odds) %>%
      rename(단어_중기_가중로그승산비_반대   =   단어,   가중로그승산비_중기_반대   = log_odds)
  ) %>%
  write_excel_csv("빈도_가중로그승산비_테이블_중기.csv")

# 5.3.1. TF-IDF: 후기 -------------------------------------
data_tb_word_prep %>% 
  filter(시기 == "후기") %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균", "민주당_이근형", "민주당_박주민", "민주당_조정식", "정부", "더불어시민당"), "찬성", 
                          ifelse(정보원 %in% c("통합당_조경태", "통합당_심재철", "통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부"), "반대", "중립"))) %>% 
  filter(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균", "민주당_이근형", "민주당_박주민", "민주당_조정식", "정부", "더불어시민당", "통합당_조경태", "통합당_심재철", "통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부")) %>% 
  count(연합구분, 단어, sort = T) %>% 
  bind_tf_idf(단어, 연합구분, n) -> data_tf_idf_후기

# 5.3.2. weighted_log_odds: 후기 ------------------------
data_tf_idf_후기 %>% 
  bind_log_odds(set = 연합구분,
                feature = 단어,
                n = n) %>% 
  rename(log_odds = "log_odds_weighted") -> data_tf_idf_odds_후기
data_tf_idf_odds_후기

# 5.3.3. 테이블 ------------------------------------------

data_tf_idf_odds_후기   %>%
  filter(연합구분  == "찬성") %>%
  slice_max(n, n = 20, with_ties = F) %>%
  select(단어, n) %>%
  rename(단어_후기_빈도_찬성   =   단어,   출현빈도_후기_찬성   = n) %>%
  bind_cols(
    data_tf_idf_odds_후기   %>%
      filter(연합구분  == "찬성") %>%
      slice_max(log_odds, n = 20, with_ties = F) %>%
      select(단어, log_odds) %>%
      rename(단어_후기_가중로그승산비_찬성   =   단어,   가중로그승산비_후기_찬성   = log_odds)
  ) %>%
  bind_cols(
    data_tf_idf_odds_후기   %>%
      filter(연합구분  == "반대") %>%
      slice_max(n, n = 20, with_ties = F) %>%
      select(단어, n) %>%
      rename(단어_후기_빈도_반대   =   단어,   출현빈도_후기_반대   = n)
  ) %>%
  bind_cols(
    data_tf_idf_odds_후기   %>%
      filter(연합구분  == "반대") %>%
      slice_max(log_odds, n = 20, with_ties = F) %>%
      select(단어, log_odds) %>%
      rename(단어_후기_가중로그승산비_반대   =   단어,   가중로그승산비_후기_반대   = log_odds)
  ) %>%
  write_excel_csv("빈도_가중로그승산비_테이블_후기.csv")




# # 미국 추이 살펴보기 ------------------------------------------
# 
# 
# predata_ver2 %>% 
#   filter(정보원 %>%  str_detect("전문가집단_")) -> data_tb_미국
# 
# data_tb_미국 %>% 
#   select(일자, 정보원, 인용문) %>% 
#   mutate(일자_0317 = ifelse(month(일자) == 3 & day(일자) < 17, "미국이전",
#                           ifelse(month(일자) == 3 & day(일자) < 30, "미국이후", "완전이후"))) %>% 
#   filter(정보원 %>% str_detect("전문가집단_")) %>% 
#   count(정보원)
# 
# 
# data_tb_미국 %>% 
#   select(일자, 정보원, 인용문) %>% 
#   filter(정보원 %>% str_detect("전문가집단")) %>% 
#   mutate(인용문 = SimplePos09(인용문) %>% 
#               unlist() %>% 
#               paste(collapse = " ") %>% 
#               str_extract_all(regex('[^\\s]+/N')) %>%
#               paste(collapse = ' ') %>% 
#               str_remove_all('/N') %>% 
#               str_remove_all(stopping_ko_end)
#   ) %>% 
#   ungroup() %>%
#   unnest_tokens(단어, 인용문) %>% 
#   anti_join(stopping_ko) %>% 
#   filter(str_length(단어) > 1) %>% 
#   mutate(단어 = ifelse(단어 %>%  str_detect("미국"), "미국", 단어)) %>% 
#   group_by(일자) %>% 
#   count(단어) %>% 
#   arrange(desc(n)) -> 단어
# 
# 
# 단어 %>% 
#   arrange(일자) %>% 
#   filter(단어 %>% str_detect("미국")) %>% 
#   mutate(주차 = (lubridate::week(일자) - 8) %>% 
#              as.factor() %>% 
#              paste0("주차")) %>% 
#   ggplot(aes(x = fct_reorder(주차, 일자), y = n)) +
#   geom_col() +
#   ylim(0, 300)


# 자치단체 네트워크 -------------------------------------------


data_tb_word_prep %>% 
  filter(정보원 %>% str_detect("자치단체")) %>% 
  count(정보원) %>% 
  arrange(desc(n))

# 정보원 확인
data_tb %>% 
  filter(정보원 %>% str_detect("광역자치단체")) %>% 
  count(정보원) %>% 
  arrange(desc(n))


data_tb %>% 
  filter(정보원 %>% str_detect("광역자치단체")) %>% 
  count(시기) %>% ㅁ
  arrange(match(시기, c("초기", "중기", "후기"))) %>% 
  mutate(id = 1:3) %>% 
  ggplot(aes(x = fct_reorder(시기, id), y = n)) +
  geom_col() +
  xlab("시기") +
  ylab("담론 빈도") +
  geom_text(aes(label = n), vjust = -1) +
  ylim(0, 200)

# 전체 출현 (광역자치단체)
data_tb %>% 
  count(시기) %>% 
  arrange(match(시기, c("초기", "중기", "후기"))) %>% 
  mutate(id = 1:3) %>% 
  ggplot(aes(x = fct_reorder(시기, id), y = n)) +
  geom_col() +
  xlab("시기") +
  ylab("담론 빈도") +
  geom_text(aes(label = n), vjust = -1)

data_tb_word_prep %>% 
  filter(정보원 %>% str_detect("광역자치단체")) %>% 
  filter(시기 %in% c("초기", "중기")) %>% 
  widyr::pairwise_count(item = 단어,
                        feature = id,
                        sort = T)

data_tb_word_prep %>% 
  filter(정보원 %>% str_detect("광역자치단체")) %>% 
  filter(시기 %in% c("초기", "중기")) %>% 
  widyr::pairwise_count(item = 단어,
                        feature = id,
                        sort = T) %>% 
  filter(n >= 15) %>% 
  as_tbl_graph(directed = F) %>% 
  ggraph::ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, family = 'AppleGothic') 

data_tb_word_prep %>% 
  filter(정보원 %>% str_detect("광역자치단체")) %>% 
  filter(시기 %in% c("초기", "중기")) %>% 
  widyr::pairwise_count(item = 단어,
                        feature = id,
                        sort = T) %>% 
  filter(n >= 15) %>% 
  as_tbl_graph(directed = F) %>% 
  ggraph::ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, family = 'AppleGothic') 

data_tb_word_prep %>% 
  filter(정보원 %>% str_detect("광역자치단체")) %>% 
  filter(시기 %in% c("초기", "중기")) %>% 
  widyr::pairwise_count(item = 단어,
                        feature = id,
                        sort = T) %>% 
  filter(n >= 15) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(cent_dgr = centrality_degree(),
         cent_btw = centrality_betweenness(),
         cent_cls = centrality_closeness(),
         cent_egn = centrality_eigen(),
         cent_wgt = centrality_pagerank(weights = n),
         group = as.factor(group_infomap())) -> data_pairs_graph
data_pairs_graph %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n,
                     edge_width = n),
                 edge_color = "darkred",
                 show.legend = F) +
  geom_node_point(aes(size = cent_wgt,
                      color = group)) +
  geom_node_text(aes(label = name), 
                 repel = T,
                 point.padding = unit(0.2, "lines"), family = 'AppleGothic') +
  theme_void()

data_pairs_graph %>% 
  as.data.frame() %>% 
  write_excel_csv("network.csv")


# topics <- c(2:10)
# data_lda_자치단체 <- topics %>% 
#   future_map(LDA, x = data_dtm_자치단체, control = list(seed = 1234))
# 
# data_lda_prep_자치단체 <- tibble(k = topics,
#                          perplex = map_dbl(data_lda_자치단체, 
#                                            perplexity))
# data_lda_prep_자치단체 %>%
#   ggplot(mapping = aes(x = k, 
#                        y = perplex)) +
#   geom_point() +
#   geom_line() +
#   ggplot2::geom_vline(xintercept = 6, size = 1, color = 'red', alpha = 0.7, linetype = 2)
# 
# data_lda_6_자치단체 <- LDA(data_dtm_자치단체, k=6, control=list(seed=1234))
# data_lda_6_자치단체 %>% 
#   tidy(matrix = "beta") -> data_topic_6_자치단체
# data_topic_6_자치단체 %>% 
#   group_by(topic) %>% 
#   slice_max(beta, n = 20) %>% 
#   ungroup() %>%
#   arrange(topic, -beta) -> data_topic_terms_자치단체
# data_topic_terms_자치단체  %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(mapping = aes(x = beta,
#                        y = term,
#                        fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap( ~ topic, scales = "free") +
#   scale_y_reordered()
# 
# data_tb %>% 
#   filter(인용문 %>% str_detect("부화")) 

data_tb_word_prep %>% 
  filter(시기 == "초기") %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령","민주당_이인영", "민주당"), "중립", 
                          ifelse(정보원 %in% c("민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "민주당_박원순_광역자치단체", "정의당", "전문가집단_윤형중"), "찬성", "반대"))) %>% 
  filter(정보원 %in% c("청와대", "문재인대통령", "민주당_이인영", "민주당", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "민주당_박원순_광역자치단체", "정의당", "전문가집단_윤형중", "기획재정부_홍남기", "통합당_심재철", "기획재정부", "통합당_황교안", "국무총리_정세균", "광역자치단체_대구시")) -> 단어_초기

data_tb_word_prep %>% 
  filter(시기 == "중기") %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균"), "찬성", 
                          ifelse(정보원 %in% c("통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부"), "반대", "중립"))) %>%
  filter(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균", "통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부")) -> 단어_중기

data_tb_word_prep %>% 
  filter(시기 == "후기") %>% 
  mutate(연합구분 = ifelse(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균", "민주당_이근형", "민주당_박주민", "민주당_조정식", "정부", "더불어시민당"), "찬성", 
                          ifelse(정보원 %in% c("통합당_조경태", "통합당_심재철", "통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부"), "반대", "중립"))) %>% 
  filter(정보원 %in% c("청와대", "문재인대통령", "민주당_김경수_광역자치단체", "민주당_이재명_광역자치단체", "정의당", "민주당", "민주당_이인영", "통합당_황교안", "민주당_이해찬", "민주당_이낙연", "광역자치단체_경기도", "민주당_박원순_광역자치단체", "민생당", "민주당_김경수_광역자치단체", "국무총리_정세균", "민주당_이근형", "민주당_박주민", "민주당_조정식", "정부", "더불어시민당", "통합당_조경태", "통합당_심재철", "통합당_김종인", "통합당_유승민", "통합당_박형준", "통합당", "기획재정부_홍남기", "기획재정부")) -> 단어_후기

단어_초기 %>% bind_rows(단어_중기) %>% bind_rows(단어_후기) %>% 
  filter(!연합구분 == "중립") %>% 
  group_by(연합구분) %>% 
  count(단어) %>% 
  arrange(desc(n)) %>% 
  filter(연합구분 == "찬성") %>% 
  filter(단어 %in% c("고소득층", "내수시장", "지역화폐", "경제활성화", "자영업자", "민생")) %>% 
  bind_rows(
    단어_초기 %>% bind_rows(단어_중기) %>% bind_rows(단어_후기) %>% 
  filter(!연합구분 == "중립") %>% 
  group_by(연합구분) %>% 
  count(단어) %>% 
  arrange(desc(n)) %>% 
  filter(연합구분 == "반대") %>% 
  filter(단어 %in% c("포퓰리즘", "재정", "재정건전성", "국채", "재원", "살포", "세금")) 
  ) %>% 
  ggplot(aes(x = fct_reorder(단어, n), y = n, fill = 연합구분)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~연합구분, ncol = 2, scales = "free") +
  geom_text(aes(label = n), hjust = 1.2) +
  xlab("") + ylab("")

data_tb_word_prep %>% 
  count(단어) %>% 
  arrange(desc(n)) %>% 
  filter(단어 %in% c("재정건전성", "건전"))
