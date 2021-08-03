
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
  filter(!정보원  %in% c("unknownactor", NA)) %>%
  select(정보원, 인용문) %>%
  filter(정보원 %>%  str_detect("김 의원")) 


rawdata %>%
  filter(!정보원 %in% c("unknownactor", NA)) %>%
  mutate(
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
    정보원 = ifelse(정보원 %>%  str_detect("조경태"), "민주당_조경태", 정보원),
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
    정보원 = ifelse(정보원 %>%  str_detect("구리"), "기초자치단체_구리시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("구리"), "기초자치단체_구리시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("인천시"), "광역자치단체_인천시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("제주"), "광역자치단체_제주시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("군수|시장|구청장"), paste0("기초자치단체_",정보원), 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("경기도"), "광역자치단체_경기도", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("울산시"), "광역자치단체_울산시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("경남"), "광역자치단체_경상남도", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("부산|변성완 시장 권한대행"), "광역자치단체_부산시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("대구"), "광역자치단체_대구시", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("총리"), "국무총리_정세균", 정보원),
    정보원 = ifelse(정보원 %>%  str_detect("경제|연구|교수|학회"), paste0("전문가집단_",정보원), 정보원),
  ) %>% 
  filter(!정보원 %>%  str_detect("김 씨|코로나|관계자|국민|김 의원")) -> predata_ver2

predata_ver2 %>% 
  filter(정보원 %>%  str_detect("안 대표")) %>% 
  select(정보원) %>% 
  count(정보원) %>% 
  arrange(desc(n)) %>% 
  as.data.frame()

predata_ver2 %>% 
  select(정보원) %>% 
  count(정보원) %>% 
  arrange(desc(n)) %>% 
  filter(n > 5) %>% 
  select(정보원) -> data_upper5

data_upper5 %>% 
  left_join(predata_ver2, by = "정보원") -> data_tb

data_tb
