# Title
정책의 연속과 변동: 전국민 재난지원금 아이디어와 채택요인 분석 
Policy Continuity and Change: Ideas and Adoption Factors for Covid-19 Assistance Fund

## Abstract
This study analyzed the process of formulating the first and second supplementary budgets in 2020 from the aspects of policy continuity and change. Compared to the previous policy tools during the financial crisis of 2008, the first supplementary budget in March 2020 had continuity with that of the financial crisis times. However, in the second supplementary budget bill of April 2020, a Covid-19 assistance fund was proposed as a new innovative policy tool. This study identified the introduction of Covid-19 assistance funds as a policy change. It analyzed the policy-making process from the perspective of discursive institutionalism, which emphasizes the importance of ideas in policy change. In order to identify the idea of a disaster assistance fund, this study used quantitative text mining techniques to extract keywords that appeared in the discourse. This study found that three factors have influenced policy change: 1) the existence of policy entrepreneurs (the heads of regional governments), 2) a political event (the general election on April 15), and 3) political control over the administration. The government (the Ministry of Economy and Finance), the ruling party, and the opposition party competed for the ideas of selective and universal support, and finally, universal support for all people was adopted. In addition, this study tried to supplement the qualitative case study method more firmly by combining quantitative text mining analysis in researching policy cases in terms of research methodology.

   
## Keywords
Discursive Institutionalism, Policy Change, Policy Idea, Text Mining, Covid-19 Assistance Fund, Policy Entrepreneur

### 1. 분석 방법
정책의 변동 관점에서 재난지원금이라고 하는 새로운 아이디어가 선택된 요인을 파악하기 위해 행위자와 행위자들의 상호작용하는 담론을 텍스트마이닝 기법을 활용하여 분석하였다. 구체적으로 단어빈도분석을 통해 어떤 단어가 담론 내에서 얼마나 빈번하게 등장하고 있는지 알아보고, 가중로그승산비 분석을 통해 특정 시점 전후로 나타난 담론의 차이를 분석하였다. 분석에 활용한 데이터는 재난지원금 논의가 처음 언론에 등장한 2020년 2월 27일부터 전국민 재난지원금 지급이 집행되기 시작한 5월 4일까지 발행된 기사를 대상으로 하였다. 데이터 수집의 편향성을 보정하기 위해 조선일보, 한국일보, 경향신문 세 곳의 언론사의 기사 및 인용문을 한국언론진흥재단의 빅카인즈(www.bigkinds.or.kr)를 통해 수집하였다. ‘재난지원금’, ‘재난기본소득’, ‘긴급재난지원금’ 등의 키워드 검색으로 총 1,306건의 뉴스기사를 추출하였고, 이에 포함된 2,861건의 인용문을 추출하였다. 이후 중복된 데이터를 제외한 후 인용문의 행위자를 데이터 정제과정을 통해 통합하였다. 정제된 데이터 2,292건 중 해당 기간 내 발언 횟수가 3회 이상인 행위자만을 선별하고 재난지원금 담론과 무관한 인용문을 제거하여 최종적으로 1,499건의 인용문을 추출할 수 있었다. 해당 인용문을 형태소분석을 통해 명사를 추출하여 총 47,251개의 단어로 구성된 데이터 셋을 확보하였다. 이후 정제과정을 거쳐 최종적으로 30,702개의 명사로 이루어진 단어 데이터로 분석을 실시하였다. 데이터의 전처리 및 분석은 통계 프로그램 R 4.1.0과 RStudio 1.4.1717을 활용하였으며(R Core Team, 2021; RStudio Team, 2021), 한국어 형태소 분석에는 한글형태소 사전인 NIAdic을 활용하였다(NIA and Jeon, 2016).  
분석을 위해 tidyverse(Wickham et al., 2019), lubridate(Grolemund and Wickham, 2011), tidylo(Schnoebelen et al., 2020), tidytext(Silge, 2016), KoNLP(Jeon, 2016) 등의 패키지를 사용하였다.

### 2. 분석 시각화
[그림 1] 긴급재난지원금 지급 담론 가중로그승산비 상위 단어 빈도 비교(전체 시기)
![image](https://user-images.githubusercontent.com/75797388/162100945-14148301-f5ac-438d-889d-ae83ce5bdb85.png)  
  
  
[그림 2] 주요 행위자 출현 빈도 (2월 말 – 총선 이전)  
초기  
![image](https://user-images.githubusercontent.com/75797388/162101017-30d0a08e-2283-4cf6-a592-73ce32b5944f.png)
  
중기  
![image](https://user-images.githubusercontent.com/75797388/162101033-5b72c789-9eae-4f21-8de0-3bbf4e2b956a.png)
  
  
[그림 3] 자치단체장 담론 네트워크 분석 (2월 말 – 총선 이전)  
![image](https://user-images.githubusercontent.com/75797388/162101112-264ff737-d1f1-4868-92db-7973c580a7d6.png)

### 3. 분석 결과
논문 참조  
