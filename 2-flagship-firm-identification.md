# 2. 기함 기업 리스트 선정

## 2.1. 개요

한국 기업 생태계의 수직적 계층화를 수행하려면 네트워크의 시작 지점이 되는 공급망 최상위 기업을 식별해야합니다. 이러한 기업들을 "기함 기업(flagship enterprise)"라고 부르며, 이들을 선정하는 기준으로는 매출과 네트워크 영향력 등이 있습니다. 

이러한 기함 기업 식별에 있어서 가장 핵심적으로 부딪히는 과제는 '현실적인' 기함 기업 목록을 선정하기 위한 기준을 만드는 것입니다. 예를 들어 매출 순위는 우리가 생각하는 '기업 순위'의 이미지에 가깝지만, 실제 공급망 구조를 반영하고 있는지는 불확실합니다. 반면, 네트워크 영향력을 기준으로만 선별하면 방법론이나 데이터의 한계로 인해 핵심 기업이 누락되는 문제가 발생합니다. 

따라서, 본 연구에서는 두 가지 기준을 모두 활용하여 초기 기함 기업 목록(n =100)을 선정하였습니다. 

### 2.1.1 복합 순위 산출

기업의 네트워크 내 위치와 경제적 규모를 동시에 고려하기 위해 PageRank와 매출액 순위의 기하평균을 활용하였습니다. 기하평균은 극단적인 순위 차이를 조정하고, 두 지표 간의 균형을 달성하는데 효과적입니다.

```R
geometric_mean_rank = sqrt(rank_pagerank * rank_sales)
```

### 2.1.2 조건부 선정 프로세스
두 단계의 필터링 과정을 통해 균형 잡힌 기함기업 후보군을 선정합니다. 

1. **네트워크 중심성 기반 선정**
   - PageRank 상위 100위 이내
   - 매출액 상위 30% 이상

2. **경제적 규모 기반 선정**
   - 매출액 상위 100위 이내
   - PageRank 상위 200위 이내

이 두 그룹을 통합한 후, 기하평균 순위를 기준으로 최종 100개 기업을 선정합니다. 

## 2.2. 실제 구현

### 2.2.1. 기본 네트워크 구성

```R
## 필요 패키지 로드
library(igraph)
library(tidyverse)
library(visNetwork)

## 데이터 읽기

network_data <- read.csv("your_data_path/KED_2022_ntwk_data.csv", 
                        encoding="UTF-8", sep=";", quote="'")
network_data$KEDCD <- as.character(network_data$KEDCD)
network_data$KEDCD_TRD <- as.character(network_data$KEDCD_TRD)

## edges 데이터프레임 준비
edges_df <- network_data[!is.na(network_data$KEDCD) & !is.na(network_data$KEDCD_TRD), 
                        c("KEDCD", "KEDCD_TRD")]

### 모든 ID를 문자열로 변환
edges_df$KEDCD <- as.character(edges_df$KEDCD)
edges_df$KEDCD_TRD <- as.character(edges_df$KEDCD_TRD)

## 기본 네트워크 생성
g_basic <- graph_from_data_frame(d = edges_df, directed = TRUE)


## (추가) 소기업 절사

### 매출 분포 확인
print("=== 매출 분포 ===")
print(summary(network_data$SALES))

### 매출 하위 25% 기준 설정
sales_cutoff <- quantile(network_data$SALES, 0.25, na.rm = TRUE)
print(paste("매출 기준값:", sales_cutoff))

### 필터링된 기업 목록 생성
filtered_firms <- network_data %>%
  filter(!is.na(ENP_NM), SALES >= sales_cutoff) %>%
  distinct(KEDCD) %>%
  pull(KEDCD)

### 필터링된 네트워크 생성
edges_filtered <- network_data %>%
  filter(KEDCD %in% filtered_firms,
         KEDCD_TRD %in% filtered_firms,
         !is.na(KEDCD), 
         !is.na(KEDCD_TRD)) %>%
  select(KEDCD, KEDCD_TRD)

edges_filtered$KEDCD <- as.character(edges_filtered$KEDCD)
edges_filtered$KEDCD_TRD <- as.character(edges_filtered$KEDCD_TRD)

g_filtered <- graph_from_data_frame(d = edges_filtered, directed = TRUE)


# 2. 중심성 지표 계산

## 기본 중심성 지표 계산

calculate_centrality <- function(g) {
  data.frame(
    KEDCD = V(g)$name,
    degree_total = degree(g, mode = "total"),
    degree_in = degree(g, mode = "in"),
    degree_out = degree(g, mode = "out"),
    pagerank = page_rank(g)$vector,
  )
}

centrality_filtered <- calculate_centrality(g_basic)

## 기업 정보 추가
network_data$KEDCD <- as.character(network_data$KEDCD)
centrality_filtered$KEDCD <- as.character(centrality_filtered$KEDCD)
centrality_filtered <- centrality_filtered %>%
  left_join(
    network_data %>% 
      select(KEDCD, ENP_NM, SALES) %>% 
      distinct(KEDCD, .keep_all = TRUE),
    by = "KEDCD"
  )

### 중간 결과 확인
print("=== 기본 중심성 계산 결과 (상위 10개 기업) ===")
print(centrality_filtered %>% 
      arrange(desc(degree_total)) %>% 
      head(10))

### 상위 연결기업 중 결측기업 확인
top_missing_firms <- centrality_filtered %>%
  filter(is.na(ENP_NM)) %>%
  arrange(desc(degree_total)) %>%
  select(KEDCD, degree_total, degree_in, degree_out, pagerank) %>%
  head(10)
```


### 2.2.2. 매출 순위+ PageRank 순위
- 매출 순위로만 정렬하면 금융 기업이 상위에 노출
- 네트워크 .영향력 순위로만 정렬하면 몇몇 대기업이 하위 계층으로 내려가는 문제

```R
rank_comparison <- centrality_filtered %>%
  # NA 매출 제외
  filter(!is.na(SALES)) %>%
  # 각 지표의 순위 계산
  mutate(
    rank_pagerank = rank(-pagerank),
    rank_sales = rank(-SALES),
    # 순위 차이 계산 (양수: 매출순위가 더 높음, 음수: pagerank 순위가 더 높음)
    rank_diff = rank_sales - rank_pagerank,
    # 매출액 백분위 계산 (상위 몇 %인지)
    sales_percentile = percent_rank(SALES),
    # log 매출액 (규모 차이 비교용)
    log_sales = log(SALES)
  )
## 2. 기초 통계 출력
print("=== 매출액 상위 20개 기업의 PageRank 순위 ===")
print(rank_comparison %>%
      arrange(rank_sales) %>%
      select(KEDCD, ENP_NM, SALES, rank_sales, rank_pagerank, rank_diff) %>%
      head(20))

print("\n=== PageRank 상위 20개 기업의 매출액 순위 ===")
print(rank_comparison %>%
      arrange(rank_pagerank) %>%
      select(KEDCD, ENP_NM, SALES, rank_sales, rank_pagerank, rank_diff) %>%
      head(20))

## 3. 극단적인 차이를 보이는 기업들 확인
print("\n=== PageRank 대비 매출순위가 크게 높은 기업 (상위 20개) ===")
print(rank_comparison %>%
      filter(rank_pagerank > 100) %>%  # PageRank 100위 밖이면서
      arrange(rank_sales) %>%          # 매출은 높은 순
      select(KEDCD, ENP_NM, SALES, rank_sales, rank_pagerank, rank_diff) %>%
      head(100))

print("\n=== 매출순위 대비 PageRank가 크게 높은 기업 (상위 20개) ===")
print(rank_comparison %>%
      filter(rank_sales > 100) %>%     # 매출 100위 밖이면서
      arrange(rank_pagerank) %>%       # PageRank는 높은 순
      select(KEDCD, ENP_NM, SALES, rank_sales, rank_pagerank, rank_diff) %>%
      head(100))

## 4. 균형잡힌 상위기업 선정
### 방법 1: 두 순위의 기하평균을 사용
balanced_rankings <- rank_comparison %>%
  mutate(
    # 기하평균 순위 계산 (양쪽 모두 고려)
    geometric_mean_rank = sqrt(rank_pagerank * rank_sales),
    # 두 순위 중 더 나쁜 순위 사용
    worst_rank = pmax(rank_pagerank, rank_sales),
    # 매출 상위 X% 내 여부
    is_top_sales = sales_percentile >= 0.9  # 상위 10% 기준
  )

### 방법 2: 조건부 선정
balanced_top_100 <- balanced_rankings %>%
  # 1단계: PageRank 상위 100개 중 매출도 어느정도 높은 기업
  filter(
    rank_pagerank <= 100 &           # PageRank 상위 100
    sales_percentile >= 0.7          # 매출 상위 30% 이상
  ) %>%
  # 2단계: 매출 상위 100개 중 PageRank도 어느정도 높은 기업
  bind_rows(
    balanced_rankings %>%
    filter(
      rank_sales <= 100 &            # 매출 상위 100
      rank_pagerank <= 200           # PageRank 상위 200 이내
    )
  ) %>%
  distinct() %>%                     # 중복 제거
  arrange(geometric_mean_rank) %>%   # 기하평균 순위로 정렬
  head(100)                         # 상위 100개 선정

### 5. 결과 출력
print("\n=== 균형잡힌 상위 100개 기업 중 상위 20개 ===")
print(balanced_top_100 %>%
      select(KEDCD, ENP_NM, SALES, rank_sales, rank_pagerank, 
             geometric_mean_rank) %>%
      head(100))

## 6. 기존 방식과 비교
print("\n=== 기존 vs 새로운 방식의 기업 구성 비교 ===")
original_top_100 <- rank_comparison %>%
  filter(rank_pagerank <= 100) %>%
  pull(KEDCD)

new_top_100 <- balanced_top_100 %>%
  pull(KEDCD)

#### 제외된 기업들
print("\n제외된 기업들 (기존 Top 100에는 있었으나 새로운 목록에서 제외):")
print(rank_comparison %>%
      filter(KEDCD %in% setdiff(original_top_100, new_top_100)) %>%
      select(KEDCD, ENP_NM, SALES, rank_sales, rank_pagerank, rank_diff) %>%
      arrange(rank_pagerank))

#### 새로 추가된 기업들
print("\n새로 추가된 기업들 (새로운 목록에 추가된 기업들):")
print(rank_comparison %>%
      filter(KEDCD %in% setdiff(new_top_100, original_top_100)) %>%
      select(KEDCD, ENP_NM, SALES, rank_sales, rank_pagerank, rank_diff) %>%
      arrange(rank_sales))

#### 추가 분석: 산업별 분포 변화 (산업 코드가 있다면)
if("KSIC" %in% names(centrality_filtered)) {
  industry_comparison <- data.frame(
    original = table(centrality_filtered$KSIC[centrality_filtered$KEDCD %in% original_top_100]),
    balanced = table(centrality_filtered$KSIC[centrality_filtered$KEDCD %in% new_top_100])
  )
  print("\n=== 산업별 분포 변화 ===")
  print(industry_comparison)
  
## 7. 결과 저장
write.csv(balanced_top_100, 
          "your_data_path/balanced_top_firms_100.csv", 
          row.names = FALSE)
```
