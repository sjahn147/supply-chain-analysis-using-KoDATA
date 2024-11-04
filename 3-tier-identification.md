## 3.1. 개요

 한국 제조업 공급망에서의 tier 구조를 식별하기 위해 기존의 세 가지 주요 방법론을 통합적으로 발전시켰습니다. 

## 3.2. 계층 할당 방법론

### 3.2.1. 기존 방법론의 통합
본 연구는 세 가지 주요 방법론을 결합한 앙상블 접근법을 채택합니다. 

1. **개선된 홍장표 방식**
   - 1차 협력사 식별을 위한 복합 지표 도입
   - 기업 규모의 상대적 비교를 통한 계층 조정
     
2. **Gofman의 Upstreamness 방식**
   - 네트워크에서 기업 간 최단 경로를 기반으로 수직적 위계성(upstreamness)을 측정
   ```
   각 노드 v에 대해, upstreamness(v) = min(distance(v, f)) for f ∈ flagship firms
   ```
-  기함기업으로부터의 거리를 고려하여 계층적 위치 파악
   

3. **Nakano & White의 CoSL(Core-Supplier Levels) 방식**
   - 군집 분석과 거래 패턴을 결합한 계층 구조 분석
   - 응집도(cohesion)와 거래 강도를 고려한 계층 할당
   - 각 군집 내에서 기업 규모와 연결 패턴을 고려한 세부 계층화

### 3.2.2. 최종 계층 결정 프로세스 요약
1. 각 방법론별 계층 산출
2. 가중치 적용된 통합 점수 계산
3. 임계값 기반 계층 조정

## 3.3. 실제 구현

먼저 홍장표(2015)의 방법론을 따라 flagship 기업들과의 직접적 거래관계를 기반으로 initial tier를 설정합니다. 한국 제조업의 실제 거래구조를 반영하기 위한 기본 틀이 된다고 할 수 있습니다. 

### 3.3.1. 초기 계층화 단계

#### 3.3.1.1.단순 네트워크 거리 기준 계산
``` R
# 기함기업 정보 활용
flagship_kedcd <- balanced_top_100$KEDCD

# Tier 계산 함수 정의
calculate_tiers <- function(g, flagship_firms) {
  tiers <- rep(NA, vcount(g))
  flagship_vertex_indices <- match(flagship_firms, V(g)$name)
  
  # Tier 0 설정 및 최단 경로 계산
  tiers[flagship_vertex_indices] <- 0
  shortest_paths <- shortest.paths(g, v = flagship_vertex_indices, mode = "out")

  # Tier 0이 아닌 나머지 기업들의 tier 계산
  non_flagship_indices <- setdiff(1:vcount(g), flagship_vertex_indices)
  tiers[non_flagship_indices] <- apply(shortest_paths, 2, min)[non_flagship_indices]
  
  return(tiers)
}

# Tier 계산 적용 및 매핑
g_filtered$simple_tier <- calculate_tiers(g_filtered, flagship_kedcd) #계산 효율을 위해 매출 하위 25% 기업을 절사한 네트워크 이용

# Centrality 데이터프레임에 tier 정보 매핑 (vectorized)
vertex_indices <- match(centrality$KEDCD, V(g_filtered)$name)
centrality$simple_tier <- g_filtered$simple_tier[vertex_indices]
centrality$hong_tier <- g_filtered$hong_tier[vertex_indices]
```

계층 할당 결과의 품질을 확인하기 위한 함수 두 개를 정의합니다. 

```R
# Tier별 특성 분석 함수
analyze_tiers <- function(data, tier_column) {
  tier_summary <- data %>%
    group_by(across(all_of(tier_column))) %>%
    summarise(
      firm_count = n(),
      avg_pagerank = mean(pagerank, na.rm = TRUE),
      avg_degree = mean(degree_total, na.rm = TRUE),
      avg_sales = mean(SALES, na.rm = TRUE),
      median_sales = median(SALES, na.rm = TRUE)
    ) %>%
    arrange(across(all_of(tier_column)))
  
  return(tier_summary)
}


# 주요 기업 출력 함수
print_top_firms <- function(data, tier_column, tier_value, n = 10) {
  cat(sprintf("\nTier %d 주요 기업:\n", tier_value))
  data %>%
    filter(get(tier_column) == tier_value) %>%
    arrange(desc(pagerank)) %>%
    select(KEDCD, ENP_NM, SALES, pagerank, degree_total) %>%
    head(n) %>%
    print()
} 
```

단순 거리 기준으로 매핑된 계층별 특성입니다. 
```R
print("=== 단순거리 기준 Tier별 특성 ===")
print(analyze_tiers(centrality, "simple_tier"))

simple_tier firm_count avg_pagerank avg_degree    avg_sales median_sales
         <dbl>      <int>        <dbl>      <dbl>        <dbl>        <dbl>
 1           0        100   0.000117       548.   14811283354.  7555249650 
 2           1       7220   0.0000116       31.2     96466067.    23123658.
 3           2      13497   0.00000662      16.7    150521625.    29692729 
 4           3       4967   0.00000620      11.6     38099004.    19568564 
 5           4       1325   0.00000427       6.29    44578863.    20501005 
 6           5        109   0.00000499       6.66    29718883.    17332707 
 7           6         19   0.00000462       7       40606486.    20114122 
 8           7          5   0.00000416       6.4     26115821     15799894 
 9           8          1   0.00000940      18        4825000      4825000 
10         Inf        363   0.00000679       9.33    22543139.    14480988 
11          NA     122579   0.00000315       3.51     9448917.     1874965 
```

```R
cat("\n=== 단순거리 기준 주요 기업 ===")
for(t in sort(unique(centrality$simple_tier))) {
	if(!is.na(t)) print_top_firms(centrality, "simple_tier", t)
}

Tier 0 주요 기업:
       KEDCD               ENP_NM        SALES     pagerank degree_total
1    "74225"       "한국전력공사"  68951546000 0.0016805865         8565
2  "7587584"           "쿠팡(주)"  25768487000 0.0005471810         2434
3      "985"     "현대자동차(주)"  65308350000 0.0004069498         2132
4      "112" "씨제이대한통운(주)"   8214653866 0.0003630769         1671
5    "41119"         "(주)케이티"  18289243000 0.0003411930         1581
6    "91399"       "삼성전자(주)" 211867483000 0.0002842245         1298
7    "48950"         "네이버(주)"   5512586322 0.0002691549         1262
8    "66222"       "엘지전자(주)"  27791746000 0.0002209386          928
9      "597"           "기아(주)"  46409721000 0.0002032036         1028
10   "35910"   "(주)엘지유플러스"  12781569000 0.0001964255          869

Tier 1 주요 기업:
       KEDCD               ENP_NM      SALES     pagerank degree_total
1    "10728" "에스케이렌터카(주)" 1247543929 6.573015e-05          324
2   "608625"       "행복나래(주)"  988709582 6.252962e-05          300
3    "99290" "현대엘리베이터(주)" 1657192106 5.568259e-05          257
4     "3582"           "(주)한라" 1379461788 4.890190e-05          232
5     "6848"       "고려해운(주)" 4861875786 4.633564e-05          195
6  "7762965" "(주)농협하나로유통" 1247811265 4.508197e-05          206
7     "5156"       "삼성카드(주)" 3641606365 4.478904e-05          215
8     "2391"   "금호석유화학(주)" 5086855586 4.433535e-05          219
9  "7183228"     "동원로엑스(주)" 1214229533 4.238859e-05          157
10    "3036"   "(주)이랜드리테일" 1393224121 4.222157e-05          190

Tier 2 주요 기업:
       KEDCD               ENP_NM      SALES     pagerank degree_total
1    "62078"     "(주)우리홈쇼핑" 1077774067 6.758265e-05          264
2  "8136487"       "십일번가(주)"  789039303 6.342334e-05          323
3    "62313"     "(주)엔에스쇼핑"  550908072 6.073325e-05          280
4    "45484" "(주)케이지이니시스"  666957435 5.974623e-05          269
5  "8302155"       "푸디스트(주)"  770224373 5.416568e-05          222
6    "68247"     "(주)현대백화점" 1655713149 5.306802e-05          232
7    "91759"   "케이비캐피탈(주)" 1878370237 5.245379e-05          275
8     "1834"       "디엘건설(주)" 1962419335 5.083732e-05          248
9     "8466"     "하나캐피탈(주)" 1399848872 5.045118e-05          224
10    "1487"       "금호건설(주)" 2041695477 4.886795e-05          247
```

- **잘못된 매출 분포** :  `avg_sales` 를 보면 Tier 1보다 Tier 2의 평균 매출이 더 높아 정상적으로 매핑되지 않은 것을 확인할 수 있습니다.  -> 이는 대기업과 직접 거래하는 작은 업체가 Tier 1으로 직접 매핑되었기 때문입니다. 
- **계층이 너무 낮게 할당된 기업 문제**: 또, 계층별 주요 기업 목록을 뽑아보면 Tier 2에 규모가 큰 기업이 상당수 포함되어 있어 직관과 들어맞지 않습니다. 
- 다양한 기준을 활용한 필터링과 목록 조정이 필요하다고 할 수 있습니다. 

#### 3.3.1.2. 홍장표(2015)의 규모 기반 접근

3.3.1.1. 에서 확인한 문제를 홍장표(2015)의 방법에 따라 조정해보겠습니다. 

1) **필터링** : 복합 점수 체계를 도입해 1차 협력사 할당시 선별 과정을 추가합니다. 
2) **계층 이동성** : 낮은 계층에 초기 할당된 기업을 상향 조정하는 등 계층 이동성을 추가합니다. 

이 때,  선별이나 계층 이동성 추가는 모두 매출이나 종업원수 등 주로 기업의 '규모'를 기준으로 초기 할당 결과를 실제 데이터에 가깝게 조정하는 과정이라고 할 수 있습니다.  주된 고려 방법은 아래와 같습니다. 

- **규모의 상대성** : 규모의 절대값을 기준으로 컷오프를 설정하기보다 비율 컷오프를 설정합니다. 
- **거래 패턴** : 
	- 공급망 내 계층의 핵심 정의는 **구매>판매**인 기업일수록 계층 내에서 더 높은 위치를 차지하고 있다는 가정에 따릅니다. 
	- 정의상 Tier 0인 '기함 기업'은 오로지 공급만 받는 (즉 구매만 하는) 기업이라고 볼 수 있습니다. 
	- 이러한 가정을 조정 및 필터링에 반영하려면 구매/판매 비율을 점수 체계에 포함시켜야합니다. 

##### 3.3.1.2.1. 복합 점수 체계의 구현
다음 요소들의 가중 평균을 통해 각 기업의 종합 점수를 산출합니다. 

1. **네트워크 중요도 (40%)**
   - PageRank 점수 (20%)
   - 연결중심성 (20%)

2. **기업 규모 (30%)**
   - 매출액 (12%)
   - 종업원 수 (18%)

3. **거래 패턴 (30%)**
   - 판매/구매 비율
   - 거래 다각화 정도

**기본 점수 체계** 
```R
base_score = network_weight * (pagerank * 0.5 + degree_total * 0.5) +
             size_weight * (normalized_sales * sales_weight + 
                          normalized_laborer * (0.3 - sales_weight)) +
             trade_weight * trade_ratio
```
※ sales_weight을 별도로 조정하는 이유 :  매출 가중치가 너무 크게 되면 공급망 구조를 무시하고 금융업 기업이 네트워크 상층으로 이동하기 때문

##### 3.3.1.2.2. 1차 협력사 필터링의 구현

```R
    # 1차 협력사 선정 기준 적용
    flagship_median_sales <- median(V(g)$sales[flagship_vertex_indices], na.rm=TRUE)
    sales_ratio <- V(g)$sales[first_tier_candidates] / flagship_median_sales
    
    score_threshold <- mean(total_score, na.rm=TRUE)
    first_tier <- first_tier_candidates[
      total_score >= score_threshold & 
        sales_ratio >= (1/sales_ratio_threshold) &
        !is.na(total_score)
      ]
    
    # Tier 할당
    tiers[first_tier] <- 1
  }
```

##### 3.3.1.2.3. 계층 이동성의 구현

1. **상향 이동 기준**

1)  2차 이하

2차 협력사(Tier 2)보다 아래에 할당된 기업 중 상향 이동이 필요한 사례를 찾습니다. 
```R
  higher_tier_candidates <- which(tiers > 1 & !is.na(tiers))
  if(length(higher_tier_candidates) > 0) {
    # 상위 이동 대상 점수 계산
    network_score_higher <- with(data.frame(
      pagerank = V(g)$pagerank[higher_tier_candidates],
      degree = V(g)$degree_total[higher_tier_candidates]
    ), {
      normalize(pagerank) * 0.6 + normalize(degree) * 0.4
    })
    
    size_score_higher <- with(data.frame(
      sales = V(g)$sales[higher_tier_candidates],
      laborer = V(g)$laborer[higher_tier_candidates]
    ), {
      normalize(sales) * sales_weight + 
        normalize(laborer) * (business_size_weight - sales_weight)
    })
    
    trade_score_higher <- normalize(V(g)$trade_ratio[higher_tier_candidates])
    
    candidate_scores <- network_score_higher * network_importance_weight +
      size_score_higher * business_size_weight +
      trade_score_higher * trade_pattern_weight
    
    # 상위 5%는 한 단계 상향
    valid_scores <- !is.na(candidate_scores)
    if(sum(valid_scores) > 0) {
      promotion_threshold <- quantile(candidate_scores[valid_scores], 0.95, na.rm=TRUE)
      promote_indices <- higher_tier_candidates[valid_scores & 
                                                  candidate_scores >= promotion_threshold]
      
      if(length(promote_indices) > 0) {
        tiers[promote_indices] <- pmax(1, tiers[promote_indices] - 1)
        log_message(sprintf("Promoted %d firms to higher tier", length(promote_indices)))
      }
    }
```


2) 1차 기업 

- promotion_to_tier0 = TRUE로 설정한 경우
- 기함 기업(Tier 0)으로 승격이 필요한 사례를 다음 기준에 의해 찾습니다. 
	- 복합 점수 상위 1%는 tier 0 승격 고려
	- 기함기업 평균 네트워크 지표의 50% 이상 충족 필요

```R
    # Tier 0 승격 평가
    if(promotion_to_tier0) { 
      tier1_companies <- which(tiers == 1)
      if(length(tier1_companies) > 0) {
        network_score_tier1 <- with(data.frame(
          pagerank = V(g)$pagerank[tier1_companies],
          degree = V(g)$degree_total[tier1_companies]
        ), {
          normalize(pagerank) * 0.6 + normalize(degree) * 0.4
        })
        
        size_score_tier1 <- with(data.frame(
          sales = V(g)$sales[tier1_companies],
          laborer = V(g)$laborer[tier1_companies]
        ), {
          normalize(sales) * sales_weight + 
            normalize(laborer) * (business_size_weight - sales_weight)
        })
        
        trade_score_tier1 <- normalize(V(g)$trade_ratio[tier1_companies])
        
        tier1_scores <- network_score_tier1 * network_importance_weight +
          size_score_tier1 * business_size_weight +
          trade_score_tier1 * trade_pattern_weight
        
        valid_tier1 <- !is.na(tier1_scores)
        if(sum(valid_tier1) > 0) {
          tier0_threshold <- quantile(tier1_scores[valid_tier1], 0.99, na.rm=TRUE)
          
          flagship_avg_pagerank <- mean(V(g)$pagerank[flagship_vertex_indices], na.rm=TRUE)
          flagship_avg_degree <- mean(V(g)$degree_total[flagship_vertex_indices], na.rm=TRUE)
          
          promote_to_tier0 <- tier1_companies[
            valid_tier1 &
              tier1_scores >= tier0_threshold &
              V(g)$pagerank[tier1_companies] >= flagship_avg_pagerank * 0.5 &
              V(g)$degree_total[tier1_companies] >= flagship_avg_degree * 0.5
            ]
          
          if(length(promote_to_tier0) > 0) {
            tiers[promote_to_tier0] <- 0
            log_message(sprintf("Promoted %d firms to tier 0", length(promote_to_tier0)))
```


**전체 코드** 
```R
calculate_hong_tiers <- function(g, flagship_firms, 
                                       sales_ratio_threshold = 5,
                                       size_ratio_threshold = 3,
                                       network_importance_weight = 0.4,
                                       business_size_weight = 0.3,
                                       trade_pattern_weight = 0.3,
                                       sales_weight = 0.15,
                                       promotion_to_tier0 = TRUE) {
  
  # 기본 정보 설정
  tiers <- rep(NA, vcount(g))
  vertex_names <- V(g)$name
  
  # 기업 정보 매칭
  vertex_to_firm <- match(vertex_names, network_data$KEDCD)
  
  # 각종 지표 계산
  V(g)$sales <- network_data$SALES[vertex_to_firm]
  V(g)$laborer <- network_data$LABORER_SUM[vertex_to_firm]
  V(g)$pagerank <- page_rank(g)$vector
  V(g)$degree_in <- degree(g, mode = "in")
  V(g)$degree_out <- degree(g, mode = "out")
  V(g)$degree_total <- degree(g, mode = "total")
  V(g)$trade_ratio <- V(g)$degree_out / pmax(V(g)$degree_in, 1)
  
  # Flagship 기업 설정
  flagship_vertex_indices <- match(flagship_firms, vertex_names)
  tiers[flagship_vertex_indices] <- 0
  
  # 거리 행렬 계산
  dist_matrix <- distances(g, v=V(g), to=flagship_vertex_indices, mode="out")
  min_distances <- apply(dist_matrix, 1, min)
  
  # 1차 협력사 후보 선정 (2단계로 분리)
  first_tier_candidates <- which(min_distances == 1)
  first_tier_candidates <- setdiff(first_tier_candidates, flagship_vertex_indices)
  
  # 정규화 함수
  normalize <- function(x) {
    if(all(is.na(x))) return(rep(0, length(x)))
    (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
  }
  
  if(length(first_tier_candidates) > 0) {
    # 네트워크 중요도 점수
    network_score <- with(data.frame(
      pagerank = V(g)$pagerank[first_tier_candidates],
      degree = V(g)$degree_total[first_tier_candidates]
    ), {
      normalize(pagerank) * 0.6 + normalize(degree) * 0.4
    })
    
    # 기업 규모 점수
    size_score <- with(data.frame(
      sales = V(g)$sales[first_tier_candidates],
      laborer = V(g)$laborer[first_tier_candidates]
    ), {
      normalize(sales) * sales_weight + 
        normalize(laborer) * (business_size_weight - sales_weight)
    })
    
    # 거래 패턴 점수
    trade_score <- normalize(V(g)$trade_ratio[first_tier_candidates])
    
    # 종합 점수 계산
    total_score <- network_score * network_importance_weight +
      size_score * business_size_weight +
      trade_score * trade_pattern_weight
    
    # 1차 협력사 선정 기준 적용
    flagship_median_sales <- median(V(g)$sales[flagship_vertex_indices], na.rm=TRUE)
    sales_ratio <- V(g)$sales[first_tier_candidates] / flagship_median_sales
    
    score_threshold <- mean(total_score, na.rm=TRUE)
    first_tier <- first_tier_candidates[
      total_score >= score_threshold & 
        sales_ratio >= (1/sales_ratio_threshold) &
        !is.na(total_score)
      ]
    
    # Tier 할당
    tiers[first_tier] <- 1
  }
  
  # 나머지 기업들의 Tier 할당
  remaining_vertices <- setdiff(1:vcount(g), c(flagship_vertex_indices, which(tiers == 1)))
  tiers[remaining_vertices] <- min_distances[remaining_vertices]
  
  # 상위 이동 대상 평가 (2차 이상 협력사)
  higher_tier_candidates <- which(tiers > 1 & !is.na(tiers))
  if(length(higher_tier_candidates) > 0) {
    # 상위 이동 대상 점수 계산
    network_score_higher <- with(data.frame(
      pagerank = V(g)$pagerank[higher_tier_candidates],
      degree = V(g)$degree_total[higher_tier_candidates]
    ), {
      normalize(pagerank) * 0.6 + normalize(degree) * 0.4
    })
    
    size_score_higher <- with(data.frame(
      sales = V(g)$sales[higher_tier_candidates],
      laborer = V(g)$laborer[higher_tier_candidates]
    ), {
      normalize(sales) * sales_weight + 
        normalize(laborer) * (business_size_weight - sales_weight)
    })
    
    trade_score_higher <- normalize(V(g)$trade_ratio[higher_tier_candidates])
    
    candidate_scores <- network_score_higher * network_importance_weight +
      size_score_higher * business_size_weight +
      trade_score_higher * trade_pattern_weight
    
    # 상위 5%는 한 단계 상향
    valid_scores <- !is.na(candidate_scores)
    if(sum(valid_scores) > 0) {
      promotion_threshold <- quantile(candidate_scores[valid_scores], 0.95, na.rm=TRUE)
      promote_indices <- higher_tier_candidates[valid_scores & 
                                                  candidate_scores >= promotion_threshold]
      
      if(length(promote_indices) > 0) {
        tiers[promote_indices] <- pmax(1, tiers[promote_indices] - 1)
        log_message(sprintf("Promoted %d firms to higher tier", length(promote_indices)))
      }
    }
    
    # Tier 0 승격 평가
    if(promotion_to_tier0) {
      tier1_companies <- which(tiers == 1)
      if(length(tier1_companies) > 0) {
        network_score_tier1 <- with(data.frame(
          pagerank = V(g)$pagerank[tier1_companies],
          degree = V(g)$degree_total[tier1_companies]
        ), {
          normalize(pagerank) * 0.6 + normalize(degree) * 0.4
        })
        
        size_score_tier1 <- with(data.frame(
          sales = V(g)$sales[tier1_companies],
          laborer = V(g)$laborer[tier1_companies]
        ), {
          normalize(sales) * sales_weight + 
            normalize(laborer) * (business_size_weight - sales_weight)
        })
        
        trade_score_tier1 <- normalize(V(g)$trade_ratio[tier1_companies])
        
        tier1_scores <- network_score_tier1 * network_importance_weight +
          size_score_tier1 * business_size_weight +
          trade_score_tier1 * trade_pattern_weight
        
        valid_tier1 <- !is.na(tier1_scores)
        if(sum(valid_tier1) > 0) {
          tier0_threshold <- quantile(tier1_scores[valid_tier1], 0.99, na.rm=TRUE)
          
          flagship_avg_pagerank <- mean(V(g)$pagerank[flagship_vertex_indices], na.rm=TRUE)
          flagship_avg_degree <- mean(V(g)$degree_total[flagship_vertex_indices], na.rm=TRUE)
          
          promote_to_tier0 <- tier1_companies[
            valid_tier1 &
              tier1_scores >= tier0_threshold &
              V(g)$pagerank[tier1_companies] >= flagship_avg_pagerank * 0.5 &
              V(g)$degree_total[tier1_companies] >= flagship_avg_degree * 0.5
            ]
          
          if(length(promote_to_tier0) > 0) {
            tiers[promote_to_tier0] <- 0
            log_message(sprintf("Promoted %d firms to tier 0", length(promote_to_tier0)))
          }
        }
      }
    }
  }
  
  return(tiers)
}

```
##### 3.3.1.2.4. 조정 결과 및 한계

```R
print("=== 단순거리 기준 Tier별 특성 ===")
print(analyze_tiers(centrality, "simple_tier"))

simple_tier firm_count avg_pagerank avg_degree    avg_sales median_sales
         <dbl>      <int>        <dbl>      <dbl>        <dbl>        <dbl>
 1           0        100   0.000117       548.   14811283354.  7555249650 
 2           1       7220   0.0000116       31.2     96466067.    23123658.
 3           2      13497   0.00000662      16.7    150521625.    29692729 
 4           3       4967   0.00000620      11.6     38099004.    19568564 
 5           4       1325   0.00000427       6.29    44578863.    20501005 
 6           5        109   0.00000499       6.66    29718883.    17332707 
 7           6         19   0.00000462       7       40606486.    20114122 
 8           7          5   0.00000416       6.4     26115821     15799894 
 9           8          1   0.00000940      18        4825000      4825000 
10         Inf        363   0.00000679       9.33    22543139.    14480988 
11          NA     122579   0.00000315       3.51     9448917.     1874965 


> print("\n=== 홍장표식 Tier별 특성 ===")
[1] "\n=== 홍장표식 Tier별 특성 ==="
> print(analyze_tiers(centrality, "hong_tier"))
# A tibble: 11 x 6
   hong_tier firm_count avg_pagerank avg_degree    avg_sales median_sales
       <dbl>      <int>        <dbl>      <dbl>        <dbl>        <dbl>
 1         0        110   0.000111       522.   13592784237.  6642955270 
 2         1       8162   0.0000121       34.3    203797587.    25908248.
 3         2      12604   0.00000590      13.5     83686753.    27975629 
 4         3       4907   0.00000610      11.3     37709057.    19427976 
 5         4       1325   0.00000427       6.29    44599508.    20532628 
 6         5        108   0.00000482       6.44    29676699.    17193208.
 7         6         19   0.00000462       7       40606486.    20114122 
 8         7          5   0.00000416       6.4     26115821     15799894 
 9         8          1   0.00000940      18        4825000      4825000 
10       Inf        365   0.00000682       9.35    22474507.    14445301 
11        NA     122579   0.00000315       3.51     9448917.     1874965
```
- Tier 0으로 10개의 기업이 이동했고, Tier 3-> Tier 2, Tier 2-> Tier 1 이동도 눈에 띕니다. 
- 매출 지표 분포가 비교적 개선된 것을 확인할 수 있습니다. (Tier0 > Tier 1 > Tier 2)
- 하위 계층에서는 여전히 매출 역전 발생하나, 네트워크 지표가 반드시 매출과 일치하는 것은 아님

### 3.3.2. 구조적 특성의 통합

네트워크의 응집성과 위계성을 동시에 고려하기 위해 Nakano and White(2007)의 CoSL 개념과 Gofman(2022)의 Upstreamness 측정 방법을 통합했습니다.

#### 3.3.2.1. Gofman and Wu(2022)의 Upstreamness 구현

Gofman에 따르면 Upstreamness는 다음과 같이 정의됩니다 : 

"a global network measure in the sense that it depends not only on a firm's direct supplier-customer relations, but also on the relations of its indirect suppliers and customers."

**이론적 정의**
```
Upstreamness_i = min_j∈Ω D(i,j)
where D(i,j) is the shortest distance between i and j, 
      Ω is the set of final goods producers
```

이러한 Upstreamness 접근은 다음 세 가지 핵심 목표를 반영한 것입니다. 

1. 공급사슬 상의 위치를 다차원적으로 고려
2. 복수의 flagship 기업과의 관계를 통합적으로 분석
3. 거래 경로의 다양성을 중요도 평가에 반영


**구현 특징**

이러한 Gofman의 방법론을 다음과 같이 반영했습니다. 

1. **계층 구조화**: Flagship firms를 tier 0으로 설정하고 이를 기준으로 계층을 구성합니다.
2. **거리 기반 분석**: 네트워크 거리행렬을 계산하여 기업 간 관계를 정량화합니다.
3. **수직적 위치 결정**: 각 기업의 최단거리를 통해 vertical position을 도출합니다.


**구현 코드**
```R
calculate_upstreamness <- function(g, flagship_firms) {
  # flagship 기업 식별
  flagship_ids <- which(V(g)$name %in% flagship_firms)
  if(length(flagship_ids) == 0) {
    return(rep(NA, vcount(g)))
  }
  
  # 최단 경로 계산
  distances <- shortest.paths(g, to=flagship_ids, mode="out")
  
  # Upstreamness 산출
  upstreamness <- apply(distances, 1, function(x) {
    if(all(is.infinite(x))) {
      finite_vals <- distances[!is.infinite(distances)]
      if(length(finite_vals) > 0) {
        return(max(finite_vals) + 1)
      } else {
        return(NA)
      }
    }
    min(x[!is.infinite(x)])
  })
  
  return(upstreamness)
}
```
-  Directed graph의 특성을 `mode="out"` 파라미터를 통해 반영합니다.
-  연결되지 않은 노드(infinite distance)에 대한 처리 로직을 포함합니다.

#### 3.3.2.2. Nakano and White(2007)의 CoSL 구현

**이론적 정의 **

Nakano and White는 CoSL을 다음과 같이 정의합니다: 

"Cohesive sequence levels derive position or role in a hierarchy from keeping elements together so as to maximize vertical cohesion, i.e., vertical closeness."

이러한 CoSL의 개념은 다음을 통해 구현됩니다. 

1. 군집 분석을 통한 응집 구조 파악
2. 유사 특성 기업들의 그룹화
3. 거래 강도를 반영한 계층화

이 개념은 특히 **오래된 기업들이 공급망 길이 대비 상대적으로 높은 위치로 이동하는 현상을 포착**하는 장점이 있습니다. 

"CoSL roles in production hierarchies reflect how, as production chains evolve, they might minimize cohesive distances... This allows older firms to "climb" the hierarchy by bringing in or positioning themselves higher up relative to the length of their supply chains."

**구현 특징**

Nakano의 cohesive sequence 개념을 다음과 같이 구현했습니다:

1. **군집화**: Walktrap 알고리즘을 통해 cohesive community를 식별합니다.
2. **다차원 평가**: Network importance(PageRank, degree centrality)와 Business size(sales, employees) 지표를 통합합니다.
3. **계층화**: 군집 내 기업들의 상대적 위치를 정규화된 점수로 산출합니다.

**구현 코드**
```R
calculate_cosl <- function(g, flagship_firms) {
  vertex_count <- vcount(g)
  V(g)$sales <- network_data$SALES[match(V(g)$name, network_data$KEDCD)]
  
  # 군집 분석 수행
  E(g)$weight <- 1
  cohesion <- cluster_walktrap(g, weights=E(g)$weight)
  
  cosl_levels <- numeric(vertex_count)
  for(cluster in 1:max(membership(cohesion))) {
    cluster_nodes <- which(membership(cohesion) == cluster)
    if(length(cluster_nodes) > 0) {
      # 각 군집 내 flagship까지의 거리 계산
      flagship_ids <- which(V(g)$name %in% flagship_firms)
      distances_to_flagship <- shortest.paths(g, v=cluster_nodes, to=flagship_ids, mode="out")
      
      min_distances <- apply(distances_to_flagship, 1, function(x) {
        if(all(is.infinite(x))) {
          finite_vals <- distances_to_flagship[!is.infinite(distances_to_flagship)]
          if(length(finite_vals) > 0) {
            return(max(finite_vals) + 1)
          } else {
            return(vertex_count)
          }
        }
        min(x[!is.infinite(x)])
      })
      
      # 군집 내 순위 결정
      sorted_indices <- order(min_distances, 
                            -V(g)$sales[cluster_nodes],
                            -degree(g, v=cluster_nodes, mode="total"))
      cosl_levels[cluster_nodes[sorted_indices]] <- seq_along(sorted_indices)
    }
  }
  
  # 정규화
  max_level <- max(cosl_levels, na.rm = TRUE)
  if(max_level > 0) cosl_levels <- cosl_levels / max_level
  
  return(cosl_levels)
}
```

**종합 점수 계산**

네트워크 중요도와 기업 규모를 통합한 최종 점수를 다음과 같이 계산합니다:

```R
# 네트워크 중요도 점수 계산
network_score <- with(data.frame(
  pagerank = V(g)$pagerank[first_tier_candidates],
  degree = V(g)$degree_total[first_tier_candidates]
), {
  normalize(pagerank) * 0.6 + normalize(degree) * 0.4
})

# 기업 규모 점수 계산
size_score <- with(data.frame(
  sales = V(g)$sales[first_tier_candidates],
  laborer = V(g)$laborer[first_tier_candidates]
), {
  normalize(sales) * sales_weight + 
    normalize(laborer) * (business_size_weight - sales_weight)
})

# 최종 점수 산출
total_score <- network_score * network_importance_weight +
               size_score * business_size_weight +
               trade_score * trade_pattern_weight
```


## 3.3.3. 동적 조정 메커니즘

각각의 metric들을 계산한 이후 반복적 조정 과정은 다음과 같이 수행됩니다. 

### 3.3.3.1. 초기화 및 결측값 처리

시작 단계에서 다음과 같은 전처리를 수행합니다. 

1. **초기 계층 설정**
   - 홍장표식 방법론으로 계산된 초기 tier 값을 할당합니다. 
   - 조정에 사용할 metric에 결측값이 있는 경우 적절한 대체값을 적용합니다. 
     - trade_ratio: 평균값으로 대체
     - pagerank: 최소값으로 대체
     - cosl: 평균값으로 대체
     - upstreamness: 최대값으로 대체
     - size_similarity: 0으로 대체

2. **통합 조정 파라미터 설정**
   ```R
   adjustment_strength = 0.01  # 기본값
   trade_threshold = 1 - (adjustment_strength * 0.3)
   size_threshold = 1 - (adjustment_strength * 0.5)
   cosl_threshold = adjustment_strength * 0.4
   ```
0~1 사이의 값을 갖는 통합 조정 파라미터 `adjustment_strength`를 만들어, 여러 metric을 사용한 조정 강도를 일관되게 변경할 수 있도록 설계하였습니다. 

### 3.3.3.2. 반복적 조정 프로세스

이후 매 반복에서 세 가지 기준에 따라 조정을 수행합니다. 

1. **거래 비율 기반 조정**
   - 거래 비율이 특정 임계값 이상인 Tier 1 기업들을 상향 조정
   - trade_ratio가 상위 그룹에 속하는 기업들이 대상

2. **규모 유사성 기반 조정**
   - 규모가 유사하고 PageRank가 높은 기업들의 계층을 상향 조정
   - size_similarity가 임계값 이상이면서 PageRank가 상위권인 경우

3. **네트워크 구조 기반 조정**
   - CoSL과 Upstreamness 값을 기반으로 조정
   - 두 지표가 모두 임계값을 만족하는 경우에만 조정 수행
   ```R
   if(metrics_data$cosl[i] <= cosl_threshold && 
      metrics_data$upstreamness[i] <= adjustment_strength * 2)
   ```

### 3.3.3.3. 수렴성 관리

1. **반복 제한**
   - 최대 10회까지만 반복을 수행
   - 3회 이상 반복 후 변화율이 충분히 작아지면 수렴으로 판단

2. **변화율 계산**
   ```R
   change_ratio = changes / total_valid
   convergence_threshold = 0.02 * (1 - adjustment_strength)
   ```

3. **수렴 조건**
   - iteration ≥ 3
   - change_ratio < convergence_threshold
   - 수렴하지 않더라도 10회 반복 후 종료

### 3.3.3.4. 주요 특징

1. **보수적 접근**
   - CoSL Thresold와 Upstreamness 는 복수 조건을 동시에 만족해야 조정
   - 조정 강도를 0.01~0.03 사이의 값으로 조정하여 과도한 tier 상승 방지 

2. **안정성 확보**
   - 결측값에 대한 체계적 처리
   - 단계적 조정 및 최대 조정 비율 제한(전체 노드의 5% 이내)

```R

# 로깅 함수 추가
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, level, message))
}

# calculate_improved_tiers 함수 수정
calculate_improved_tiers <- function(g, flagship_firms, 
                                     metrics_data,
                                     hong_tiers_df = NULL,
                                     adjustment_strength = 0.01,
                                     run_tests = FALSE) {
  
  log_message("Starting tier calculation process")
  
  # 홍장표식 Tier 데이터 로드
  if(is.null(hong_tiers_df)) {
    hong_tiers_df <- calculate_and_save_hong_tiers(g, flagship_firms)
  }
  
  # 홍장표식 Tier 매칭
  initial_tiers <- hong_tiers_df$hong_tier[match(V(g)$name, hong_tiers_df$KEDCD)]
  
  # 1. 결측값 처리
  log_message("Checking and handling missing values in metrics")
  metrics_data <- metrics_data %>%
    mutate(across(everything(), ~ifelse(is.infinite(.), NA, .))) %>%
    mutate(
      trade_ratio = ifelse(is.na(trade_ratio), mean(trade_ratio, na.rm = TRUE), trade_ratio),
      pagerank = ifelse(is.na(pagerank), min(pagerank, na.rm = TRUE), pagerank),
      cosl = ifelse(is.na(cosl), mean(cosl, na.rm = TRUE), cosl),
      upstreamness = ifelse(is.na(upstreamness), max(upstreamness, na.rm = TRUE), upstreamness),
      size_similarity = ifelse(is.na(size_similarity), 0, size_similarity)
    )
  
  # 2. 동적 조정 시작
  tiers <- initial_tiers
  prev_tiers <- tiers
  converged <- FALSE
  iteration <- 0
  change_history <- numeric(0)
  
  # 조정 임계값 설정
  log_message(sprintf("Setting adjustment parameters with strength: %.2f", adjustment_strength))
  trade_threshold <- 1 - (adjustment_strength * 0.3)
  size_threshold <- 1 - (adjustment_strength * 0.5)
  cosl_threshold <- adjustment_strength * 0.4
  
  while(!converged && iteration < 10) {
    iteration <- iteration + 1
    log_message(sprintf("Starting iteration %d", iteration))
    
    # 3.1 거래 비율 기반 상향 조정
    log_message("Processing trade ratio adjustments")
    valid_ratios <- metrics_data$trade_ratio[!is.na(metrics_data$trade_ratio)]
    if(length(valid_ratios) > 0) {
      ratio_threshold <- quantile(valid_ratios, trade_threshold, na.rm = TRUE)
      high_trade_ratio <- metrics_data$trade_ratio >= ratio_threshold
      tier1_candidates <- which(tiers == 1 & high_trade_ratio & !is.na(tiers))
      
      if(length(tier1_candidates) > 0) {
        tiers[tier1_candidates] <- pmax(0, tiers[tier1_candidates] - 1)
        log_message(sprintf("Adjusted %d firms based on trade ratio", length(tier1_candidates)))
      }
    }
    
    # 3.2 규모 유사성 기반 조정
    log_message("Processing size similarity adjustments")
    for(i in which(!is.na(tiers) & tiers > 0)) {
      if(metrics_data$size_similarity[i] >= size_threshold) {
        pr_threshold <- quantile(metrics_data$pagerank, 1 - adjustment_strength, na.rm = TRUE)
        if(!is.na(metrics_data$pagerank[i]) && metrics_data$pagerank[i] >= pr_threshold) {
          tiers[i] <- max(0, tiers[i] - 1)
        }
      }
    }
    
    # 3.3 네트워크 구조 기반 조정
    log_message("Processing network structure adjustments")
    valid_indices <- which(!is.na(tiers) & tiers > 0 & 
                             !is.na(metrics_data$cosl) & 
                             !is.na(metrics_data$upstreamness))
    
    for(i in valid_indices) {
      if(metrics_data$cosl[i] <= cosl_threshold && 
         metrics_data$upstreamness[i] <= adjustment_strength * 2) {
        tiers[i] <- max(0, tiers[i] - 1)
      }
    }
    
    # 변화 추적
    valid_tiers <- !is.na(tiers) & !is.na(prev_tiers)
    changes <- sum(tiers[valid_tiers] != prev_tiers[valid_tiers])
    total_valid <- sum(valid_tiers)
    
    #안전한 변화율 계산
    change_ratio <- if(total_valid > 0) {
      changes / total_valid
    } else {
      0  # 유효한 데이터가 없으면 0으로 처리
    }
    change_history <- c(change_history, change_ratio)
    
    log_message(sprintf("Iteration %d completed with %d changes (%.2f%%)", 
                        iteration, changes, change_ratio * 100))
    
    # 수렴 검사
    if(iteration >= 3 && change_ratio < (0.02 * (1 - adjustment_strength))) {
      converged <- TRUE
      log_message("Convergence achieved")
    }
    
    prev_tiers <- tiers
  }
  
  # 4. 테스트 실행 
  test_results <- NULL
  if(run_tests) {
    log_message("Running validation tests", "TEST")
    test_results <- run_complex_tests(g, tiers, metrics_data, flagship_firms)
  }
  
  # 5. 결과 정리
  result <- list(
    tiers = tiers,
    iterations = iteration,
    converged = converged,
    change_history = change_history,
    initial_tiers = initial_tiers,
    test_results = test_results
  )
  
  # 최종 통계 출력
  log_message("Final Statistics:")
  log_message(sprintf("- Total iterations: %d", iteration))
  log_message(sprintf("- Converged: %s", converged))
  log_message(sprintf("- Final change ratio: %.4f", tail(change_history, 1)))
  
  return(result)
}

run_complex_tests <- function(g, tiers, metrics_data, flagship_firms) {
  log_message("Starting complex tests", "TEST")
  
  # 1. 기본 통계 검증
  na_count <- sum(is.na(tiers))
  tier_dist <- table(tiers, useNA = "ifany")
  
  # 2. Tier별 특성 분석
  tier_stats <- list()
  for(t in sort(unique(tiers[!is.na(tiers)]))) {
    tier_firms <- which(tiers == t)
    tier_stats[[as.character(t)]] <- list(
      count = length(tier_firms),
      avg_pagerank = mean(metrics_data$pagerank[tier_firms], na.rm = TRUE),
      avg_trade_ratio = mean(metrics_data$trade_ratio[tier_firms], na.rm = TRUE),
      avg_cosl = mean(metrics_data$cosl[tier_firms], na.rm = TRUE)
    )
  }
  
  # 3. 결과 반환
  return(list(
    na_count = na_count,
    tier_distribution = tier_dist,
    tier_statistics = tier_stats
  ))
}

```

3. **검증 체계**
   - 매 반복마다 변화 추적
   - 기본 통계량 모니터링
   - 최종 결과에 대한 종합 검증 수행

```R

# 테스트 실행 함수
run_complex_tests <- function() {
  test_env <- create_complex_test_network()
  g <- test_env$graph
  firms <- test_env$firms
  flagship <- test_env$flagship
  true_tiers <- test_env$true_tiers
  
  cat("=== 복잡 네트워크 테스트 시작 ===\n")
  cat("네트워크 크기:", vcount(g), "노드,", ecount(g), "엣지\n\n")
  
  # 1. 메트릭 계산 테스트
  cat("1. 메트릭 계산 테스트\n")
  metrics <- calculate_and_store_metrics(g, flagship)
  
  cat("메트릭 검증:\n")
  cat("- NA 비율:\n")
  for(col in names(metrics)) {
    na_ratio <- mean(is.na(metrics[[col]]))
    cat(sprintf("  %s: %.2f%%\n", col, na_ratio * 100))
  }
  
  # 2. 동적 Tier 계산 테스트
  cat("\n2. 동적 Tier 계산 테스트\n")
  result <- calculate_dynamic_tiers(g, flagship, metrics)
  tiers <- result$tiers
  
  cat("\n수렴 정보:\n")
  cat("- 반복 횟수:", result$iterations, "\n")
  cat("- 수렴 여부:", result$converged, "\n")
  cat("- 변화율 기록:\n")
  for(i in seq_along(result$change_history)) {
    cat(sprintf("  Iteration %d: %.2f%%\n", i, result$change_history[i] * 100))
  }
  
  # Tier 분포 상세 분석
  cat("\nTier 분포 상세:\n")
  tier_table <- table(tiers, useNA = "ifany")
  print(tier_table)
  
  # NA가 아닌 tier의 비율
  valid_tier_ratio <- mean(!is.na(tiers))
  cat(sprintf("\n유효 tier 비율: %.2f%%\n", valid_tier_ratio * 100))
  
  # Tier별 기업 특성
  cat("\nTier별 평균 특성:\n")
  for(t in sort(unique(tiers[!is.na(tiers)]))) {
    firms_in_tier <- which(tiers == t)
    cat(sprintf("\nTier %d:\n", t))
    cat(sprintf("- 기업 수: %d\n", length(firms_in_tier)))
    cat(sprintf("- 평균 매출: %.2e\n", mean(firms$SALES[firms_in_tier])))
    cat(sprintf("- 평균 종업원: %.2f\n", mean(firms$LABORER_SUM[firms_in_tier])))
    cat(sprintf("- 평균 PageRank: %.2e\n", mean(metrics$pagerank[firms_in_tier])))
    cat(sprintf("- 평균 거래비율: %.2f\n", mean(metrics$trade_ratio[firms_in_tier])))
  }
  
  # 3. 정확도 분석
  cat("\n3. 정확도 분석\n")
  accuracy <- mean(tiers == true_tiers, na.rm = TRUE)
  cat(sprintf("전체 정확도: %.2f%%\n", accuracy * 100))
  
  # Tier별 정확도
  for(t in sort(unique(true_tiers))) {
    tier_accuracy <- mean(tiers[true_tiers == t] == t, na.rm = TRUE)
    cat(sprintf("Tier %d 정확도: %.2f%%\n", t, tier_accuracy * 100))
  }
  
  # 4. 오분류 분석
  cat("\n4. 오분류 분석\n")
  misclassified <- which(tiers != true_tiers & !is.na(tiers))
  if(length(misclassified) > 0) {
    cat("오분류 사례 (상위 5개):\n")
    for(i in head(misclassified, 5)) {
      cat(sprintf("\n기업 %s:\n", firms$KEDCD[i]))
      cat(sprintf("- 예측/실제 tier: %d/%d\n", tiers[i], true_tiers[i]))
      cat(sprintf("- 매출: %.2e\n", firms$SALES[i]))
      cat(sprintf("- 종업원: %d\n", firms$LABORER_SUM[i]))
      cat(sprintf("- PageRank: %.2e\n", metrics$pagerank[i]))
      cat(sprintf("- 거래비율: %.2f\n", metrics$trade_ratio[i]))
      cat(sprintf("- Upstreamness: %.2f\n", metrics$upstreamness[i]))
      cat(sprintf("- CoSL: %.2f\n", metrics$cosl[i]))
    }
  }
  
  # 5. 성능 테스트
  cat("\n5. 성능 테스트\n")
  time_dynamic <- system.time(
    replicate(5, calculate_dynamic_tiers(g, flagship, metrics))
  )
  cat(sprintf("평균 계산 시간: %.3f초\n", time_dynamic[3]/5))
  
  return(list(
    metrics = metrics,
    result = result,
    accuracy = accuracy,
    misclassified = misclassified
  ))
}

```

## 4. 방법론의 의의

본 연구는 다음의 방법론적인 개선을 통해 현실적인 한국 공급망 계층구조를 재구성 하였습니다. 

   - 단순 거리 기반 계층화의 한계 극복 :   기업의 규모(홍장표), 네트워크 응집성(Nakano), 수직적 위계성(Gofman) 관점의 균형있는 통합
   - 다양한 기업 특성의 종합적 고려
   - 동적인 계층 이동성 확보 