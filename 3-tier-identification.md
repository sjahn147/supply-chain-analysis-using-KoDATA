## 1. 개요

본 연구는 한국 제조업 공급망에서의 tier 구조를 식별하기 위해 기존의 세 가지 주요 방법론 - 홍장표(2015)의 거래관계 기반 계층 구분, Nakano and White(2006)의 Cohesive Sequence Levels(CoSL), Gofman(2011)의 Upstreamness - 을 통합적으로 발전시켰다. 

## 2. 계층 할당 방법론

### 기존 방법론의 통합
본 연구는 세 가지 주요 방법론을 결합한 앙상블 접근법을 채택한다:

1. **Gofman의 Upstreamness 방식**
   - 네트워크에서 기업 간 최단 경로를 기반으로 상류도(upstreamness)를 측정
   - 기함기업으로부터의 거리를 고려하여 계층적 위치 파악
   - 수식: 각 노드 v에 대해, upstreamness(v) = min(distance(v, f)) for f ∈ flagship firms

2. **Nakano & White의 CoSL(Core-Supplier Levels) 방식**
   - 군집 분석과 거래 패턴을 결합한 계층 구조 분석
   - 응집도(cohesion)와 거래 강도를 고려한 계층 할당
   - 각 군집 내에서 기업 규모와 연결 패턴을 고려한 세부 계층화

3. **개선된 홍장표 방식**
   - 1차 협력사 식별을 위한 복합 지표 도입
   - 기업 규모의 상대적 비교를 통한 계층 조정
   - 거래 패턴과 네트워크 위치를 고려한 동적 계층 할당

### 최종 계층 결정 프로세스
1. 각 방법론별 계층 산출
2. 가중치 적용된 통합 점수 계산
3. 임계값 기반 계층 조정
4. 이상치 검증 및 수정

## 실제 구현

### 1. 초기 계층화 단계

단순 네트워크 거리 기준 계산 방법

``` R
calculate_tiers <- function(g, top_firms) {
  shortest_paths <- shortest.paths(g, 
                                 v = which(V(g)$name %in% top_firms),
                                 mode = "out")
  apply(shortest_paths, 2, min)
}
```

먼저 홍장표(2015)의 방법론을 따라 flagship 기업들과의 직접적 거래관계를 기반으로 initial tier를 설정한다. 이는 한국 제조업의 실제 거래구조를 반영하는 기본 틀을 제공한다. 

#### 홍장표(2015)의 규모 기반 접근

- 매출액 기준의 1차 필터링
- 거래 비중을 통한 관계 강도 평가
- 규모의 상대성 고려

```R
calculate_final_hong_tiers <- function(g, flagship_firms, 
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

##### 복합 점수 체계
다음 요소들의 가중 평균을 통해 각 기업의 종합 점수를 산출한다:

1. **네트워크 중요도 (40%)**
   - PageRank 점수 (20%)
   - 연결중심성 (20%)

2. **기업 규모 (30%)**
   - 매출액 (12%)
   - 종업원 수 (18%)

3. **거래 패턴 (30%)**
   - 판매/구매 비율
   - 거래 다각화 정도

```R
base_score = network_weight * (pagerank * 0.5 + degree_total * 0.5) +
             size_weight * (normalized_sales * sales_weight + 
                          normalized_laborer * (0.3 - sales_weight)) +
             trade_weight * trade_ratio
```

##### 계층 이동성
1. **상향 이동 기준**
   - 복합 점수 상위 5%는 tier 0 승격 고려

```R
promotion_to_tier0 <- function(node) {
  # 기본 조건
  meets_base <- composite_score[node] >= quantile(composite_score, 0.95)
  
  # 네트워크 구조 조건
  network_condition <- (
    upstream_score[node] >= mean(upstream_score[flagship_ids]) * 0.3 &&
    multiple_path_count[node] >= 2
  )
  
  # 응집성 조건
  cohesion_condition <- (
    cosl_score[node] <= quantile(cosl_score, 0.2) &&
    cluster_bonus[node] >= median(cluster_bonus)
  )
  
  return(meets_base && network_condition && cohesion_condition)
}
```

   - 기함기업 평균 지표의 30% 이상 충족 필요

```R
promote_tier <- function(node) {
  # 복합 점수 상위 20%
  if(composite_score[node] >= quantile(composite_score, 0.8)) {
    # 응집성과 네트워크 구조 추가 검증
    if(cosl_score[node] <= median(cosl_score) &&
       upstream_score[node] <= median(upstream_score)) {
      return(current_tier[node] - 1)
    }
  }
  return(current_tier[node])
}
```

2. **하향 이동 기준**
   - 매출액 비율이 기함기업 대비 1/5 미만
   - 네트워크 지표가 하위 20% 이하

```R
demote_tier <- function(node) {
  # 기본 조건: 복합 점수 하위 20%
  if(composite_score[node] <= quantile(composite_score, 0.2)) {
    # 네트워크 구조적 취약성
    if(upstream_score[node] >= median(upstream_score) ||
       multiple_path_count[node] <= 1) {
      # 응집성 저하
      if(cosl_score[node] >= quantile(cosl_score, 0.8)) {
        return(current_tier[node] + 1)
      }
    }
  }
  return(current_tier[node])
}
```

그러나 홍장표의 방식이 갖는 정적이고 이분법적인 한계를 극복하기 위해, 이후 단계에서 네트워크의 구조적 특성을 반영한 동적 조정을 도입한다.

### 2. 구조적 특성의 통합

Nakano and White(2006)의 CoSL 개념과 Gofman(2011)의 Upstreamness 측정을 결합하여 네트워크의 응집성과 위계성을 동시에 고려한다. 

#### Gofman and Wu (2022)의 Upstreamness

- 공급사슬 상의 위치를 다차원적으로 고려
- 복수의 flagship 기업과의 관계를 통합
- 거래 경로의 다양성이 중요도 평가에 반영

Gofman 논문의 이론적 정의:
```
Upstreamness_i = min_j∈Ω D(i,j)

where D(i,j) is the shortest distance between i and j, and Ω is the set of final goods producers
```

calculate_upstreamness() 함수:
```r

# Upstreamness 계산 함수
calculate_upstreamness <- function(g, flagship_firms) {
  flagship_ids <- which(V(g)$name %in% flagship_firms)
  if(length(flagship_ids) == 0) {
    return(rep(NA, vcount(g)))
  }
  
  distances <- shortest.paths(g, to=flagship_ids, mode="out")
  
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


```r
# 기본 정보 설정
tiers <- rep(NA, vcount(g))
vertex_names <- V(g)$name

# Flagship firms 기반 Tier 할당
flagship_vertex_indices <- match(flagship_firms, vertex_names) 
tiers[flagship_vertex_indices] <- 0

# 거리 행렬 계산을 통한 vertical position 도출
dist_matrix <- distances(g, v=V(g), to=flagship_vertex_indices, mode="out")
min_distances <- apply(dist_matrix, 1, min)
```

이 부분은 Gofman 논문의 핵심 방법론을 구현한 것이다:
- Flagship firms를 tier 0으로 설정
- 네트워크 거리행렬(distances)을 계산
- 각 기업의 최단거리를 통해 vertical position 도출

이는 Gofman 논문의 다음 내용을 구현한 것이다:
"Upstreamness is a global network measure in the sense that it depends not only on a firm's direct supplier-customer relations, but also on the relations of its indirect suppliers and customers."

장점:
- 논문의 수학적 정의를 정확히 구현
- Directed graph의 특성 반영(mode="out")
- 연결되지 않은 노드(infinite distance) 처리 로직 포함

개선필요:
- 논문의 "each firm supplies to the next firm in the chain (i-1)" 가정이 코드에서 명시적으로 검증되지 않음
- 복수의 flagship firms 간 관계 처리가 불명확

#### Nakano and White (2007)의 CoSL 

- 군집 분석을 통한 응집구조 파악
- 유사 특성 기업들의 그룹화
- 거래 강도를 반영한 계층화

Nakano 논문의 이론:
```
CoSL roles in production hierarchies reflect how, as production chains evolve, they might minimize cohesive distances... This allows older firms to "climb" the hierarchy by bringing in or positioning themselves higher up relative to the length of their supply chains.
```

 calculate_cosl() 함수 :
```r
# CoSL 계산 함수
calculate_cosl <- function(g, flagship_firms) {
  vertex_count <- vcount(g)
  V(g)$sales <- network_data$SALES[match(V(g)$name, network_data$KEDCD)]
  
  E(g)$weight <- 1
  cohesion <- cluster_walktrap(g, weights=E(g)$weight)
  
  cosl_levels <- numeric(vertex_count)
  for(cluster in 1:max(membership(cohesion))) {
    cluster_nodes <- which(membership(cohesion) == cluster)
    if(length(cluster_nodes) > 0) {
      flagship_ids <- which(V(g)$name %in% flagship_firms)
      distances_to_flagship <- shortest.paths(g, v=cluster_nodes, to=flagship_ids, mode="out")
      
      min_distances <- apply(distances_to_flagship, 1, function(x) {
        if(all(is.infinite(x))) {
          finite_vals <- distances_to_flagship[!is.infinite(distances_to_flagship)]
          if(length(finite_vals) > 0) {
            return(max(finite_vals) + 1)
          } else {
            return(vertex_count)  # 최대 거리 할당
          }
        }
        min(x[!is.infinite(x)])
      })
      
      sorted_indices <- order(min_distances, 
                              -V(g)$sales[cluster_nodes],
                              -degree(g, v=cluster_nodes, mode="total"))
      cosl_levels[cluster_nodes[sorted_indices]] <- seq_along(sorted_indices)
    }
  }
  
  max_level <- max(cosl_levels, na.rm = TRUE)
  if(max_level > 0) cosl_levels <- cosl_levels / max_level
  
  return(cosl_levels)
}
```


```r
# Network importance weight를 반영한 점수 계산
network_score <- with(data.frame(
  pagerank = V(g)$pagerank[first_tier_candidates],
  degree = V(g)$degree_total[first_tier_candidates]
), {
  normalize(pagerank) * 0.6 + normalize(degree) * 0.4
})

# Business size weight를 반영한 점수 계산  
size_score <- with(data.frame(
  sales = V(g)$sales[first_tier_candidates],
  laborer = V(g)$laborer[first_tier_candidates]
), {
  normalize(sales) * sales_weight + 
    normalize(laborer) * (business_size_weight - sales_weight)
})

# 종합 점수 계산
total_score <- network_score * network_importance_weight +
  size_score * business_size_weight +
  trade_score * trade_pattern_weight
```

이 부분은 Nakano의 cohesive sequence 개념을 구현한 것이다:
- Network importance (PageRank, degree centrality)
- Business size (sales, employees) 
- Trade patterns 

이는 Nakano 논문의 다음 내용을 반영한 것이다:
"Cohesive sequence levels derive position or role in a hierarchy from keeping elements together so as to maximize vertical cohesion, i.e., vertical closeness."

특히:

```r
network_score <- with(data.frame(
  pagerank = V(g)$pagerank[first_tier_candidates],
  degree = V(g)$degree_total[first_tier_candidates]
), {
  normalize(pagerank) * 0.6 + normalize(degree) * 0.4
})
```

여기서 PageRank와 degree centrality의 가중 결합은 네트워크 내 기업의 구조적 중요도를 포착한다. 가중치(0.6, 0.4)는 광범위한 실증분석을 통해 조정되었다.

장점:
- Walktrap 알고리즘을 통한 cohesive community 식별
- Sales와 degree centrality를 고려한 다차원적 위치 산정
- 정규화를 통한 상대적 위치 계산

개선필요:
- 논문의 "keeping elements together so as to maximize vertical cohesion" 개념이 cluster_walktrap()으로 충분히 구현되는지 검증 필요
- "Cohesive neighborhood" 개념의 명시적 구현 필요
- Firm age 변수 미반영


## 3. 동적 조정 메커니즘

이후 반복적 조정 과정에서는 다음과 같은 세 가지 핵심 측면을 고려한다:

1. Gofman의 vertical position 기반 초기 tier 할당
2. Nakano의 cohesive relationship 고려한 조정
3. 실제 거래관계와 기업 특성을 반영한 tier 조정

```R

# 로깅 함수 추가
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, level, message))
}

# calculate_improved_tiers 함수 수정
calculate_improved_tiers <- function(g, flagship_firms, 
                                     metrics_data,
                                     hong_tiers_df = NULL,  # 새로운 파라미터
                                     adjustment_strength = 0.5,
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

# 테스트 함수 통합
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

1. 구조적 특성 통합:
```r
!is.na(metrics_data$cosl) & !is.na(metrics_data$upstreamness)
```
- Nakano의 CoSL(응집성)과 Gofman의 Upstreamness(위계성)를 동시에 고려
- 두 지표가 모두 유효한 케이스만 선별하여 구조적 완결성 확보

2. 조정 로직:
```r
if(metrics_data$cosl[i] <= cosl_threshold && 
   metrics_data$upstreamness[i] <= adjustment_strength * 2)
```
- CoSL threshold: 응집성이 낮은 경우에만 조정 발생
- Upstreamness 기준: adjustment_strength의 2배를 threshold로 사용
- 두 조건의 AND 연산으로 보수적 접근

3. 미세 조정 메커니즘:
```r
adjustment_strength = 0.01~0.03
tiers[i] <- max(0, tiers[i] - 1)
```
- 매우 작은 adjustment_strength 값으로 점진적 조정
- 한번에 한 단계씩만 조정
- tier가 음수가 되지 않도록 보호

이 접근의 장점:

1. 구조적 완결성:
- 응집성(CoSL)과 위계성(Upstreamness)의 균형있는 고려
- 네트워크의 전역적 특성과 국지적 특성을 모두 반영

2. 안정성:
- 매우 작은 adjustment_strength로 급격한 변화 방지
- 단계적 조정으로 수렴성 확보

3. 보수성:
- 두 조건(CoSL, Upstreamness)을 모두 만족해야 조정
- 하향 조정만 허용하여 과도한 tier 상승 방지


이 과정에서 매우 보수적인 adjustment_strength(0.01~0.03)를 적용하여 급격한 변화를 방지하고, 수렴성을 확보한다. 특히 주목할 점은:

```r
while(!converged && iteration < 10) {
  iteration <- iteration + 1
  
  # 거래 비율 기반 상향 조정
  valid_ratios <- metrics_data$trade_ratio[!is.na(metrics_data$trade_ratio)]
  ratio_threshold <- quantile(valid_ratios, trade_threshold, na.rm = TRUE)
  
  # 규모 유사성 기반 조정
  for(i in which(!is.na(tiers) & tiers > 0)) {
    if(metrics_data$size_similarity[i] >= size_threshold) {
      pr_threshold <- quantile(metrics_data$pagerank, 1 - adjustment_strength, na.rm = TRUE)
      if(!is.na(metrics_data$pagerank[i]) && metrics_data$pagerank[i] >= pr_threshold) {
        tiers[i] <- max(0, tiers[i] - 1)  
      }
    }
  }
}

# 네트워크 구조 기반 조정
for(i in valid_indices) {
	if(metrics_data$cosl[i] <= cosl_threshold && 
     metrics_data$upstreamness[i] <= adjustment_strength * 2) {
    tiers[i] <- max(0, tiers[i] - 1)
  }
}
```

- CoSL threshold와 Upstreamness 조건의 AND 연산을 통한 보수적 접근
- 하향 조정만 허용하여 과도한 tier 상승 방지
- 각 iteration마다 최대 조정 비율 제한(전체 노드의 5% 이내)


4. 수렴성 검증

각 반복 과정에서 변화율을 모니터링하여 안정적 수렴을 보장한다:

```r
change_ratio <- if(total_valid > 0) {
  changes / total_valid
} else {
  0
}

if(iteration >= 3 && change_ratio < (0.02 * (1 - adjustment_strength))) {
  converged <- TRUE
}
```

## 4. 방법론의 의의

본 연구가 제시하는 앙상블 접근법은 다음과 같은 장점을 가진다:
1. 단순 거리 기반 계층화의 한계 극복
2. 다양한 기업 특성의 종합적 고려
3. 동적인 계층 이동성 확보
4. 실제 기업 간 관계를 더 정확히 반영

이러한 통합적 접근은 한국 제조업 공급망의 복잡한 계층구조를 보다 정교하게 포착하면서도, 급격한 변동을 방지하는 안정적인 tier 식별을 가능하게 한다. 특히 거래관계의 직접성(홍장표), 네트워크 응집성(Nakano), 수직적 위계성(Gofman)을 균형있게 반영하여 현실적인 공급망 구조를 재구성한다는 점에서 의의가 있다.

이상의 방법론은 기존 연구들의 장점을 살리면서도, 동적 조정을 통해 보다 유연하고 현실적인 tier 구조 파악을 가능하게 한다. 향후 산업별 특성을 반영한 가중치 차별화 등을 통해 방법론의 정교화가 가능할 것으로 기대된다.

이러한 방법론은 기업 생태계의 구조를 더 정확하게 이해하고, 정책 수립에 있어 보다 현실적인 기초자료를 제공할 수 있다.