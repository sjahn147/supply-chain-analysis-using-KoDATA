


```
* 원본 데이터 불러오기
use "your_data_path\KED_TRD_test.dta", clear

* 초기 상태 확인
di "=== 초기 데이터 상태 ==="
di "전체 관측치 수:"
count
di "유니크 KEDCD 수:"
distinct KEDCD

* 공통 변수 생성
gen data_stat = ""
la var data_stat "데이터 생성 출처 구분"
```



```
*------------- 데이터셋 1: KEDCD_TRD 결측 케이스 -------------*
preserve
    keep if missing(KEDCD_TRD)
    replace data_stat = "missing_TRD"
    
    di "=== 데이터셋 1: KEDCD_TRD 결측 케이스 ==="
    di "관측치 수:"
    count
    di "유니크 KEDCD 수:"
    distinct KEDCD
    
    save "your_data_path\dataset1.dta", replace
restore
```

```
*------------- 데이터셋 2: 원본 정상 케이스 -------------*
preserve
    keep if !missing(KEDCD_TRD)
    replace data_stat = "original"
    
    di "=== 데이터셋 2: 정상 케이스 ==="
    di "관측치 수:"
    count
    di "유니크 KEDCD 수:"
    distinct KEDCD
    di "유니크 KEDCD_TRD 수:"
    distinct KEDCD_TRD
    
    save "your_data_path\dataset2.dta", replace
restore
```


```
*------------- 데이터셋 3: 역방향 관계 생성 -------------*
preserve
    * 정상 케이스만 사용
    keep if !missing(KEDCD_TRD)
    
    * KEDCD_TRD가 KEDCD로 존재하는 케이스 식별
    gen exists_as_kedcd = 0
    bysort KEDCD_TRD: replace exists_as_kedcd = 1 if KEDCD_TRD == KEDCD[_n]
    
    keep if exists_as_kedcd == 1
    
    * 역방향 관계 생성
    gen temp_kedcd = KEDCD
    replace KEDCD = KEDCD_TRD
    replace KEDCD_TRD = temp_kedcd
    drop temp_kedcd
    
    * 거래 방향 반전
    replace v3 = 2 if v3 == 1
    replace v3 = 1 if v3 == 2
    
    replace data_stat = "generated"
    
    di "=== 데이터셋 3: 역방향 관계 ==="
    di "관측치 수:"
    count
    di "유니크 거래쌍 수:"
    distinct KEDCD KEDCD_TRD
    
    save "your_data_path\dataset3.dta", replace
restore
```

```
*------------- 데이터셋 4: 누락 기업 처리 -------------*
preserve
    keep if !missing(KEDCD_TRD)
    
    * KEDCD_TRD가 KEDCD로 존재하지 않는 케이스 식별
    gen exists_as_kedcd = 0
    bysort KEDCD_TRD: replace exists_as_kedcd = 1 if KEDCD_TRD == KEDCD[_n]
    keep if exists_as_kedcd == 0
    
    * 필요한 정보만 저장
    gen temp_kedcd = KEDCD_TRD
    gen temp_enp = v7
    gen temp_v7 = ENP_NM     // 기존 기업명을 거래처명으로
    gen temp_v3 = v3         // 기존 거래방향 저장
    keep KEDCD temp_kedcd temp_enp temp_v7 temp_v3
    
    * 새 기업 정보 생성
    rename KEDCD KEDCD_TRD
    rename temp_kedcd KEDCD
    rename temp_enp ENP_NM
    rename temp_v7 v7
    
    * 거래 방향 반전 (1:구매처, 2:판매처)
    replace temp_v3 = 2 if temp_v3 == 1
    replace temp_v3 = 1 if temp_v3 == 2
    rename temp_v3 v3
    
    gen data_stat = "missing_firms"
    
    di "=== 데이터셋 4: 누락 기업 ==="
    di "관측치 수:"
    count
    di "유니크 KEDCD 수:"
    distinct KEDCD
    di "거래 방향 분포:"
    tab v3
    
    save "your_data_path\dataset4.dta", replace
restore
```

```
*------------- 데이터셋 병합 -------------*
* 데이터셋 2 불러오기
use "your_data_path\dataset2.dta", clear

di "=== 데이터셋 2 기본 상태 ==="
tab data_stat

* 데이터셋 3 추가
append using "your_data_path\dataset3.dta"
di "=== 데이터셋 3 추가 후 상태 ==="
tab data_stat

* 데이터셋 4 추가
append using "your_data_path\dataset4.dta"
di "=== 데이터셋 4 추가 후 상태 ==="
tab data_stat

* 데이터셋 1 추가
append using "your_data_path\dataset1.dta"
di "=== 최종 상태 ==="
tab data_stat
```


```

* 현재 상태 확인
di "=== 초기 상태 ==="
tab data_stat
distinct KEDCD

* KEDCD별 data_stat 패턴 확인
bysort KEDCD: egen has_other = max(data_stat != "missing_TRD")
bysort KEDCD: egen only_missing = min(data_stat == "missing_TRD")

* missing_TRD만 있는 KEDCD 식별
gen keep_missing = (has_other == 0 & only_missing == 1)
la var keep_missing "missing_TRD만 있는 기업"

* 중복 제거 전 통계
di "=== missing_TRD only 기업 수 ==="
distinct KEDCD if keep_missing == 1

drop if data_stat =="missing_TRD" & keep_missing ~=1

* 중복 행 제거
duplicates drop year data_stat KEDCD ENP_NM ENP_NM_ENG ENP_NM_TRD ///
    ENP_SZE SALES KEDCD_TRD v15 v16 v17 v3 v7 ymd v5 v6 v9 v11, force

* 최종 상태 확인
di "=== 최종 상태 ==="
tab data_stat
distinct KEDCD

* missing_TRD only 기업들의 최종 상태 확인
di "=== missing_TRD only 기업 최종 상태 ==="
distinct KEDCD if keep_missing == 1

* 임시 변수 삭제
drop has_other only_missing keep_missing

* 거래 쌍 식별을 위한 정렬된 ID 생성
gen KEDCD_min = min(KEDCD, KEDCD_TRD)
gen KEDCD_max = max(KEDCD, KEDCD_TRD)
egen pair_id = group(KEDCD_min KEDCD_max)
la var pair_id "거래 쌍 고유 번호"

```


### 추가. 가중치 만들기 

* 1. 각 요소 정규화
```
foreach var in v15 v16 v17 {
    egen norm_`var' = std(`var') if v3 == 2  // 판매처 보고 기준만 사용
    la var norm_`var' "정규화된 `var' (판매처 보고)"
}
```

* 2. 거래 강도 기반 가중치 (Transaction Intensity)
```
gen w_intensity = 0.7*norm_v15 + 0.3*norm_v17 if v3 == 2
la var w_intensity "거래강도 가중치 (거래액+기간)"
```
거래 강도 기반 접근 (Transaction Intensity):
- 거래액(v15)과 거래기간(v17)의 조합
- 이론적 근거: 거래비용이론(TCE)에서 거래 특수성과 빈도가 관계의 강도를 결정
strength_index = w1*(normalized_v15) + w2*(normalized_v17)


* 3. 관계 투자 기반 가중치 (Relationship Investment)
```
gen w_investment = 0.6*norm_v16 + 0.4*norm_v17 if v3 == 2
la var w_investment "관계투자 가중치 (거래비용+기간)"

```

관계 투자 기반 접근 (Relationship Investment):
- 거래비용(v16)과 거래기간(v17)의 조합
- 이론적 근거: 관계특수투자이론에서 장기적 관계에 대한 투자 강조
investment_index = (v16) * (v17/max_duration)

* 4. 상호의존도 가중치 (Interdependence)
```
gen dep_ratio = v15/SALES if v3 == 2
egen norm_dep = std(dep_ratio) if v3 == 2
gen w_dependence = 0.7*norm_dep + 0.3*norm_v17 if v3 == 2
la var w_dependence "상호의존도 가중치 (매출비중+기간)"
```

상호의존도 기반 접근 (Interdependence):
- 거래액과 기업 규모(SALES) 대비 비중
- 이론적 근거: 자원의존이론(RDT)에서 거래 규모의 상대적 중요성 강조

dependence_index = (v15/SALES_buyer) * (duration_weight)

* 5. 복합 지표 가중치 (Factor Analysis)
```
factor norm_v15 norm_v16 norm_v17 if v3 == 2, pcf
predict w_factor if v3 == 2
la var w_factor "요인분석 기반 복합 가중치"
```

복합 지표 접근 (Composite Index):
- 주성분분석(PCA) 또는 요인분석을 통한 통합
- 이론적 근거: 다차원 관계 특성의 잠재 구조 파악

* 6. 통합 가중치 (Integrated)
```
gen w_integrated = 0.5*norm_v15 + 0.3*norm_v17 + 0.2*norm_v16 if v3 == 2
la var w_integrated "통합 가중치 (거래액+기간+비용)"
```

계층적 가중치 접근 (Hierarchical Weighting):
- 거래 특성별 중요도를 계층적으로 구성
- 이론적 근거: 분석적 계층화 과정(AHP) 방법론
final_weight = w1*(transaction_importance) + 
               w2*(relationship_duration) + 
               w3*(cost_commitment)




```
* 기본 통계 출력
di "=== 최종 네트워크 데이터 요약 ==="
di "전체 관측치 수:"
count
di "유니크 거래쌍 수:"
distinct pair_id
di "판매처 기준 가중치가 있는 케이스:"
count if !missing(w_integrated)

* 가중치간 상관관계 확인
corr w_* if v3 == 2

* 최종 통계
di "=== 최종 데이터 요약 ==="
di "전체 관측치 수:"
count
di "유니크 KEDCD 수:"
distinct KEDCD
di "유니크 KEDCD_TRD 수 (결측 제외):"
distinct KEDCD_TRD if !missing(KEDCD_TRD)


* 최종 데이터 정리 및 내보내기

order year KEDCD ENP_NM ENP_NM_ENG ENP_NM_TRD ENP_SZE SALES KEDCD_TRD v15 v16 v17 v3 v7 data_stat, first
gsort -SALES data_stat KEDCD KEDCD_TRD

keep KEDCD KEDCD_TRD data_stat v3 pair_id ///
     w_intensity w_investment w_dependence w_factor w_integrated ///
     ENP_SZE SALES ENP_NM v7 

* csv 내보내기
export delimited using "your_data_path\KED_2022_ntwk_data.csv", ///
    delimiter(";") quote replace
```

