# LDM-Course

성균관대학교 통계학과의 *대용량자료관리및시각화* 수업을 통해 진행한

- **두 개의 간단한 과제** - HW1. 전처리 & HW2. 시각화
- **서울시 공공자전거 대여소의 지역적 특징 분석을 통한 자전거 도로 신설 제안** - 개인 프로젝트

를 담았습니다.

사용한 언어는 모두 R이며, 다음과 같은 코드를 통하여 개인 프로젝트의 결론을 `shiny app`을 통하여 보실 수 있습니다.

This repo consist with assignments and a project from the course **Large Data Management and Visualization(STA3034)** in SKKU.
The topic of the project is "Proposal for the new bicycle road by analyzing regional characteristics of public bicycle in Seoul"
You can see the conclustion of this project through `R` code below.

```
packages = c("tidyverse", "shiny", "data.table")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

runGithub("LDM-shiny, "Kyounghyeon", ref = "main") 
```
