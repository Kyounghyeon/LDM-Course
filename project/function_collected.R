#Kakao API로부터 주소 -> 좌표 추출하는 함수
get_coord_from_addr <- function(addr) {
    browser()
    data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = addr),
        add_headers(Authorization = paste0("KakaoAK ", "4f732d4bcbe257beeda49cfd137d95e6"))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  
  lon_lat_df <- tibble(ADDRESS = addr, 
                       lat = as.numeric(data_list$documents$y),
                       long = as.numeric(data_list$documents$x))
  
  return(lon_lat_df)
}

#Kakao API로부터 도로명 -> 좌표 추출하는 함수
get_coord_from_road <- function(road_name) {

  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = road_name),
        add_headers(Authorization = paste0("KakaoAK ", "4f732d4bcbe257beeda49cfd137d95e6"))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  
  lon_lat_df <- tibble(ADDRESS = road_name, 
                       lat = as.numeric(data_list$documents$y),
                       long = as.numeric(data_list$documents$x))
  
  return(lon_lat_df)
}



#Kakao API로부터 주소 -> 행정동 추출하는 함수
get_hdong_from_addr <- function(addr) {

  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = addr),
        add_headers(Authorization = paste0("KakaoAK ", "4f732d4bcbe257beeda49cfd137d95e6"))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  if (data_list$documents %>% length() == 0){
    dong_df <- tibble(ADDRESS = addr, 
                      행정동코드 = NA,
                      행정동명 = NA)
  }else if(data_list$documents$address %>% is.na){
    dong_df <- tibble(ADDRESS = addr, 
                      행정동코드 = NA,
                      행정동명 = NA)
  }else{
  
    dong_df <- tibble(ADDRESS = addr, 
                         행정동코드 = as.numeric(data_list$documents$address$h_code) %/% 100,
                         행정동명 = as.character(data_list$documents$address$region_3depth_h_name))
  }
  return(dong_df)
}

#좌표로 행정동 받아오자,,,,,,
get_hdong_from_coord <- function(x,y) {

  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json?input_coord=WGS84&output_coord=WGS84',
        query = list(x=x,
                     y=y),
        add_headers(Authorization = paste0("KakaoAK ", "4f732d4bcbe257beeda49cfd137d95e6"))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
    dong_df <- tibble(long = x,
                      lat = y,
                      행정동코드 = as.numeric(data_list$documents$code[2]) %/% 100,
                      행정동명 = as.character(data_list$documents$region_3depth_name[2]))
  return(dong_df)
}

# 대여이력 load + 전처리 함수
read_bike_record1 = function(year=2020, month){


  file_name = str_c("bike/rent_record/공공자전거 대여이력 정보_", year,".", month,".csv")
  
  # drop을 사용해서 대여/반납소 이름, 대여/반납소 남아있는 자전거 수 제거
  # QR형 자전거는 대여/반납소 잔여 개수에 기록되지 않기 때문에 남아있는 자전거 수 열을 제거함
  bike_data = fread(file_name, drop=c(4,5,8,9))
  
  
  bike_data = bike_data %>%
    
    #indexing을 위해 영어로 열 이름 변경
    rename(
      bike_no = "자전거번호",
      rent_date = "대여일시",
      # "대여 대여소번호" 또는 "대여.대여소번호"로 열 이름이 존재해서 regular expression 사용
      rent_id = str_subset(colnames(bike_data), pattern = "대여.대여소번호"),
      return_date = "반납일시",
      return_id = "반납대여소번호",
      time_used = "이용시간",
      distance_moved = "이용거리") %>% 
  
    
    #자료형에 맞게 변환
    mutate(rent_date = as_datetime(rent_date),
           rent_id = as.numeric(rent_id),
           return_date = as_datetime(return_date),
           return_id = as.numeric(return_id),
           time_used = as.numeric(time_used),
           distance_moved = as.numeric(distance_moved)) %>% 
    
    #NA있는 행 drop(매우 적음; 10개 미만)
    drop_na %>% 
    
    #이용시간, 이용거리 0인 행 제거 (의미 없다고 판단)
    filter(time_used != 0 & distance_moved != 0) %>% 
    
    # 대여소 고유 번호는 20210131 기준 101번부터 4770번까지 있기 때문에 해당되지 않는 대여소 번호 제거
    filter(rent_id > 100 & rent_id < 5000) %>% 
    
    # 대여일자 기준 데이터
    filter(rent_date >= ymd(str_c(year,month,"01")))

return(bike_data)
}

# 대여이력(시간별)+연령+나이대 load + 전처리 함수
read_bike_record2 = function(year=2020, month){

  file_name = str_c("bike/rent_record/hour/공공자전거 이용정보(시간대별)_", year, ".", month,".csv")
  bike_data = fread(file_name) %>% 
    
    #indexing을 위해 영어로 열 이름 변경
    rename(rent_date = "대여일자",
           rent_hour = "대여시간",
           rent_id = "대여소번호", 
           type = "대여구분코드", 
           age = "연령대코드",
           gender = "성별",
           count = "이용건수",
           distance_moved = "이동거리", 
           time_used = "사용시간") %>%
    dplyr::select(-c(대여소명, 운동량, 탄소량)) %>% 
    
    # 대여소 고유 번호는 20210131 기준 101번부터 4770번까지 있기 때문에 해당되지 않는 대여소 번호 제거
    filter(rent_id > 100 & rent_id < 5000)
  
  return(bike_data)
}


# 대여이력과 좌표 정보 합치는 함수
get_bike_coord_data= function(year = 2020, month){

  
  # 대여이력 저장
  bike_rent_record = read_bike_record1(year=year, month=month)
  
  # 대여시 대여소 좌표 join
  data1 = full_join(bike_rent_record, bike_place, by=c("rent_id"="ID"))  
  data1 = data1 %>%
    rename(rent_gu = "GU",
           rent_address = "ADDRESS",
           rent_lat = "lat",
           rent_long = "long")
  
  
  # 반닙시 대여소 좌표 join
  data2 = full_join(data1, bike_place, by=c("return_id"="ID"))
  data2 = data2 %>%
    rename(return_gu = "GU",
           return_address = "ADDRESS",
           return_lat = "lat",
           return_long = "long")
  
  
  # 열 순서 변경
  full_bike_rent = data2 %>%
    relocate(contains("rent"), .before = contains("return")) %>% 
    relocate(contains("return"), .after = contains("rent")) %>% 

    
  #NA행 drop, 20210131 기준으로 없어진 대여소 존재하기 때문   
    drop_na
  
  return(full_bike_rent)
}

# 대여이력(시간별)+연령+나이대 데이터와 좌표 정보 합치는 함수
get_bike_coord_data_hour= function(year = 2020, month){
  
  # 대여이력 저장
  bike_rent_record = read_bike_record2(year=year, month=month)
  
  # 대여시 대여소 좌표 join
  data1 = full_join(bike_rent_record, bike_place, by=c("rent_id"="ID"))  
  data1 = data1 %>%
    rename(rent_gu = "GU",
           rent_address = "ADDRESS",
           rent_lat = "lat",
           rent_long = "long")
  
  
  # 반납시 대여소 좌표 join
  data2 = full_join(data1, bike_place, by=c("rent_id"="ID"))
  full_bike_rent = data2 %>%
    rename(return_gu = "GU",
           return_address = "ADDRESS",
           return_lat = "lat",
           return_long = "long")
  
  
  #열 순서 변경
  full_bike_rent = full_bike_rent %>%
    relocate(contains("rent"), .before = contains("return")) %>% 
    relocate(contains("return"), .after = contains("rent")) %>% 
    
    drop_na
  
  return(full_bike_rent)
}


#시간별 미세먼지 데이터 받는 함수
#입력값 YYYYMMDD
get_dust_data = function(start=1, end=600, date){
  
  #
  date = as.numeric(date)
  
  #초기값
  url = 'http://openapi.seoul.go.kr:8088'
  auth_key = "6a4d47485a77686f37386575577171"
  TYPE = "xml"
  SERVICE = "TimeAverageAirQuality"
  START_index = start
  END_index = end
  date = date
  query=paste(TYPE, SERVICE, START_index, END_index, date, sep="/")
  API_url = paste(url, auth_key, query, sep="/")
  
  #XML Parsing
  dust_tibble = xmlParse(API_url) %>% getNodeSet("//row") %>% xmlToDataFrame() %>% as_tibble
  
  return(dust_tibble)
}

#시간별 미세먼지 데이터 필요한 부분만 이용하도록 전처리하는 함수
#입력값 YYYYMMDD
manipulate_dust_data = function(date){
  
  dust_data = get_dust_data(date = date) 
  
  rename( GU = "MSRSTE_NM") %>% 
    #미세먼지(PM25), 초미세먼지(PM10) 정보만 남기기
    dplyr::select(-(NO2:SO2)) %>% 
    
    #날짜형으로 변경
    mutate(
      date = ymd_hm(MSRDT),
      PM10 = as.numeric(PM10),
      PM2.5 = as.numeric(PM25), .keep="unused") %>% 
    
    #날짜와 시간 분리
    mutate(
      hour = hour(date),
      date = as_date(date)) %>% 
    
    #열 정렬
    relocate(date, hour) %>% 
    
    #시간별 미세먼지 평균
    group_by(date, hour) %>% 
    summarise(PM10 = mean(PM10, na.rm=T) %>% round,
              PM2.5 = mean(PM2.5, na.rm=T) %>% round) %>% 
    ungroup
  
  
  return(dust_data)
  
}


# 시간별 날씨 자료 받아오는 함수
get_weather_data = function(date){
  weather_api_key ="L53A6pHe7VGrjMITfOcdQxp8pHW001Yt1mWmaS9DCel07PQk0vLbp3secPxEsBTnw1yoGj8iX9q7PJmwRE%2FC%2Fw%3D%3D"
  weather_api_url = "http://apis.data.go.kr/1360000/VilageFcstInfoService/getUltraSrtFcst"
  cent_long = 126.979108	
  cent_lat = 37.55963
  weather_date = 20200101
  time = 0000
  
  
  weather_url = str_c(weather_api_url,
                      "?serviceKey=",weather_api_key,
                      "&numOfRows=",10,
                      "&pageNo=",1,
                      "&base_date=",weather_date,
                      "&base_time=",time,
                      "&nx=",cent_lat,
                      "&ny=",cent_long)
  
  
  #XML Parsing
  weather_tibble = xmlParse(weather_url) %>% getNodeSet("//row") %>% xmlToDataFrame() %>% as_tibble
  
  
  return(weather_tibble)
  
}


#이용건수(대여)에 따라 구 별로 plotting 하는 함수
plot_bike_count_by_gu = function(gu){
  
  register_google(key = "AIzaSyB7U3Nnn7N05oTyHhlsjIxZ9uUfbAVTTDU")
  
  subway = fread("place/서울시_지하철역_좌표.csv")
  hanriver = fread("place/한강공원_좌표.csv")
  
  plt =
    bike_count_by_place %>%
    filter(rent_gu == gu) %$%
    get_googlemap(center = c(mean(rent_long), mean(rent_lat)),
                  zoom = 14,
                  size = c(750,750),
                  maptype = "roadmap",
                  color = "bw") %>%
    ggmap() +
    
    #대여소
    geom_point(data = bike_count_by_place %>% filter(rent_gu == gu),
               aes(x=rent_long, y=rent_lat, size=count), color = "#006633", alpha = .5) +
    scale_size_continuous(range = c(1,6)) +
    
    #지하철역
    geom_point(data = subway,
               aes(x=lon, y=lat), size = 3, shape = 15, color = "#E66E0B", alpha = .7) +
    
    #한강공원
    geom_point(data = hanriver,
               aes(x=lon, y=lat), size = 5, shape = 25, color = "#07B2EC", alpha = .7)+
    
    labs(title = str_c(gu," 대여소"))+
    my_theme +
    theme(axis.title.y = element_blank())
  
  return(plt)
}


get_accident_data = function(gu_Cd){
  
  url = "http://taas.koroad.or.kr/data/rest/frequentzone/bicycle?authKey="
  auth_key = "08QSC0qhFE59rJPR6hb3dqPJy%2BMlFjBbhffEtGvbvDSHEYdp1Sww5CtNYOVHup4%2F"
  sido = 11
  gu = as.numeric(gu_Cd)
  query=paste0("&searchYearCd=2020037",
               "&sido=",sido,
               "&guGun=",gu,
               "&type=xml",
               "&numOfRows=100")
  
  API_url = paste0(url,auth_key,query)
  
  #XML Parsing
  result_code = xmlParse(API_url) %>% getNodeSet("//resultCode") %>% xmlToDataFrame()
  
  if (result_code[,"text"] != "00" ){
    return(NULL)
  }
  
  acc_raw = xmlParse(API_url) %>% getNodeSet("//item") 
  acc_tb = acc_raw %>% xmlToDataFrame() %>% as_tibble
  
  return(acc_tb)
}


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}