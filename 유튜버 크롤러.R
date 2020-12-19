
# 1. 준비
## 1.1. 필요한 라이브러리 설치
library(tuber)
library(tidyverse)
library(lubridate)
library(stringi)
library(wordcloud)
library(gridExtra)

#install.packages("wordcloud")
library("wordcloud")

## 1.2. API 권한 받기
#4번 api 사용
client_id <- "id"
client_secret <- "secret"

yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')

# 2. 유튜버 크롤링 하기

## 2.1. 유튜버 채널을 저장하기 

chstat = get_channel_stats("channel_stats")

## 2.2. 유튜버 채널의 비디오 가져오기
### 가져올 비디오를 검색하고
videos = yt_search(term="검색어", type="video", published_after="2020-01-01T00:00:00Z",published_before='2020-01-10T00:00:00Z')


videos = videos %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date >= "2020-01-01", date <= "2020-01-02") %>%
  arrange(date)

## 2.3. Comments 가져오기
comments = lapply(as.character(videos$video_id), function(x){
  get_comment_threads(c(video_id = x),max_results = 20) 
})

# 3. 전처리
## 3.1. Video 별로 가져오기
### videostats,date, title, viewCount, likeCount, dislikeCount, commentCount 가져오기
videostats = lapply(as.character(videos$video_id), function(x){
  get_stats(video_id = x)
})
videostats = do.call(rbind.data.frame, videostats)
videostats$title = videos$title #video title
videostats$date = videos$date # video date
videostats = select(videostats, date, title, viewCount, likeCount, dislikeCount, commentCount) %>%
  as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount))) 

## 3.2. data Frame 로 유튜버 정보정리

genstat = data.frame(Channel="", Subcriptions=chstat$statistics$subscriberCount,
                     Views = chstat$statistics$viewCount,
                     Videos = chstat$statistics$videoCount, Likes = sum(videostats$likeCount),
                     Dislikes = sum(videostats$dislikeCount), Comments = sum(videostats$commentCount))


# 4. 시각화
## 4.1. 조회수 대비 좋아요, 싫어요, 댓글 수 시각화
p1 = ggplot(data = videostats[-1, ]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = videostats[-1, ]) + geom_point(aes(x = viewCount, y = dislikeCount))
p3 = ggplot(data = videostats[-1, ]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, p3, ncol = 2)

# 5. 코멘트 관련
## 5.1. 코멘트 날짜별로 배열 
comments_ts = lapply(comments, function(x){
  as.Date(x$publishedAt)
})

### 날짜별로 comment count group by
comments_ts = tibble(date = as.Date(Reduce(c, comments_ts))) %>%
  group_by(date) %>% count()

## 5.2. comment count 시각화
### 날짜별로 시각화
ggplot(data = comments_ts) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-12-01")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype = 2,color = "red")

## 5.3. Top Comment 4개 모아서 추출하기
selected = (nrow(videostats) - 3):nrow(videostats)
top4 = videostats$title[selected]
top4comments = comments[selected]

p = list() #빈리스트만들기

for(i in 1:4){
  df = top4comments[[i]]
  df$date = as.Date(df$publishedAt)
  df = df %>%
    arrange(date) %>%
    group_by(year(date), month(date), day(date)) %>%
    count()
  df$date = make_date(df$`year(date)`, df$`month(date)`,df$`day(date)`)
  p[[i]] = ggplot(data=df) + geom_line(aes(x = date, y = n)) + ggtitle(top4[i])
}
do.call(grid.arrange,p)

## 5.3. comments 내용 가져오기
comments_text = lapply(comments,function(x){
  as.character(x$textOriginal)
})

## 5.4. 전처리과정에서 제외할 단어 선정하기
comments_text = tibble(text = Reduce(c, comments_text)) %>%
  mutate(text = stri_trans_general(tolower(text), "Latin-ASCII"))
remove = c("ㅋ","계속 추가")

## 5.5. WordCloud 만들기
words = tibble(word = Reduce(c, stri_extract_all_words(comments_text$text))) %>%
  group_by(word) %>% count() %>% arrange(desc(n)) %>% filter(nchar(word) >= 3) %>%
  filter(n > 10 & word %in% remove == FALSE) 
set.seed(3)
wordcloud(words$word, words$n, random.order = FALSE, random.color = TRUE,
          rot.per = 0.3, colors = 1:nrow(words))
