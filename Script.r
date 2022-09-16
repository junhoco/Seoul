#### 10-1 ####

## -------------------------------------------------------------------- ##
#패키지 설치
#   *********************************************
#   ***** 관리자권한으로 설치해야 한다  **********
#   *********************************************
install.packages("rJava")
install.packages("multilinguer")

multilinguer::install_jdk()

install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex",
                   "lazyeval", "htmlwidgets", "crosstalk", "promises", "later",
                   "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr",
                   "covr", "DT", "rcmdcheck", "rversions"), type = "binary")

install.packages("KoNLP", 
                 repos = "https://forkonlp.r-universe.dev",
                 dependencies = TRUE,
                 INSTALL_opts = c("--no-multiarch")
)

install.packages("memoise")
install.packages("tm") # for text mining
install.packages("SnowballC") #SnowballC는 이 단어들을 뭉쳐줄 수 있는 패키지, # for text stemming
install.packages("RColorBrewer") # color palettes
install.packages("wordcloud")  #워드클라우드 차트 지원
install.packages("stringr")

#다 해도 에러날때 에러처리하기 - 파일을 직접 다운받아 설치해야 한다 
#https://always-apramada.tistory.com/129
#https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.8/

#.libPaths()함수로 위치값 찾기 
#KoNLP - java 에 scala-library-2.11.8.jar 다운받아 붙여넣기
# 패키지 로드
library(hash)
library(tau)
library(Sejong)
library(KoNLP)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)


useNIADic()
useSejongDic()
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다") #명사만 추출하기 



setwd("C:/r_workspace/Data")

# 데이터 불러오기
txt <- readLines("hiphop.txt")
head(txt)

# 특수문제 제거
txt <- str_replace_all(txt, "\\W", " ")


## -------------------------------------------------------------------- ##
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다") #명사만 추출하기 

# 가사에서 명사추출
nouns <- extractNoun(txt)

#extractNoun 함수가 list  타입으로 가져온다 
# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
#unlist - list타입으로 온 데이터를 vector f로 바꾼다 
#table함수가 vector 만 취급한다. list는 안된다. 
wordcount <- table(unlist(nouns))
wordcount 

# 데이터 프레임으로 변환 문자열은 factor  형태로 전환한다 
df_word <- as.data.frame(wordcount, stringsAsFactors = F)

# 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
head(df_word)

# 두 글자 이상 단어 추출
# nchar(문자열) - 문자열의 개수를 반환하는 함수 
df_word <- df_word %>% filter(nchar(word) >= 2)

#빈도수가 많은것부터 20개만 추출하기 
top_20 <- df_word %>%
        arrange(desc(freq)) %>%
        head(20)
top_20



pal <- brewer.pal(8,"Dark2")  # Dark2 색상 목록에서 8개 색상 추출

set.seed(3) #seed - 컴퓨터안에서 랜덤값을 추출한다 
               #정수를 준다 
wordcloud(words = df_word$word,  # 단어
          freq = df_word$freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal)          # 색깔 목록



brewer.pal.info


pal <- brewer.pal(9,"Blues")[5:9]  # 색상 목록 생성

pal <- brewer.pal(11,"RdBu")  # Dark2 색상 목록에서 8개 색상 추출
set.seed(1234)                     # 난수 고정

wordcloud(words = df_word$word,    # 단어
          freq = df_word$freq,     # 빈도
          min.freq = 4,            # 최소 단어 빈도
          max.words = 300,         # 표현 단어 수
          random.order = T,        # 고빈도 단어 중앙 배치
          rot.per = .1,            # 회전 단어 비율
          scale = c(4, 0.3),       # 단어 크기 범위
          colors = pal)            # 색상 목록


#### 10-2 ####

## -------------------------------------------------------------------- ##
# 데이터 로드
twitter <- read.csv("twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")

class(twitter)
head(twitter)
str(twitter)



# 변수명 수정
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)

# 특수문자 제거 

twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)

# 트윗에서 명사추출 - list타입으로 보낸다.  
nouns <- extractNoun(twitter$tw)
nouns[1:20]

#table( nouns ) #에러발생 
ul <- unlist(nouns)
ul[1:40]
#unlist - list -> vector로 바꾸기 
# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount[1:10 ]



# 데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)

# 변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)


# 두 글자 이상 단어만 추출
df_word <- filter(df_word, nchar(word) >= 2)

# 상위 20개 추출
top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)

top20



library(ggplot2)

order <- arrange(top20, freq)$word               # 빈도 순서 변수 생성
order
ggplot(data = top20, aes(x = word, y = freq)) +  
  ylim(0, 2500) +
  geom_col() + 
  coord_flip() + #수평으로 
  scale_x_discrete(limit = order) +  # 빈도 순서 변수 기준 막대 정렬
  geom_text(aes(label = freq), hjust = -0.3)     # 빈도 표시








pal <- brewer.pal(8,"Dark2")       # 색상 목록 생성
set.seed(1234)                     # 난수 고정

wordcloud(words = df_word$word,    # 단어
          freq = df_word$freq,     # 빈도
          min.freq = 10,           # 최소 단어 빈도
          max.words = 200,         # 표현 단어 수
          random.order = F,        # 고빈도 단어 중앙 배치
          rot.per = .1,            # 회전 단어 비율
          scale = c(4, 0.2),       # 단어 크기 범위
          colors = pal)            # 색상 목록


pal <- brewer.pal(9,"Blues")[5:9]  # 색상 목록 생성
set.seed(1234)                     # 난수 고정

wordcloud(words = df_word$word,    # 단어
          freq = df_word$freq,     # 빈도
          min.freq = 10,           # 최소 단어 빈도
          max.words = 200,         # 표현 단어 수
          random.order = F,        # 고빈도 단어 중앙 배치
          rot.per = .1,            # 회전 단어 비율
          scale = c(6, 0.2),       # 단어 크기 범위
          colors = pal)            # 색상 목록

wordcloud(words=df_word$word, 
	freq =df_word$freq, 
	scale=c(6,0.5), 
	rot.per=0.1, 
	min.freq=10, 
	max.words=200,
	random.order=F, 
	random.color=T, 
	colors=pal)

windows() # 윈도우형태로 출력하기 위해 함수 호출 ; 
#windows()함수를 실행안하면 savePlot() 에서 에러 발생

display.brewer.all()
palete = brewer.pal(9,"Set1")
wordcloud(words=df_word$word, 
	freq=df_word$freq,  
	scale=c(5,.5), 
	rot.per= .25, 
	min.freq=10, 
	max.words=200,
	random.order=F, 
	random.color = T, 
	colors=palete)
savePlot(filename="a.jpg") #작업디렉토리에 저장하기 
getwd()

########### wordcloud2 ####################
#https://blog.naver.com/PostView.nhn?blogId=regenesis90&logNo=222264793312
#https://alcemist.tistory.com/36

install.packages('wordcloud2')
install.packages('tidyverse')
install.packages('nord')

library(tidyverse)
library(wordcloud2)
library(nord)

wordcloud2(df_word)

wordcloud2(df_word, color='random-light')

#circle / cardioid / diamond / triangle-forward / triangle / pentagon / star


wordcloud2(df_word,color=nord(palette='afternoon_prarie'),
        shape='star')

dim(df_word)
df <- df_word %>% arrange( desc(freq)) %>% head(300) 
dim(df)
wordcloud2(df,color='random-light',size=0.6,shape='diamond')



wordcloud2(df, color = "skyblue",size=1,
           figPath="C:/r_workspace/Data/images/apple.png",
           backgroundColor = "black" )

letterCloud(df, word = "K", size = 2)
wordcloud2(df,  figPath="C:/r_workspace/Data/images/apple.png", size = 1.5, 
                color = "skyblue", backgroundColor="black")


#차트그리기 
#변수를 하나만 줄때는 y축으로 판단한다 
x<-c(1,2,3)
plot(x)

x<-c(2,4,6,8,10)
plot(x)


#x축, y축
x<-c(1,2,3,4,5)
y<-2*x+1
plot(x, y)
x
y

#x축에 그림 
x<-1:10
y<-rep(2,10) #반복해서 데이터 만들기 
plot(x, y)

x<-rep(3,10)
y<-seq(from=1, to=20,by=2)
plot(x,y)

#제목 붙이기 
y<- c(170, 165, 180, 156, 167)
x <- c(65, 54, 73, 44, 60)
plot(x, y, main="키와몸무게의 상관관계")

plot(x, y, xlim=c(1,100), ylim=c(1,190), main="키와몸무게의 상관관계", xlab="몸무게", ylab="키")
plot.new() #기존에 만든걸 모두 지운다 

#그림이 제멋대로 나온다 - 정렬하자 (꼭정렬하자)
xx<-x[order(x)]
yy<-y[order(x)]

plot(xx, yy, type="o", col="red",main="키와몸무게의 상관관계", xlab="몸무게", ylab="키")

#dataframe으로 만들어서 정렬할 수 도 있다 
data <- data.frame(x=x, y=y)
data

library(dplyr)
data <- data %>% arrange(x)

install.packages("plyr")
library(plyr)
data<- arrange(data, data$x)
#   x   y
#1 44 156
#2 54 165
#3 60 167
#4 65 170
#5 73 180


plot(data$x, data$y, type="p") #점모양
plot(data$x, data$y, type="l") #선모양
plot(data$x, data$y, type="b") #점과 선모양
plot(data$x, data$y, type="c") #b애서 점 생략
plot(data$x, data$y, type="o") #점과 선을 중첩
plot(data$x, data$y, type="h") #각 점에서 x축까지의 수직선
plot(data$x, data$y, type="s") #왼쪽값을 기준으로 계단모양
plot(data$x, data$y, type="S") #오른쪽값을 기준으로 계단모양
plot(data$x, data$y, type="n") #축만 그림


#배경색 
par(bg = "light cyan")

#r에서는 빛의 강도를 0~1까지로 한다 rgb(0,0,1) #0000ff 
plot(data$x, data$y, col=rgb(0,0,0.9))


plot(data$x, data$y, ann=F)
plot(data$x, data$y, ann=F, type="o", col=rgb(1,0,1))
title(xlab="x축", col.lab="blue")
title(ylab="y축", col.lab="pink")
title(main='키와몸무게', font.main=4, col.main='red')

plot(1:8, rep(1,8), axes = FALSE, col=1:8, pch=16, cex=2, xlab=NA, ylab=NA)
axis(1, sprintf("%i", 1:8),col="white")

col.lst <- c("blue" , "red", "green" , "bisque", "grey20", "grey90", "green2", "olivedrab2")
plot(1:8, rep(1,8), axes = FALSE, pch=16, cex=2, col=col.lst, xlab=NA, ylab=NA,
     xlim = c(0, 10))
axis(1, at=1:8, labels = sprintf("%s", col.lst), col="white", cex=0.6, padj=-3)


plot( speed ~ dist, cars, pch=16, col=rgb(0,0,1))
#투명도 
plot( Sepal.Width ~ Sepal.Length, iris , pch=16, col=rgb(0,0,1, 0.2))

plot( x ~ y, data , pch=16, col=rgb(0,0,1, 0.2))

data$z <- c(30, 20, 10, 15, 20)
plot( x ~ y, data , pch=16, col=rgb(0,0,1, 0.2))
plot( x ~ z, data , pch=16, col=rgb(0,0,1, 0.2))
plot( y ~ z, data , pch=16, col=rgb(0,0,1, 0.2))


#한 화면에 여러개의 차트를 그립시다 
par(mfrow=c(1,3))
plot( x ~ y, data , pch=16, col=rgb(0,0,1, 0.9))
plot( x ~ z, data , pch=16, col=rgb(0,0,1, 0.9))
plot( y ~ z, data , pch=16, col=rgb(0,0,1, 0.9))

plot( y ~ z, data , pch=16, col=rgb(0,0,1, 0.9), type='o')
plot( y ~ z, data , pch=16, col=rgb(0,0,1, 0.9), type='s')
plot( y ~ z, data , pch=16, col=rgb(0,0,1, 0.9), type='l')


pie(data$x)
plot(data$x, type='o')
barplot(data$x)

#분할화면 지우기 
par(mfrow=c(1,1))
dev.off()

#그래프 그릴때 여백조절하기 
plot.new()
a<-c(1,2,3)
plot(a, xlab="aaa")


par("mar")
#[1] 5.1 4.1 4.1 2.1
par("mgp")
#[1] 3 1 0
par("las")
#[1] 0

#mar – A numeric vector of length 4, which sets the margin sizes in the following order: 
#bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
#mgp – A numeric vector of length 3, which sets the axis label locations relative to 
#the edge of the inner plot window. 
#The first value represents the location the labels (i.e. xlab and ylab in plot), 
#the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
#las – A numeric value indicating the orientation of the tick mark labels and any other
#text added to a plot after its initialization. The options are as follows: 
#always parallel to the axis (the default, 0), 
#always horizontal (1), always perpendicular to the axis (2), and always vertical (3).


plot.new()
v1 <- c(1,2,3,4,5)
v2 <- c(5,4,3,2,1)
v3 <- c(3,4,5,6,7)
plot(v1, type="s", col='red', ylim=c(1,5))
plot(v2, type="o", col='blue', ylim=c(1,5))

plot(v1, type="s", col='red',  ylim=c(1,5))
par(new=T)
plot(v2, type="o", col='blue', pch=12, ylim=c(1,5))
par(new=T)
plot(v3, type="l", col='green',   ylim=c(1,5))
 

help(pch) #0~25


plot(v3, type="l", col='green', pch=25,  ylim=c(1,5))
#In plot.xy(xy, type, ...) : pch의 값 '26'는 구현되지 않았습니다
plot(v3, type="o", col='green', pch=1,  ylim=c(1,5))
plot(v3, type="o", col='green', pch=2,  ylim=c(1,5))
plot(v3, type="o", col='green', pch=15,  ylim=c(1,5))
plot(v3, type="o", col='green', pch=16,  ylim=c(1,5))
plot(v3, type="o", col='green', pch=17,  ylim=c(1,5))


#범례추가하기 
v1 <- c(1,2,3,4,5)
v2 <- c(5,4,3,2,1)
v3 <- c(3,4,5,6,7)
plot(v1, type="s", col='red', ylim=c(1,5))
lines(v2, type="o", col='blue', pch=16, ylim=c(1,5))
lines(v3, type="l", col='green',  ylim=c(1,5))
legend(4,5,c("v1", "v2", "v3"), cex=0.9, col=c('red', 'green', 'blue'), lty=1)
legend("topright",c("v1", "v2", "v3"), cex=0.9, col=c('red', 'green', 'blue'), lty=1)
legend("topleft",c("v1", "v2", "v3"), cex=0.9, col=c('red', 'green', 'blue'), lty=1)
legend("bottomleft",c("v1", "v2", "v3"), cex=0.9, col=c('red', 'green', 'blue'), lty=1)

##########문제풀이 1
#	홍길동	임꺽정	장길산	홍경래	홍준표	하태경
#1월	2000	3000	1700	2500	1100	1700
#2월	1500	3200	2500	2600	600	1500
#3월	2700	2900	2300	2700	900	900
#4월	2000	2400	2200	2400	1500	800
#5월	2800	3500	2500	2300	1200	700
#6월	1700	1600	1700	2200	1400	600

data <- read.csv("c:/Study/실적2.csv", header=T)
name <- names(data)[2:length(names(data))]
colors <- c("brown", "green", "blue", "red", "pink", "cyan", "orange")
for( i in seq(length(name)))
{	
	print(i+1)
	if( i==1)
		plot(data[,i+1], col=colors[i], type="o", pch=19,ylim=c(500,5000), 
		axes=F, ann=F)
	else
	{
		lines(data[,i+1], col=colors[i], type="o", pch=19)
	}
}

title(main='영업실적')
title(xlab="실적")
title(ylab="이름")
axis(1,at=1:6, lab=name)
axis(2,ylim=c(500, 5000))


legend("topright", name, cex=0.9,col=colors,lty=1)




##########문제풀이2
#이름,1월,2월,3월,4월,5월,6월,color
#홍길동,2000,1500,2700,2000,2800,1700,brown
#임꺽정,3000,3200,2900,2400,3500,1600,green
#장길산,1700,2500,2300,2200,2500,1700,blue
#홍경래,2500,2600,2700,2400,2300,2200,pink
#홍준표,1100,600,900,1500,1200,1400,red
#하태경,1700,1500,900,800,700,600,cyan

data <- read.csv("c:/Study/실적.csv", header=T)
data[1,]
data[2,]
plot(c(1,2,3,4,5,6), data[2,2:7], type="o")
rows <- nrow(data) #행의 개수 
cols <- ncol(data) #열의 개수 
colors=rainbow(ncol)
colors
colors=data[,ncol(data)]
colors

name<-names(data)[3:cols-1] #첫번째 열이 이름으로 나온다 
name

name <- substr(name, 2, 4)
name

x <- seq(cols-2)

for( i in seq(cols-1))
{	
	print(i+1)
	if( i==1)
		plot(x, data[i,3:cols-1], col=colors[i], type="o", pch=i,ylim=c(500,5000), 
		axes=F, ann=F)
	else
	{
		lines(x, data[i,3:cols-1], col=colors[i], type="o", pch=i)
	}
}

title(main='영업실적')
title(xlab="이름")
title(ylab="실적")
axis(1,at=1:6, lab=name)
axis(2,ylim=c(500, 5000))

legend("topright", as.vector(data[,1]), cex=0.9,col=colors,lty=1)




#barplot 그리기 
#막대형 그래프
v1<-sample(1:10, 5)
barplot(v1)# 세로로
barplot(v1, horiz=T)

#그래프가 누적으로 보인다 
x<-matrix(c(5,4,3,2), c(2,2))
x

barplot(x)# 세로로
barplot(x, horiz=T)

barplot(x, beside=T, names=c(5,3))# 세로로
barplot(x, beside=T, names=c(5,3), col=c('red', 'green'))
barplot(x, names=c(5,3), col=c('red', 'green'))


v1<-c(100, 120, 140, 160, 180)
v2<-c(120,130,150, 140, 170)
v3<-c(140, 170, 120, 110, 160)
data <- data.frame(banana=v1, orange=v2, cherry=v3)
barplot(as.matrix(data))
barplot(as.matrix(data), horiz=T, beside=T, col=rainbow(nrow(data)))
barplot(as.matrix(data), beside=T, col=rainbow(nrow(data)))

legend("topleft", c("MON", "TUE", "WED", "THU", "FRI"), cex=0.8, fill=rainbow(nrow(data)))
legend(14,200, c("MON", "TUE", "WED", "THU", "FRI"), cex=0.8, fill=rainbow(nrow(data)))
legend(14,120, c("MON", "TUE", "WED", "THU", "FRI"), cex=0.8, fill=rainbow(nrow(data)))



#그래프에 조건 부여하기 - 조건문을 이용해 그래프를 그려보자 
peach<-c(180, 200, 250, 198, 170)
colors<-c()
for(i in 1:length(peach))
{
	if( peach[i]>=200)
	{
		colors<-c(colors, "red")
	}
	else if(peach[i]>=180)
	{
		colors<-c(colors, "yellow")
	}
	else
	{
		colors<-c(colors, "green")
	}
}

colors
barplot(peach, beside=T, names.arg= c("MON", "TUE", "WED", "THU", "FRI"), col=colors)






#그래프에 조건 부여하기 - 조건문을 이용해 그래프를 그려보자 
peach<-c(180, 200, 250, 198, 170)
colors<-c()
for(i in 1:length(peach))
{
	if( peach[i]>=200)
	{
		colors<-c(colors, "red")
	}
	else if(peach[i]>=180)
	{
		colors<-c(colors, "yellow")
	}
	else
	{
		colors<-c(colors, "green")
	}
}
	

#문제풀기

name<-c("홍길동", "임꺽정", "장길산", "홍경래", "이징옥")
sub1<-c(100, 200, 100, 150, 200) 
sub2<-c(90, 130, 140, 200, 150)
sub3<-c(100, 190, 90, 100, 120)
sub4<-c(300, 340, 240, 200, 180)

data2 <- data.frame(name=name, word=sub1, excel=sub2, ppt=sub3, write=sub4)
data2$total <-data2$word+data2$excel+data2$ppt+data2$write
barplot(as.matrix(data2$total), beside=T, names.arg=data2$name, col=rainbow(nrow(data2)))
colors<-c()
for(i in 1:nrow(data2))
{
	if( data2$total[i]>=800)
	{
		colors<-c(colors, "red")
	}
	else if(data2$total[i]>=600)
	{
		colors<-c(colors, "yellow")
	}
	else if(data2$total[i]>=400)
	{
		colors<-c(colors, "blue")
	}
	else
	{
		colors<-c(colors, "green")
	}
}

barplot(as.matrix(data2$total), beside=T, names.arg=data2$name, col=colors)

#히스토그램 - 특정 데이터의 빈도수를 막대모양으로 표현한다 
#10명의 키 : 182, 175, 167, 172, 163, 178, 181, 166, 159, 155

#데이타의 특정 구간내에서의 빈도수를 표시한다 

#1.가장 큰값과 가장 작은값을 구한다 
#2. 최대값과 최소값 사이를 적절한 구간으로 나눈다 (5~8)
#3. 각 구간을 대표하는 대표값을 설정한다 (보통은 구간의 중간값을 구한다) 
#4. 각 구간의 값의 개수를 구한다(이 개수를 도수라 한다)
#5. 각 구간의 도수가 전체 값에서 차지하는 비중을 구한다(이를 상대도수라 한다)
#6. 각 구간의 도수를 누적해서 계산(이값을 누적 도수라고 한다) 

#도수 분포표 
height <- c( 182, 175, 167, 172, 163, 178, 181, 166, 159, 155)
hist(height)

hist(height, freq = FALSE)
hist(height, breaks=5)

#파이차트
x<-c(1,2,2,4,5)
pie(x)
pie(x, radius=1, init.angle=90)
pie(x, col=rainbow(length(x)), label=c("제주도", "유럽", "동남아시아", "미국", "서유럽"))
pct <- round(x/sum(x)*100, 1)
pct
lab<-paste(lab, pct, '%')
lab

pie(x, col=rainbow(length(x)), label=lab)
legend(1,1,1, c("제주도", "유럽", "동남아시아", "미국", "서유럽"), cex=0.5, fill=rainbow(length(x)))

pie(x, col=rainbow(length(x)), label=lab)
legend(1,1, c("제주도", "유럽", "동남아시아", "미국", "서유럽"), cex=0.8, fill=rainbow(length(x)))

pie(x, col=rainbow(length(x)), label=lab)
legend("bottomleft", c("제주도", "유럽", "동남아시아", "미국", "서유럽"), cex=0.8, fill=rainbow(length(x)))


lab<-c("제주도", "유럽", "동남아시아", "미국", "서유럽")
lab<-paste(lab, pct, '%')
lab
pie(x, col=rainbow(length(x)), label=lab)
legend("bottomleft", lab, cex=0.8, fill=rainbow(length(x)))

install.packages("plotrix")

library("plotrix")
pie3D(x, col=rainbow(length(x)), labels=lab)
pie3D(x, col=rainbow(length(x)), labels=lab, explode=0.05)
legend("bottomleft", lab, cex=0.8, fill=rainbow(length(x)))


 pieval<-c(2,4,6,8)
 pielabels<-
  c("We hate\n pies","We oppose\n  pies","We don't\n  care","We just love pies")
 # grab the radial positions of the labels
 lp<-pie3D(pieval,radius=0.9,labels=pielabels,explode=0.1,main="3D PIE OPINIONS")
 # lengthen the last label and move it to the left
 pielabels[4]<-"We cannot survive without our pies"
 lp[4]<-4.8
 # specify some new colors
 pie3D(pieval,radius=0.9,labels=pielabels,explode=0.1,main="3D PIE OPINIONS",
  col=c("brown","#ddaa00","pink","#dd00dd"),labelpos=lp)

#boxplot(상자차트)

v1<-c(10, 12, 15, 11, 20)
v2<-c(5,7,15,8,9)
v3<-c(11,20,15,18,13)
boxplot(v1, v2, v3)


boxplot(v1, v2, v3)
boxplot(v1, v2, v3, col=c('red', 'green', 'blue'))
boxplot(v1, col=c('red'))
summary(v1)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   10.0    11.0    12.0    13.6    15.0    20.0 
boxplot(v2, col=c('red'))
#v2
#[1]  5  7 15  8  9
summary(v2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    5.0     7.0     8.0     8.8     9.0    15.0 
v1<-c(1,30,2,3,4)
boxplot(v1, col=c('red'))
summary(v1)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      1       2       3       8       4      30 

#관계도 그리기(igraph)

install.packages("igraph") #오래걸린다 
library("igraph")

g1<-graph(c(1,2,  2,3,  2,4,  1,4,  5,5,  3,6))
plot(g1)

savePlot("network_1.png", type="png")

#지하철 노선도 
g2 <- graph(c("서울대입구", "낙성대", "낙성대", "사당", "사당", "방배"))
plot(g2)
g2 <- graph(c("서울대입구", "낙성대", "낙성대", "사당", "사당", "방배", "사당","이촌"))
plot(g2)


name<-c("A", "B", "C", "D", "E")
position<-c("B", "C", "D", "E", "A")
data3 <- data.frame(name=name, pos=position)
g3 <- graph.data.frame(data3, directed=T)
plot(g3, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5)


name<-c("서진수 대표 이사", "일지매 부장", "김유신 과장", "손흥민 대리", "노정호 대리", "김어준 과장", 
"양향자 상무", "강감찬 부장", "광개토 과장", "정몽주 대리")
pemp<-c("서진수 대표 이사", "서진수 대표 이사", "일지매 부장", "김유신 과장", "김유신 과장", "서진수 대표 이사", 
"양향자 상무", "서진수 대표 이사", "유관순 과장", "강감찬 부장")
data4 <- data.frame(name=name, pos=pemp)
g4 <- graph.data.frame(data4, directed=T)
plot(g4, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5)
plot(g4, layout=layout.circle, vertex.size=8, edge.arrow.size=0.5)
plot(g4, layout=layout.kamada.kawai, vertex.size=8, edge.arrow.size=0.5)


#사다리 차트 만들기 

library("KoNLP")#이거 먼저 로딩하고 나서 설치하자 
install.packages("devtools")
library("devtools")

#혹시 설치가 안되면 아래 사이트에서 직접 다운받아 설치해야 한다 

#https://www.r-project.org/nosvn/pandoc/devtools.html
#RTools.exe 다운받아 설치해야 함 
#https://cran.r-project.org/bin/windows/Rtools/

#http://christophergandrud.github.io/d3Network/

library("devtools")
devtools::install_github('christophergandrud/d3Network')

install.packages("RCurl")

# Load packages
library(RCurl)
library(d3Network)

library(RCurl)
d3SimpleNetwork(data4, width=600, height=500, file="c:/RTest/test1.html")

#/RTest 폴더가 있어야 한다 

name<-c("Tom", "Jane", "Smith", "Susan", "Brown", "Lily", "John", "Stepany", "Stella", "Jony")
pemp<-c("Stepany", "Jane", "Lily", "Stepany", "John", "Lily", "John", "Stepany", "Stella", "Jony")
data4 <- data.frame(name=name, pos=pemp)
d3SimpleNetwork(data4, width=900, height=400, file="c:/RTest/test2.html", )


Source <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
Target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
NetworkData <- data.frame(Source, Target)
d3SimpleNetwork(NetworkData, width = 400, height = 250, file="c:/RTest/test3.html")


d3SimpleNetwork(NetworkData, width = 400, height = 250,
                textColour = "orange", linkColour = "red",
                nodeColour = "pink", opacity = 0.9, file="c:/RTest/test4.html")

d3SimpleNetwork(NetworkData, width = 400, height = 250,
                textColour = "#D95F0E", linkColour = "#FEC44F",
                nodeColour = "#D95F0E", opacity = 0.9,
                charge = -50, fontsize = 12, file="c:/RTest/test5.html")


# Load RCurl package for downloading the data
library(RCurl)
# Gather raw JSON formatted data
URL <- "https://raw.githubusercontent.com/christophergandrud/d3Network/master/JSONdata/miserables.json"
MisJson <- getURL(URL, ssl.verifypeer = FALSE)
 
# Convert JSON arrays into data frames
MisLinks <- JSONtoDF(jsonStr = MisJson, array = "links")
MisNodes <- JSONtoDF(jsonStr = MisJson, array = "nodes")
d3ForceNetwork(Links = MisLinks, Nodes = MisNodes,
                Source = "source", Target = "target",
                Value = "value", NodeID = "name",
                Group = "group", width = 550, height = 400,
                opacity = 0.9, file="c:/RTest/test6.html")


library("igraph")
g <- read.csv("C:/Study/R/군집분석.csv", header=T)
graph <- data.frame(학생=g$학생, 교수=g$교수)
g<-graph.data.frame(graph, directed=T)
plot(g, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.5, vertex.color="green", vertex.label=NA)
plot(g, layout=layout.kamada.kawai, vertex.size=2, edge.arrow.size=0.5, vertex.color="green", vertex.label=NA)

install.packages("stringr")
library("stringr")

#전체 이름 다 가져옴 
gubun <- V(g)$name

colors<-c()
for(i in 1:length(gubun))
{
	if(str_count(gubun[1], "S")==1)
	{
		colors<-c(colors, "red")
	}
	else
	{
		colors<-c(colors, "green")
	}
}

plot(g, layout=layout.kamada.kawai, vertex.size=2, edge.arrow.size=0.5, vertex.color=colors, vertex.label=NA)



g <- read.csv("C:/Study/R/군집분석.csv", header=T)
graph <- data.frame(학생=g$학생, 교수=g$교수)
d3SimpleNetwork(graph, width = 800, height = 500,
                textColour = "orange", linkColour = "red",
                nodeColour = "pink", opacity = 0.9, file="c:/Study/test7.html")


g <- read.csv("C:/Study/R/군집분석.csv", header=T)
graph <- data.frame(학생=g$학생, 교수=g$교수)
d3SimpleNetwork(g, width = 800, height = 500,
                textColour = "orange", linkColour = "red",
                nodeColour = "pink", opacity = 0.9, file="c:/Study/test7.html")


g <- read.csv("C:/Study/R/메르스전염현황.csv", header=T)
d3SimpleNetwork(g, width = 800, height = 500,
                textColour = "orange", linkColour = "red",
                nodeColour = "pink", opacity = 0.9, file="c:/Study/test7.html")

install.packages("treemap")
library("treemap")
total <- read.csv("c:/Study/R/학생시험결과_전체점수.csv", header=T, sep=",")
total
treemap(total, vSize='점수', index=c('점수', '이름'))
treemap(total, vSize='점수', index=c('점수','조', '이름'))
treemap(total, vSize='점수', index=c('조', '이름'))

total<-read.table("c:/Study/R/학생별전체성적_new.txt")
row.names(total)<-total$이름


> total<-read.table("c:/Study/R/학생별전체성적_new.txt", header=T, sep=",")
> total
    이름 국어 영어 수학 국사 화학 물리
1 박지영   90   85   55   88   91   79
2 김태함   70   65   80   75   76   89
3 김효섭   92   95   76   65   89   91
4 임경희   76   89   88   98  100   91
5 권혁진   97   87   83   91   86   91
6 하혜진   80   86   97   85   69   77
7 이준원   80   30   40   50   70   90
8 윤정웅   70   82   54   56   58   60
9 주시현   90   95  100   85   89   92
> total[2:7]
  국어 영어 수학 국사 화학 물리
1   90   85   55   88   91   79
2   70   65   80   75   76   89
3   92   95   76   65   89   91
4   76   89   88   98  100   91
5   97   87   83   91   86   91
6   80   86   97   85   69   77
7   80   30   40   50   70   90
8   70   82   54   56   58   60
9   90   95  100   85   89   92
> row.names(total)<-total$이름
> total[2:7]
       국어 영어 수학 국사 화학 물리
박지영   90   85   55   88   91   79
김태함   70   65   80   75   76   89
김효섭   92   95   76   65   89   91
임경희   76   89   88   98  100   91
권혁진   97   87   83   91   86   91
하혜진   80   86   97   85   69   77
이준원   80   30   40   50   70   90
윤정웅   70   82   54   56   58   60
주시현   90   95  100   85   89   92
> total
         이름 국어 영어 수학 국사 화학 물리
박지영 박지영   90   85   55   88   91   79
김태함 김태함   70   65   80   75   76   89
김효섭 김효섭   92   95   76   65   89   91
임경희 임경희   76   89   88   98  100   91
권혁진 권혁진   97   87   83   91   86   91
하혜진 하혜진   80   86   97   85   69   77
이준원 이준원   80   30   40   50   70   90
윤정웅 윤정웅   70   82   54   56   58   60
주시현 주시현   90   95  100   85   89   92
> total[2:7]
       국어 영어 수학 국사 화학 물리
박지영   90   85   55   88   91   79
김태함   70   65   80   75   76   89
김효섭   92   95   76   65   89   91
임경희   76   89   88   98  100   91
권혁진   97   87   83   91   86   91
하혜진   80   86   97   85   69   77
이준원   80   30   40   50   70   90
윤정웅   70   82   54   56   58   60
주시현   90   95  100   85   89   92
> total<-total[2:7]
> stars(total, flip.labels=F, draw.segment=F, frame.plot=T, full=T)
> 

사다리차트
install.packages("fmsb")
library("fmsb")

layout <- data.frame( 분석력=c(5,1),
           창의력=c(15,3),
           판단력=c(3,0),
           리더쉽=c(5,1),
           사교성=c(5,1))

set.seed(123) -- 임의의 데이터를 생성합니다
data1 <- data.frame(
        분석력=runif(3,1,5),
        창의력=rnorm(3,10,2),
        판단력=c(0.5,NA,3),
        리더쉽=runif(3,1,5),
        사교성=c(5,2.5,4))
data2 <- rbind(layout,data1)
op <- par(mar=c(1,0.5,3,1),mfrow=c(2,2)) # 여백과 배치를 조정합니다
radarchart(data2,axistype=1,seg=5,plty=1,title="첫번째 타입")
radarchart(data2,axistype=2,pcol=topo.colors(3),plty=1,title="두번째 타입")
radarchart(data2,axistype=3,pty=32,plty=1,axislabcol="grey",na.itp=FALSE,
  title="세번째 타입)")
radarchart(data2,axistype=0,plwd=1:5,pcol=1,title="네번째 타입")



plot(1:15)
abline(h=8)  # 선 긋기
rect(1,6,3,8)  # 사각형 그리기
arrows(1,1,5,5) # 화살표 그리기
text(8,9,"TEXT")  # 글자 쓰기
title("THIS IS TEST","SUB") # 제목 표시하기

install.packages("ggplot2")
library(ggplot2)

/*
♦ 데이터 프레임(data frame)
♦ 색상, 크기 같은 외적 요소(aes)
♦ 점, 선, 모양 같은 기하학적 요소(geoms)
♦ 통계적 처리 방법(stats)
♦ aes에서 사용할 스케일(scale)
*/

http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
korean <- read.table("학생별국어성적_new.txt",header=T,sep=",")
korean

#dataframe 객체의 데이터를 빠르게 쉽게, 그룹핑도 도와준다 
g<- ggplot(korean,aes(x=이름,y=점수)) 
g + geom_point()

g<- ggplot(korean,aes(x=이름,y=점수)) 
g + geom_point(aes(color="red"), size=5)

#무조건 도수 분포표화 한다 
“identity” (데이터 프레임의 값을 그대로 사용해서 그래프를 그리라는 뜻)
ggplot(korean,aes(x=이름,y=점수)) + geom_bar(stat="identity")

ggplot(korean,aes(x=이름,y=점수)) + geom_bar(stat="identity",fill="green",  colour="red")
gg <- ggplot(korean,aes(x=이름,y=점수)) + geom_bar(stat="identity",fill="pink",   colour="red")
gg + theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1,  
color="blue",size=8))


score_kem <- read.csv("학생별과목별성적_국영수_new.csv",header=T)
score_kem

library(plyr)
#이름과 과목순으로 정렬읋 한다 
sort_kem <- arrange(score_kem,이름,과목)
sort_kem
/*
ddply(데이터프레임, 기준컬럼, 적용함수나 결과들)
cdata <- ddply(dataNA, c("sex", "condition"), summarise,
               N    = sum(!is.na(change)),
               mean = mean(change, na.rm=TRUE),
               sd   = sd(change, na.rm=TRUE),
               se   = sd / sqrt(N)
)
cdata
#>   sex condition  N      mean        sd        se
#> 1   F   aspirin  4 -3.425000 0.9979145 0.4989572
#> 2   F   placebo 12 -2.058333 0.5247655 0.1514867
#> 3   M   aspirin  7 -5.142857 1.0674848 0.4034713
#> 4   M   placebo  3 -1.300000 0.5291503 0.3055050

transform <- 행으로 합계, cumsum누적합계 
*/

sort_kem2 <- ddply(sort_kem,"이름",transform,누적합계=cumsum(점수))
sort_kem2

ggplot(sort_kem2,aes(x=이름,y=점수,fill=과목)) + 
   geom_bar(stat="identity") +
   geom_text(aes(y=누적합계,label=paste(점수,'점')),colour="black",size=4)


sort_kem3 <- ddply(sort_kem2,"이름",transform,누적합계=cumsum(점수),  label=cumsum(점수)-0.5*점수)
sort_kem3
ggplot(sort_kem3,aes(x=이름,y=점수,fill=과목)) + 
   geom_bar(stat="identity") +
   geom_text(aes(y=label,label=paste(점수,'점')),colour="black",size=4)

gg2 <- ggplot(sort_kem3,aes(x=이름,y=점수,fill=과목)) + 
        geom_bar(stat="identity") +
        geom_text(aes(y=label,label=paste(점수,'점')),colour="black",size=4) +
        guides(fill=guide_legend(reverse=T))
 
gg2 + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1,
             colour="black", size=8))



score <- read.table("학생별전체성적_new.txt",header=T,sep=",")
score
score[,c("이름","영어")]
score

#클래브랜드 점 그래프 

ggplot(score,aes(x=영어,y=reorder(이름,영어))) + 
 geom_point(size=6) + 
 theme_bw( )  

ggplot(score,aes(x=영어,y=reorder(이름,영어))) + 
 geom_point(size=6) + 
 theme_bw( )  +
 theme(panel.grid.major.x=element_blank( ) ,
       panel.grid.minor.x=element_blank( ) ,
       panel.grid.major.y=element_line(color="red",linetype="dashed"))

#가로 세로 빨간선 
ggplot(score,aes(x=영어,y=reorder(이름,영어))) + 
 geom_point(size=6) + 
 theme_bw( )  +
 theme(panel.grid.major.x=element_line(color="red",linetype="dashed"),
       panel.grid.minor.x=element_blank( ) ,
       panel.grid.major.y=element_line(color="red",linetype="dashed"))

ggplot(score,aes(x=영어,y=reorder(이름,영어))) + 
 geom_segment(aes(yend=이름), xend=0, color="blue")+ 
 geom_point(size=6, color="green") + 
 theme_bw( )  +
 theme(panel.grid.major=element_blank( ))

install.packages("gridExtra")
library(gridExtra)
v_mt <- mtcars
v_mt
graph1 <- ggplot(mtcars,aes(x=hp , y=mpg))
graph1 + geom_point()	
savePlot("graph1.png",type="png")

graph2 <- graph1 + geom_point(color="blue") # 색상 변경하기
graph2
savePlot("graph2.png",type="png")


graph3 <- graph2 + geom_point(aes(color=factor(am))) # 종류별로 다른 색상 지정 하기
graph3
savePlot("graph3.png",type="png")

graph4 <- graph1 + geom_point(size = 7)# 크기 지정하기
graph4
savePlot("graph4.png",type="png")


graph5 <- graph1 + geom_point(aes(size = wt)) #값 별로 다른 크기 지정하기
graph5
savePlot("graph5.png",type="png")


graph6 <- graph1 + geom_point(aes(shape=factor(am),size = wt))
graph6
savePlot("graph6.png",type="png") # 종류별로 크기와 모양 지정하기

graph7 <- graph1 + geom_point(aes(shape=factor(am),color=factor(am),size = wt)) + 
    scale_color_manual(values=c("red","green"))
graph7
savePlot("graph7.png",type="png") # 종류별로 크기, 모양, 색상 지정하기


graph8 <- graph1 + geom_point(color="red") + geom_line()  # 선 추가하기
graph8
savePlot("graph8.png",type="png")


graph9 <- graph1 + geom_point(color="blue") +labs(x="마력" , y="연비(mile/gallon)")
graph9 # x 축과 y 축 이름 바꾸기
savePlot("graph9.png",type="png")

three <- read.csv("학생별과목별성적_3기_3명.csv",header=T)
three
sort_score <- arrange(three,이름,과목)
sort_score
ggplot(sort_score,aes(x=과목,y=점수,color=이름,group=이름)) + geom_line( )

ggplot(sort_score,aes(x=과목,y=점수,color=이름,group=이름)) +  geom_line( ) + geom_point( )

ggplot(sort_score,aes(x=과목,y=점수,color=이름,group=이름,fill=이름)) + geom_line( ) + geom_point(size=6,shape=22 )

data <- read.csv("실적3.csv")
data2 <- arrange(data, 이름, 월)

#산포도 
ggplot(data2,aes(x=월,y=실적,color=이름,group=이름)) + geom_point( )

ggplot(data2,aes(x=월,y=실적,color=이름,group=이름)) + geom_line( )
ggplot(data2,aes(x=월,y=실적,color=이름,group=이름)) +  geom_line( ) + geom_point( )
ggplot(data2,aes(x=월,y=실적,color=이름,group=이름, fill=이름)) + geom_line( ) 
+ geom_point(size=6,shape=22 )

ggplot(data2,aes(x=이름,y=실적, fill=월)) + geom_bar(stat="identity")
data3 <- ddply(data2,"이름",transform,누적합계=cumsum(실적),  label=cumsum(실적)-0.5*실적)

gg2 <- ggplot(data3,aes(x=이름,y=실적,fill=월))  
gg2 +    geom_bar(stat="identity")


gg2 <- ggplot(data3, aes(x=이름,y=실적,fill=월)) + 
        geom_bar(stat="identity") +
        geom_text(aes(y=label,label=paste(실적,'만원')),colour="black",size=4) +
        guides(fill=guide_legend(reverse=T))
gg2 + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1,
             colour="black", size=8))


dis <- read.csv("1군전염병발병현황_년도별.csv",stringsAsFactors=F)
dis
ggplot(dis,aes(x=년도별,y=장티푸스,group=1)) + geom_line( )
ggplot(dis,aes(x=년도별,y=장티푸스,group=1)) + geom_area( )
ggplot(dis,aes(x=년도별,y=장티푸스,group=1)) + geom_area(color="red",fill="cyan",alpha=0.4)


R에서의 피봇
install.packages("reshape2")
library("reshape2")
dcast(데이터, 행이될항목~열이될항목, 값으로사용할 항목, 결과형태)

데이터프레임 그룹연산
install.packages("dplyr")
library("dplyr")

install.packages("plotly")
library(plotly)
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)