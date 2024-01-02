if(!require(stringi)) install.packages("stringr")
if(!require(stringr)) install.packages("stringi")
if(!require(writexl)) install.packages("writexl")
if(!require(readxl)) install.packages("readxl")
if(!require(tidytext)) install.packages("tidytext")
if(!require(dplyr)) install.packages("dplyr")
if(!require(wordcloud2)) install.packages("wordcloud2")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(stopwords)) install.packages("stopwords")
if(!require(RedditExtractoR)) install.packages("RedditExtractorR")
if(!require(tuber)) install.packages("tuber")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(glue)) install.packages("glue")
if(!require(tidyverse)) install.packages("tidyverse")
# Veri içerisinde kereksiz işaretler ve 
# kelimereri kaldırmak için kendi oluşturduğumuz paket
if(!require(StringRegex)) install.packages("StringRegex")

library(tidyverse)
library(glue)
library(StringRegex)#kendi oluşturduğumuz paket.
library(openxlsx)#Excel dosyalarını okuma, yazma ve değiştirmek için kullanılır.
library(stringr)#stringler üzerinde daha rahat çalişmak içindir.
library(stringi)#geniş stringler üzerinde değişim veya düzenleme işlemi yapar.
library(tm)#metin madenciliği ve analizi için kullanılır.
library(writexl)#Excel dosyalarını doğrudan yazmaya yönelik işlevler sağlar.
library(readxl)#Excel dosyalarını kolayca okumanıza olanak tanıyar.
library(tidytext)#Düzenli veri ilkelerini kullanarak metin madenciliği ve analiz için kullanılır.
library(dplyr)#Veri çerçeveleri üzerinde çalışan bir dizi işlev sağlayarak karmaşık veri işlemlerini ifade etmeyi kolaylaştırır.
library(wordcloud2)#kelime Bulutu oluşturmak için kullanılır.
library(ggplot2)#Animasyonlu ve etkileşimli veri görselleştirmeleri oluşturmak için kullanılır.
library(RColorBrewer)#Grafiklerde ve görselleştirmelerde renkli hale getirmek için kullanılır.
library(NLP)#bilgisayarlar ve insan dilleri arasındaki etkileşim kurmak için kullanılır.
library(stopwords)#Gereksiz kelimeleri kaldırmak için kullanılır.
library(syuzhet)#Çeşitli duygu analizi sözlüklerini kullanarak metinden duygu çıkarmak için kullanılır.

#gereksiz kelimelerin listesi
BreakWords <- read_excel("Data-mining/breakwords.xlsx")

#redditden verileri çekmek için kullandığmiz paket
library(RedditExtractoR)

#STEP 1: toplam çekilen yorum sayısı 638
hot_kripto <- find_thread_urls(subreddit = "kriptopara",sort_by ="hot",period = "day")
reddit_text<- hot_kripto$title

#STEP 2: yorumuların içerisinde olan kereksiz işaretleri temizleyelim
reddit_text <-replace_all(reddit_text)

#İşaretlerden temizlenmiş yorumları tabloya aktaralım
Rdata <-data.frame(word=reddit_text)

#sonra excel tablosuna aktaralım
write_xlsx(Rdata,"Data-mining/REDDIT/Redditlexcion.csv")

#karakter kümesine dönüştürelim
corpus_reddit <- iconv(reddit_text,"UTF-8","UTF-8")

#Corpus işlevini kullanarak bir derlem oluşturuyoruz
corpus_reddit <- Corpus(VectorSource(corpus_reddit))

#terim belge matrisi oluşturalım
tdm <- TermDocumentMatrix(corpus_reddit) 

#Terim-belge matrisini matrise dönüştürelim
tdm <- as.matrix(tdm) 
# sonra tokenleri oluşturalım
words <- sort(rowSums(tdm),decreasing=TRUE) 

create_tokens_R <-data.frame(text=names(words),n=words)

#STEP 3: Tokenin içerisindeki gereksiz kelimeleri silelim
clean_tokensR <-removeVoca(create_tokens_R,BreakWords)
# son olarak reddit tokenlerimiz hazirdir.
reddit_tokens <-data.frame(text=clean_tokensR$word,ferq=clean_tokensR$n)

#STEP 6: kelme bulutu oluşturalım.
wordcloud2(
  reddit_tokens,
  color = "random-dark",
  size = 1,
  shape = "hexagon"
)

#STEP 7: tokens taplosunun içerisinde olan en çok ferekanslı kelimeleri
# alıp görselleştireceğiz.
get_plot <- reddit_tokens %>%
  filter(ferq >20) %>% #20 den büyük ferqansları alalım
  ggplot(aes(text,ferq,width=0.8))+geom_col() +
  xlab(NULL)+ylab(NULL) + coord_flip() +
  theme_minimal() + labs(title = "Reddit üzerinde en çok kullanılan kelimelerin görseli")
get_plot

#step 8 barplot
#20 den buyuk olan ferekanslari ketir
reddit_n <- subset(reddit_tokens$ferq,reddit_tokens$ferq>=20)

#ferekansın index ile kelme tokenlerin indexsi eşit olacak.
if(length(reddit_n)!=0){
  #index 0'dan başladığı için uzunlaukdan hep 1 çikar.
  reddit_text <-reddit_tokens$text[0:length(reddit_n-1)]
  barplot(reddit_n,main = "Reddit üzerinde olan kelimelrin ferekansı ",las=3,names.arg = reddit_text,col = rainbow(40))
}


# ---------------------- |
# STEP 8: duygu analizi |
#----------------------|

#reddit yorumlarini alalım
R_lexcion <-read_xlsx("Data-mining/REDDIT/Redditlexcion.csv")

# yorumlari karakter haline cevirelim.
commentsR <- iconv(R_lexcion$word,"UTF-8","UTF-8")

#Yorum Sözlüğüne dayalı duygu puanlarını çıkarmak.
s <-get_nrc_sentiment(commentsR)

# Yorumların Duygu puanların görselleştirelim
barplot(colSums(s),las=2,col = rainbow(15),
        ylab = "sayi",
        main = "reddit yorumlarındaki duygu puanıları"
        )


#_________
#YOUTUBE |
#_______|

#YOUTUBE: yorumları almak için kullandığımız paket.
library(tuber)

#youtube api alamak.
client_id<-"291335792560-sl0fuetu93njaeivd12ocdbv92avrb33.apps.googleusercontent.com"
client_secret <- "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"
yt_api <- yt_oauth(app_id=client_id,app_secret = client_secret,token = "")

#toplam yorum sayısı--> 968

search_comments1 <- get_all_comments("dU3uJUcvdbo")
search_comments2 <-get_all_comments("rIfL2nT93dQ")
search_comments3 <- get_all_comments("yGPmvZvGTWw")
search_comments4 <-get_all_comments("sHfzKM8QVqY")
search_comments5 <- get_all_comments("U1Q37HCYkxk")
search_comments6 <- get_all_comments("wA6jW1NmTdM")

#yorumları liste haline getirelim.
yt_text <- list(
  
  layer1=search_comments1$textDisplay,
  layer2=search_comments2$textDisplay,
  layer3=search_comments3$textDisplay,
  layer4=search_comments4$textDisplay,
  layer5=search_comments5$textDisplay,
  layer6=search_comments6$textDisplay
)

#yorumlari birleştirerek tabloda sutun haline getirelim.
combinde_df <-cbind(
  text1=yt_text$layer1,
  text2=yt_text$layer2,
  text3=yt_text$layer3,
  text4=yt_text$layer4,
  text5=yt_text$layer5,
  text5=yt_text$layer6
  )
#birleşmiş yorumları tabloda satır haline getirelim
yt_comment_all <- data.frame(text=sprintf("%s",combinde_df))

#yorumuların çerisinde olan kereksiz işaretleri temizleyelim.
yt_comment_all$text <-replace_all(yt_comment_all$text)
Ydata <-data.frame(text=yt_comment_all$text)

#veriyleri excele aktaralım.
write_xlsx(Ydata,"Data-mining/YOUTUBE/YoutubeLexcoin.csv")

#Corpus işlevini kullanarak bir derlem oluşturuyoruz
yt_docs <- Corpus(VectorSource(yt_comment_all$text))

#Terim-belge matrisine dönüştürelim
dtm_yt <- TermDocumentMatrix(yt_docs)

#Terim-belge matrisini matrise dönüştürelim
matrix_yt <- as.matrix(dtm_yt)

# sonra tokenleri oluşturalım
words_yt <- sort(rowSums(matrix_yt),decreasing=TRUE)

create_tokens_Yt <- data.frame(text=names(words_yt),n=words_yt)

#Tokenin içerisindeki gereksiz kelimeleri silelim
clean_tokensY <-removeVoca(create_tokens_Yt,BreakWords)
  
# duzenlenms tokeni alalim
youtube_tokens <-data.frame(text=clean_tokensY$word,ferq=clean_tokensY$n)

#kelime bulutu.
wordcloud2(data = youtube_tokens,size = 1,color = "random-dark",shape = "hexagon")

#Tokenler taplosunun içerisinde olan en çok ferekanslı kelimeleri alıp görselleştireceğiz.
get_freq_yt <- youtube_tokens %>%
  filter(ferq >30) %>% #20 den büyük ferqansları alalım
  ggplot(aes(text,ferq,width=0.7))+geom_col() +
  xlab(NULL)+ylab(NULL) + coord_flip() +
  theme_minimal() + labs(title = "Youtube üzerinden en çok kullanılan kelimelerin görseli")
get_freq_yt

# 35 den buyuk olan ferekanslari ketir
youtube_n <- subset(youtube_tokens$ferq,youtube_tokens$ferq>=35)

if(length(youtube_n)!=0){
  
  #index 0'dan başladığı için uzunlaukdan hep 1 çikar.
  youtbe_text <-youtube_tokens$text[0:length(youtube_n-1)]
  barplot(height = youtube_n,main = "Youtube üzerinde olan kelimelrin ferekansı ",names.arg = youtbe_text,las=2,col = rainbow(50))
  
}


#--------------
# duygu analizi
#---------------

#yorumlarini alalım
yt_lexcion <-read_xlsx("Data-mining/YOUTUBE/YoutubeLexcoin.csv")

# yorumlari karakter haline cevirelim.
commentsYt <-iconv(yt_lexcion$text,"UTF-8","UTF-8")

#Yorum Sözlüğüne dayalı duygu puanlarını çıkarmak.
nrc_y <-get_nrc_sentiment(commentsYt)


# Yorumların Duygu puanların görselleştirelim
barplot(colSums(nrc_y),las=3,col = rainbow(40),
        ylab = "sayi",
        main = "youtube uzerinde duygu analizi"
)


#--------------------- |
# YOUTUBE VE REDDIT   |
#------------------- |


#son olarak yotube ve reddit üzerinden yorumları birleştirelim

# verileri oluşturduğmuz dosyanın uzerine yazacağız
file1 <- file2 <- file3 <-loadWorkbook("Data-mining/RedditAndYoutube.xlsx")

writeData(file2,sheet = "Sheet 1",x="text",startRow = 1,startCol = 1)
writeData(file1,sheet = "Sheet 1",x=R_lexcion$word,startRow = 2,startCol = 1)
writeData(file2,sheet = "Sheet 1",x=yt_lexcion$text,startRow = 639,startCol = 1)

saveWorkbook(file1,"Data-mining/RedditAndYoutube.xlsx",overwrite = T)
saveWorkbook(file2,"Data-mining/RedditAndYoutube.xlsx",overwrite = T)
saveWorkbook(file3,"Ddata-mining/RedditAndYoutube.xlsx",overwrite = T)

# en sonda duygu analiz 
redditAndyoutube <-read_xlsx("Data-mining/RedditAndYoutube.xlsx")

# tokenleri alalım
tokens <- data.frame(text=redditAndyoutube$text)

#yorumların negativ,positiv olduğunu belirliyelim.
sentimet <- tokens %>%
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("bing"),by="word") %>% 
  count(sentiment) %>% 
  spread(sentiment,n,fill = 0) %>%
  mutate(sentiment=positive+negative)#negativ,positiv yorumlarin toplamı

sentimet

#verileri grafike aktaralım
barplot(colSums(sentiment),las=1,col = rainbow(40),
        ylab = "sayi",
        main = "Duygu analizi"
)
        
        
