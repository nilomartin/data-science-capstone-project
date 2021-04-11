#Function to find and return the higher prob next word, based on input.
#
db1<-readRDS("db1.rds")
db2<-readRDS("db2.rds")
db3<-readRDS("db3.rds")
db4<-readRDS("db4.rds")


pred.return<-function(string){
        require(stringr)
        require(dplyr)
        n<-str_count(string, '\\w+')
        if(n>3){
                string4<-word(string,n-2,n) 
                count.string4<-db4%>%filter(grepl(paste0("\\b^", string4, "\\b"), feature))%>%pull(2)%>%sum()
                if(count.string4>0){
                        filtered<-db4%>%filter(grepl(paste0("\\b^", string4, "\\b"), feature))%>%
                                mutate(score=(frequency/(count.string4)))
                        out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(4,4)
                }else{
                        string3<-word(string,n-1,n)
                        count.string3<-db3%>%filter(grepl(paste0("\\b^", string3, "\\b"), feature))%>%pull(2)%>%sum()
                        
                        if(count.string3>0){
                                filtered<-db3%>%filter(grepl(paste0("\\b^", string3, "\\b"), feature))%>%
                                        mutate(score=0.4*(frequency/(count.string3)))
                                out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(3,3)
                        }else{
                                string2<-word(string,n,n)
                                count.string2<-db2%>%filter(grepl(paste0("\\b^", string2, "\\b"), feature))%>%pull(2)%>%sum()
                                if(count.string2>0){
                                        filtered<-db2%>%filter(grepl(paste0("\\b^", string2, "\\b"), feature))%>%
                                                mutate(score=0.4*0.4*(frequency/(count.string2)))
                                        out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(2,2)
                                }else{
                                        string1<-word(string,n,n)
                                        count.string1<-db1%>%filter(grepl(paste0("\\b^", string1, "\\b"), feature))%>%pull(2)%>%sum()
                                        if(count.string1>0){
                                                filtered<-db1%>%filter(grepl(paste0("\\b^", string1, "\\b"), feature))%>%
                                                        mutate(score=0.4*0.4*0.4*(frequency/(count.string1)))
                                                out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(1,1)
                                        }else{
                                                out<-db1%>%filter(frequency==max(frequency))%>%select(feature)%>%pull(1)%>%word(1,1)
                                        }
                                }
                        }
                        
                }
                
        }else if(n>1){
                string3<-word(string,n-1,n)
                count.string3<-db3%>%filter(grepl(paste0("\\b^", string3, "\\b"), feature))%>%pull(2)%>%sum()
                if(count.string3>0){
                        filtered<-db3%>%filter(grepl(paste0("\\b^", string3, "\\b"), feature))%>%
                                mutate(score=0.4*(frequency/(count.string3)))
                        out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(3,3)
                }else{
                        string2<-word(string,n,n)
                        count.string2<-db2%>%filter(grepl(paste0("\\b^", string2, "\\b"), feature))%>%pull(2)%>%sum()
                        if(count.string2>0){
                                filtered<-db2%>%filter(grepl(paste0("\\b^", string2, "\\b"), feature))%>%
                                        mutate(score=0.4*0.4*(frequency/(count.string2)))
                                out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(2,2)
                        }else{
                                string1<-word(string,n,n)
                                count.string1<-db1%>%filter(grepl(paste0("\\b^", string1, "\\b"), feature))%>%pull(2)%>%sum()
                                if(count.string1>0){
                                        filtered<-db1%>%filter(grepl(paste0("\\b^", string1, "\\b"), feature))%>%
                                                mutate(score=0.4*0.4*0.4*(frequency/(count.string1)))
                                        out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(1,1)
                                }else{
                                        out<-db1%>%filter(frequency==max(frequency))%>%select(feature)%>%pull(1)%>%word(1,1)
                                }
                        }
                }
                
                
        }else{
                
                string2<-word(string,n,n)
                count.string2<-db2%>%filter(grepl(paste0("\\b^", string2, "\\b"), feature))%>%pull(2)%>%sum()
                if(count.string2>0){
                        filtered<-db2%>%filter(grepl(paste0("\\b^", string2, "\\b"), feature))%>%
                                mutate(score=0.4*0.4*(frequency/(count.string2)))
                        out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(2,2)
                }else{
                        string1<-word(string,n,n)
                        count.string1<-db1%>%filter(grepl(paste0("\\b^", string1, "\\b"), feature))%>%pull(2)%>%sum()
                        if(count.string1>0){
                                filtered<-db1%>%filter(grepl(paste0("\\b^", string1, "\\b"), feature))%>%
                                        mutate(score=0.4*0.4*0.4*(frequency/(count.string1)))
                                out<-filtered%>%filter(score==max(score))%>%select(feature)%>%pull(1)%>%word(1,1)
                        }else{
                                out<-db1%>%filter(frequency==max(frequency))%>%select(feature)%>%pull(1)%>%word(1,1)
                        }
                }     
                
                
        }
        
        return(out)
}

string<-"do you think it was a great"

pred.return(string)



 



