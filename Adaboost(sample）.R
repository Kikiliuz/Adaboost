#Adaboost，求解李航案例
library('dplyr')
x<-c(0:9)
y<-c(1,1,1,-1,-1,-1,1,1,1,-1)
data<-cbind(x,y)
data<-data.frame(data)
data$w<-1/nrow(data)
#将结果的密度由大到小排序（负样本较少的情况相反）
variable_cnt<-arrange(data.frame(summarise(group_by(data,y),sum=length(x))),
		desc(data.frame(summarise(group_by(data,y),sum=length(x)))$sum))
# #将频数最大的结果值填充到pred,作为初始值
# data$pred<-rep(variable_cnt[1,1],nrow(data))
#生成所有循环结果（共n*t个，其中n为样本数，t为结果数)
#m=1时的循环，（需要进一步修改，简洁代码~》Q_Q《~)
result=function(v){
a=list()
b=list()
t=list()
p=list()
s=list()
new_w=list()
m=1
category=length(unique(data$y))
pred_1=c(rep(0,n))
pred_2=c(rep(0,n))
for (i in c(2:n)){
		pred_1[i-1]=1
		pred_1[c(i:n)]=-1
		a[[i]]=pred_1
		a[[i+1]]=rep(1,n)
	}
for (i in c(2:n)){
		pred_2[i-1]=-1
		pred_2[c(i:n)]=1
		b[[i]]=pred_2
		b[[i+1]]=rep(-1,n)
	}
	b=b[2:11]
	a=a[2:11]
	c<-data.frame(a)
	d<-data.frame(b)
	t=cbind(c,d)
for (i in c(1:(n*category))){
	s[i]=sum(data$w[data$y - data.frame(t[i])!=0])
	min_err=min(data.frame(s))
	position=min(which(s==min_err))
	alpha=0.5*log((1-min_err)/min_err)
	}
for (i in c(1:(n*category))){
	if(position<=10){
		new_w=data.frame(s[c(1:10)])*alpha
	}else{
		new_w=data.frame(s[c(11:20)])*alpha
	}
}
while(sum(t[position]-data$y)!=0){
	data$y=t[position]
	data$w=new_w
	m=m+1
}
return(m)
}
b=result(10)
