edges = read.csv("edges.csv")
users = read.csv("users.csv")

str(users)

##what is the average number of friends per user? 
##Hint: this question is tricky, and it might help to start by 
##thinking about a small example with two users who are friends.	
## 292/59=4.95 as,there are 146 pairs of friends in edges,and total users
##are 59

v=c()
	for(num in users$id) {
		n1 = table(edges$V1 == num)
		n2 = table(edges$V2 == num)
		n1 = as.numeric(n1["TRUE"])
		n2 = as.numeric(n2["TRUE"])
		if(is.na(n1))
			n1=0
		if(is.na(n2))
			n2=0
		count = n1+n2
		v = c(v,count)		
	}
	mean = mean(v)
	mean

table(users$school)#school B=40

table(users$school,users$gender)##B=(11,28,1) A=(1,3,13)

install.packages("igraph")
library("igraph")


g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
degree(g)
table(degree(g)>10)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
which.max(V(g)$size)
##[1] 51
V(g)$size[51]
##[1] 11
which.min(V(g)$size)
##[1] 4

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

V(g)$color[V(g)$school == "A"] = "green"
V(g)$color[V(g)$school == "AB"] = "blue"
V(g)$color[V(g)$school == "B"] = "red"

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "green"
plot(g, vertex.label=NA)










> V(g)$size[4]

















