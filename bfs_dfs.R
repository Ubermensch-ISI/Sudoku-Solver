
graph=list("1"=c(2,3),"2"=c(1,4,5),"3"=c(1,7,6),"4"=c(2,8),"5"=2,
           "6"=3,"7"=c(3,9),"8"=c(4,10),"9"=c(7,11,12),"10"=8,"11"=9,
           "12"=9)

incidence=function(graph){
A=matrix(0,length(names(graph)),length(names(graph)))
for (i in 1:length(names(graph))) {
  a=graph[[i]]
  for (j in 1:length(names(graph))) {
    print(j)
    if(is.element(j,a)) {
      print("b")
      A[i,j]=1
      A[j,i]=1
    }  
  }
}
return(A)
}

A=incidence(graph)

inc2graph=function(A){
  graph=list()
  
  for(i in 1:dim(A)[1]){
    a=NULL
    for(j in 1:dim(A)[1]){
      if(A[i,j]==1) a=c(a,j)
    }
    graph[[i]]=a
  }
  return(graph)
}
inc2graph(A)


branch=function(graph,node=1){
  return(setdiff(graph[[node]],node))
}

bfs=function(graph,node=1){
   visi=node
   que=branch(graph,node)
   visi=c(visi,que)
   while(length(que)>0){
     d=que[1]
     que=que[-1]
     f=branch(graph,d)
     que=c(que,f[!(f %in% visi)])
     visi=c(visi,f[!(f %in% visi)])
     
   }
   return(visi)
}
bfs(graph)


dfs=function(graph,node=1){
  visi=node
  que=branch(graph,node)
  while(length(que)>0){
    visi=c(visi,que[1])
    h=branch(graph,que[1])
    h=h[!(h %in% visi)]
    que=que[-1]
    que=c(h,que)
    }
  return(visi)
}

dfs(graph,node=1)
bfs(graph,node=1)

#------------- sudoku valid
sudoku_check=function(A){
  d=0
  
  for(i in 1:9){
    a=A[i,]
    b=A[,i]
    a=a[a!=0]
    b=b[b!=0]
    if(length(a)!=length(unique(a)) || length(b)!=length(unique(b))) return("invalid")
  }
  
  ro=seq(1,9,by=3)
  k=NULL
  for(r in ro){
    for(k in ro){
    B=A[r:(r+2),k:(k+2)]
    p=c(B)
    p=p[p!=0]
    if(length(unique(p))!=length(p)) return("invalid")
  }
  }

  return("valid")
}


#------------------- Creating Sudoku
# Incomplete Sudoku with blanks as 0
incomplete_sudoku <- matrix(
  c(
    5, 3, 0, 0, 7, 0, 0, 0, 0,
    6, 0, 0, 1, 9, 5, 0, 0, 0,
    0, 9, 8, 0, 0, 0, 0, 6, 0,
    8, 0, 0, 0, 6, 0, 0, 0, 3,
    4, 0, 0, 8, 0, 3, 0, 0, 1,
    7, 0, 0, 0, 2, 0, 0, 0, 6,
    0, 6, 0, 0, 0, 0, 2, 8, 0,
    0, 0, 0, 4, 1, 9, 0, 0, 5,
    0, 0, 0, 0, 8, 0, 0, 7, 9
  ),
  nrow = 9,
  byrow = TRUE
)



sudoku_check(incomplete_sudoku)

get_blank=function(node){
  b=matrix(0,nc=2)
  A=node
  for(i in 1:9){
    for(j in 1:9){
      if(A[i,j]==0) b=rbind(c(i,j),b)
      
    }
  }
 return(b) 
}

#----------------------------- bfs sudoku
sudoku_solver_bfs=function(incomplete_sudoku=incomplete_sudoku){
node=incomplete_sudoku

blank=get_blank(node)
valid=list(node)
up_valid=list()
j=1
while(TRUE)
{
  a=blank[1,1]
  
  b=blank[1,2]
for(i in 1:9){
  node[a,b]=i
  if(sudoku_check(node)=="valid") up_valid=append(up_valid,list(node))
}
  valid=valid[-1]
  if(length(valid)==0){
   
    valid=up_valid
    just=up_valid
    up_valid=list()
    blank=blank[-1,]
    if(length(valid)==0) break
  }
 
  #print(blank[1,])
  node=valid[[1]]
 if(sum(blank!=0)==0) break
  j=j+1
  print(j)
}
return(node)
}

sudoku_solver_bfs(incomplete_sudoku)


#---------------------------------dfs algorithm
sudoku_solver_dfs=function(incomplete_sudoku){
node=incomplete_sudoku
blank=get_blank(node)
d=dim(blank)[1]
visi=1:9
j=1
l=1
while(TRUE){
  a=blank[j,1]
  b=blank[j,2]
  node[a,b]=node[a,b]+1
  if(node[a,b]>9) {
    j=j-1
    node[a,b]=0
    next
  }
  if(sudoku_check(node)=="valid") j=j+1
 
  if(j>=d) break
  if(j==0) {
    return("no solution")
    break
  }
  print(l)
  l=l+1
}
return(node)
}


star=Sys.time()
sudoku_solver_dfs(incomplete_sudoku)
end=Sys.time()
end-star


star=Sys.time()
sudoku_solver_bfs(incomplete_sudoku)
end=Sys.time()
end-star
#----------bfs is faster

inc=matrix(c(
  5, 3, 0, 6, 7, 8, 0, 1, 2,
  6, 0, 2, 1, 9, 5, 3, 0, 8,
  1, 9, 8, 0, 4, 2, 5, 6, 0,
  0, 5, 9, 7, 6, 1, 4, 2, 0,
  4, 2, 6, 0, 5, 3, 0, 9, 1,
  7, 1, 3, 9, 2, 4, 0, 5, 6,
  9, 6, 1, 0, 3, 7, 2, 8, 4,
  0, 8, 7, 4, 1, 0, 6, 3, 5,
  3, 0, 5, 2, 8, 6, 1, 7, 9
), nrow = 9, byrow = TRUE)



a1=sudoku_solver_bfs(inc)
a2=sudoku_solver_dfs(inc)
sum(a1==a2)
