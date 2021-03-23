install.packages('linkprediction')
library(linkprediction)

## 라이브러리 기초 사용법
if (requireNamespace("igraph")) {
    g <- igraph::make_graph(~A - -C:D:E - -B - -F - -G:H - -I)
    # Adamic-Adar
    proxfun(g, method = "aa", value = "edgelist")
    # Random Walk with Restart
    proxfun(g, method = "rwr", value = "edgelist")
}


install.packages('gcookbook') ## 샘플 데이터
library(gcookbook)
library(igraph)

g = graph.data.frame(madmen2, directed = TRUE) ## 데이터 세트로부터 방향이 있는 그래프 객체 생성
par(mar = c(0, 0, 0, 0)) ## 불필요한 여백을 제거
plot(g, layout = layout.fruchterman.reingold, vertex.size = 8, edge.arrow.size = 0.5, ## fruchterman 알고리듬 사용
     vertex.label = NA)


g = graph.data.frame(madmen, directed = FALSE) ## 데이터 세트로부터 방향이 없는 그래프 객체 생성
par(mar = c(0, 0, 0, 0)) ## 불필요한 여백을 제거
plot(g, layout = layout.circle, vertex.size = 8, vertex.label = NA)



## 네트워크 그래프에 텍스트 라벨넣기
## vertex.label에 이름으로 이루어진 벡터를 전달한다

m = madmen[1:nrow(madmen) %% 2 == 1,] ## 한 행 걸러 선택
m = madmen2[1:nrow(madmen2) %% 2 == 1,] ## 한 행 걸러 선택
g = graph.data.frame(m, directed = FALSE)
V(g)$name ## 각 노트의 이름 출력-그래프 데이터에 대해 반응
plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 4, ## 노드 크기를 작게
     vertex.label = V(g)$name, ## 라벨을 설정
     vertex.label.cex = 0.8, ## 폰트 크기를 약간 작게
     vertex.label.dist = 0.4, ## 라벨 위치 옮김
     vertex.label.color = "black") ## 라벨 색을 검정

## plot개체를 수정해서도 동일한 결과를 얻을 수 있다.
V(g)$size = 4
V(g)$label = V(g)$name
V(g)$label.cex = 0.8
V(g)$label.dist = 0.4
V(g)$label.color = "black"
## 그래프 전체의 속성을 설정
g$layout = layout.fruchterman.reingold
plot(g)


## link prediction
plot(g)
dg = decompose.graph(g)[[1]] # 그래프 분해


# cn
linkPredCn <- function(dg) {
    g.cn = proxfun(dg, method = "cn", value = "graph")
    g.cn = simplify(g.cn)

    exis = paste(as_edgelist(g.cn)[, 1], as_edgelist(g.cn)[, 2])
    new = paste(as_edgelist(g.cn - dg)[, 1], as_edgelist(g.cn - dg)[, 2])

    if (length(new) > 0) {
        colorIdx = which(exis == new) # 새로운 edge
        vertexIdx = as_edgelist(g.cn)[colorIdx,] # 새로운 edge의 head와 tail

        matIdx = list()
        for (j in 1:length(colorIdx)) {
            matIdx[[j]] = match(vertexIdx[j,], V(g.cn)$name)
        }
        dg2 = add_edges(dg, unlist(matIdx)) # 새로운 edge 추가

        E(dg2)$color = "grey70"
        E(dg2)[length(E(dg)):length(E(dg2))]$color = "red"
        E(dg2)[length(E(dg)):length(E(dg2))]$label = "new"
        plot(dg2)
    } else {
        plot(dg)
    }
}

clusters(g)$no
clusters(g)$csize
dg = decompose.graph(g)[[1]] # 그래프 분해

linkPredCn(dg)






# aa
g.aa <- proxfun(dg, method = "aa", value = "graph")
g.aa <- simplify(g.aa)
plot(g.aa)

E(g.aa)$weight



# rwr
linkPredRwr <- function(dg, num) {
    dg = simplify(dg)
    g.rwr = proxfun(dg, method = "rwr", value = "graph")
    g.rwr = simplify(g.rwr)

    threshold = sort(E(g.rwr)$weight)[length(E(g.rwr)) * as.numeric(num)]
    #avg = mean(E(g.rwr)$weight)
    g.rwr2 = delete_edges(g.rwr, which(E(g.rwr)$weight < threshold))
    plot(g.rwr2)
    plot(simplify(dg))

    exis = paste(as_edgelist(g.rwr2)[, 1], as_edgelist(g.rwr2)[, 2])
    new = paste(as_edgelist(g.rwr2 - dg)[, 1], as_edgelist(g.rwr2 - dg)[, 2])

    if (length(new) > 0) {
        colorIdx = list()
        for (j in 1:length(new)) {
            colorIdx[[j]] = which(exis == new[j])
        }

        E(g.rwr2)$color = "grey70"
        E(g.rwr2)[unlist(colorIdx)]$color = "red"
        E(g.rwr2)[unlist(colorIdx)]$label = "new"

        E(g.rwr2)$label
        plot(g.rwr2)
    } else {
        plot(dg)
    }
}

clusters(g)$no
clusters(g)$csize
dg = decompose.graph(g)[[1]] # 그래프 분해

linkPredRwr(dg, 0.80)
