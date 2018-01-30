class(mtcars)
mtcars_tb <- as_tibble(mtcars)
class(mtcars_tb)
mtcars_tb[3]
df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

df_tb <- as_tibble(df)
df_tb$x
df_tb[,"xyz"]
df[,c("abc","xyz")]
tibble(x=letters)
tibble(x=1:3,y=list(1:5,1:10,1:20))
data.frame(x=1:3,y=list(1:5,1:10,1:20))
