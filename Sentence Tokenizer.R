text<-"This is first sentence . This is second one"
text1<-"India, officially the Republic of India (Bharat Ga???arajya),[e] is a country in South Asia. It is the seventh-largest country by area, the second-most populous country (with over 1.2 billion people), and the most populous democracy in the world"

text_split<- str_split(text1,"((?<=[a-z0-9][.?!])|(?<=[a-z0-9][.?!]\"))(\\s|\r\n)(?=\"?[A-Z])")[[1]]
text_split

mg<-"Born and raised in a Hindu merchant caste family in coastal Gujarat, western India, and trained in law at the Inner Temple, London, Gandhi first employed nonviolent civil disobedience as an expatriate lawyer in South Africa, in the resident Indian community's struggle for civil rights. After his return to India in 1915, he set about organising peasants, farmers, and urban labourers to protest against excessive land-tax and discrimination. Assuming leadership of the Indian National Congress in 1921, Gandhi led nationwide campaigns for various social causes and for achieving Swaraj or self-rule."

text_split<- str_split(mg,"((?<=[a-z0-9][.?!])|(?<=[a-z0-9][.?!]\"))(\\s|\r\n)(?=\"?[A-Z])")[[1]]
text_split

ex1<-"My friend holds a Msc. in Computer Science."
ex1<- str_split(ex1,"((?<=[a-z0-9][.?!])|(?<=[a-z0-9][.?!]\"))(\\s|\r\n)(?=\"?[A-Z])")[[1]]
ex1[[2]]


ex2<- "The temperature was 32.8 degrees Celsius. His B.Sc. degree was deemed insufficient. He owed the bank USD 4000.50 which he had not paid back. On 27.07.2004 a major earthquake occurred. It was 17.05 by the clock."
ex2_op <- str_split(ex2,"((?<=[a-z0-9][.?!])|(?<=[a-z0-9][.?!]\"))(\\s|\r\n)(?=\"?[A-Z])")[[1]]
ex2_op1 <- str_split(ex2,"[.\\s]")[[1]]
x <- str_split(ex2,"((?<=[a-z0-9][.?!]))"); 
ex2_op
ex2_op1
