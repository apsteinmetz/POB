# Create number names to extract chapter
number_names <-  "1 = One	11 = Eleven	21 = Twenty-one	31 = Thirty-one	41 = Forty-one
2 = Two	12 = Twelve	22 = Twenty-two	32 = Thirty-two	42 = Forty-two
3 = Three	13 = Thirteen	23 = Twenty-three	33 = Thirty-three	43 = Forty-three
4 = Four	14 = Fourteen	24 = Twenty-four	34 = Thirty-four	44 = Forty-four
5 = Five	15 = Fifteen	25 = Twenty-five	35 = Thirty-five	45 = Forty-five
6 = Six	16 = Sixteen	26 = Twenty-six	36 = Thirty-six	46 = Forty-six
7 = Seven	17 = Seventeen	27 = Twenty-seven	37 = Thirty-seven	47 = Forty-seven
8 = Eight	18 = Eighteen	28 = Twenty-eight	38 = Thirty-eight	48 = Forty-eight
9 = Nine	19 = Nineteen	29 = Twenty-nine	39 = Thirty-nine	49 = Forty-nine
10 = Ten	20 = Twenty	30 = Thirty	40 = Forty	50 = Fifty"

str_split(number_names," (= )?")

temp <- number_names %>%
  str_replace_all("\t|\n"," ") %>%
  str_split(" (= )?") %>%
  unlist()

a <- temp[1:50 *2- 1]
b <- temp[1:50 *2]
number_names <- tibble(number=as.numeric(a),name=b) %>%
  arrange(number)

write_csv(number_names,file="../number_names1to50.csv")
