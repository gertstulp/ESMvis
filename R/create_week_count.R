# Function that creates week count
# Every week should start at sunday.
create_week_count <- function(input) {
  week_cnt <- vector(mode = "integer", length(input))
  cnt=1
  week_cnt[1]=cnt # First day always week 1
  for(i in 2:length(input)) {
    if(input[i]=="Mon"&input[i-1]!="Mon") {
      cnt = cnt + 1
    } 
    week_cnt[i]=cnt
  }
  return(week_cnt)
} 