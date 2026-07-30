library(usethis)
use_git_config(user.name = "Joacometrics", 
               user.email = "joaquinandressalazarp@gmail.com")

create_github_token()

library(gitcreds)
gitcreds_set()


use_git()

library(checkmate)
use_github()
