
Pages of a Story
page/create/{story} - create a new page of a story form         GET, POST
  successful POST redirects to page/edit/{story}/{page}
page/view/{story}/{page} - read the page of a story             GET
page/edit/{story}/{page} - editing the page of a story form     GET, PUT
  successful PUT redirects to page/view/{story}/{page}
page/delete/{story}/{page} - delete the page of a story         GET, DELETE
pages/{story} - lists all pages                                 GET
when {page} = start - Special homepage of a story

out of scope

Story
story/create/
story/edit/{story}

users / accounts