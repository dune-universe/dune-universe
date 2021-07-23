open Camyll.Url

let () =
  assert (relativize ~src:"/posts/first-post/" ~dest:"/posts/" = "../");
  assert (relativize ~src:"/posts/" ~dest:"/posts/first-post/" = "first-post/");
  assert
    (relativize ~src:"/posts/first-post/" ~dest:"/posts/second-post/"
     = "../second-post/");
  assert
    (relativize ~src:"/posts/first-post/" ~dest:"posts/second-post/"
     = "posts/second-post/");
  assert (relativize ~src:"/posts/first-post/" ~dest:"" = "");
  assert (relativize ~src:"/" ~dest:"/docs/index.html" = "docs/index.html");
  assert
    (relativize ~src:"/docs/index.html" ~dest:"/docs/configuration.html"
     = "configuration.html");
  assert
    (relativize ~src:"/docs/index.html" ~dest:"/site.css"
     = "../site.css");
  assert
    (relativize ~src:"/posts/second-post.html" ~dest:"/posts/" = "./");
  assert
    (relativize ~src:"/posts/third-post.html" ~dest:"/posts/third-post.html"
     = "third-post.html")
