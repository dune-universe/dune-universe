open Cowabloga
open Cow.Html

let read_entry ent =
  match Mirage_entries.read ent with
  | None   -> Lwt.return (string "???")
  | Some b -> Lwt.return (Cow.Markdown.of_string b)

let config = {
  Atom_feed.base_uri="http://localhost:8081";
  id = "";
  title = "The Mirage Blog";
  subtitle = Some "on building functional operating systems";
  rights = Mirage_people.rights;
  author = None;
  read_entry
}

let posts =
  Lwt_main.run begin
    Blog.to_html ?sep:None ~feed:config ~entries:Mirage_blog.entries
  end

let nav_links = [
    "Blog", Uri.of_string "/blog";
    "Docs", Uri.of_string "/docs";
    "API", Uri.of_string "/api";
    "Community", Uri.of_string "/community";
    "About", Uri.of_string "/about";
  ]

let top_nav =
  Foundation.top_nav
    ~title:(string "Mirage OS")
    ~title_uri:(Uri.of_string "/")
    ~nav_links:(Foundation.Link.top_nav ~align:`Left nav_links)

let t =
  let recent_posts = Blog.recent_posts config Mirage_blog.entries in
  let sidebar = Foundation.Sidebar.t ~title:"Recent Posts" ~content:recent_posts in
  let copyright = string "Anil Madhavapeddy" in
  let { Atom_feed.title; subtitle; _ } = config in
  Foundation.Blog.t ~title ~subtitle ~sidebar ~posts ~copyright ()

let index =
  let content = Foundation.Index.t ~top_nav in
  let body =
    Foundation.body ~title:"Mirage OS" ~headers:[] ~content ~trailers:[] ()
  in
  Foundation.page ~body

let blog =
  let headers = empty in
  let content = top_nav @ t in
  let body =
    Foundation.body ~title:"Mirage Musings" ~headers ~content ~trailers:[] ()
  in
  Foundation.page ~body
