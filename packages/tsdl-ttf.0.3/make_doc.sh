#!/bin/bash -ve

dune build @doc
rsync -avz --delete _build/default/_doc/_html/tsdl-ttf/Tsdl_ttf/ docs
for file in "docs/index.html" "docs/Ttf/index.html"
do
  sed -i "s|../../||g" $file
  sed -i "s|<span>&#45;&gt;</span>|<span class=\"arrow\">→</span>|g" $file
done

sed -i "s| (tsdl-ttf.Tsdl_ttf)||g" docs/index.html
cp ./_build/default/_doc/_html/odoc.css docs/
cp _build/default/_doc/_html/highlight.pack.js docs/
chmod 644 docs/odoc.css
echo "header nav {display: none;} header nav.toc {display: block;} header dl dd, header dl dt {display: inline-block;} " >>  docs/odoc.css

echo "Done"
