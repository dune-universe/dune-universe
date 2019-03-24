base=$(date +%Y%m%d)
suffix=""
nr=0

while ! git tag -a -m "release $base$suffix" "$base$suffix" ; do
  nr=$((nr+1))
  suffix="-$nr"
done

version="$base$suffix"
name="esgg-$version"

echo ">>> created tag $version - check and push if ok"

git archive --prefix="$name/" "$version" | gzip > "$name.tar.gz"

echo ">>> created $name.tar.gz - check and upload if ok"

if [ -z "$OPAM_REPOSITORY" ] ; then
  echo no OPAM_REPOSITORY set, skipping creation of opam package
else
  path=$OPAM_REPOSITORY/packages/esgg/esgg.$version
  mkdir -p "$path"
  cp esgg.opam "$path/opam"
  md5=$(md5sum "$name.tar.gz" | cut -d ' ' -f 1)
  printf "url {\n  src: \"https://github.com/ahrefs/esgg/releases/download/%s/%s.tar.gz\"\n  checksum: \"md5=%s\"\n}\n" "$version" "$name" "$md5" >> "$path/opam"
  git -C "$path" add opam
  git -C "$path" commit -m "+ esgg $version" opam
  echo ">>> created opam package at $path - check and push if ok"
fi

if [ -z "$AHREFS_DEV_ROOT" ] ; then
  echo no AHREFS_DEV_ROOT set, skipping creation of opam package
else
  path=$AHREFS_DEV_ROOT/opam/packages/esgg.$version
  mkdir "$path"
  cp esgg.opam "$path/opam"
  printf "url {\n  src: \"git+https://github.com/ahrefs/esgg.git#%s\"\n}\n" "$version" >> "$path/opam"
  git -C "$path" add opam
  git -C "$path" commit -m "esgg: publish $version" opam
  echo ">>> created opam package at $path - check and push if ok"
fi
