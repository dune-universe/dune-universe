#!/bin/sh
#
# creates dump.edsp.xz, Sources_*.xz and Packages_*_*.xz in the current directory

set -exu

timestamp="$1"
arch=amd64

# to reduce the size, we throw away all unnecessary fields
binfields=Package,Version,Architecture,Multi-Arch,Essential,Source,Provides,Depends,Pre-Depends,Conflicts,Breaks,Replaces
srcfields=Package,Version,Architecture,Build-Depends,Build-Conflicts,Build-Conflicts-Indep,Build-Conflicts-Arch,Build-Depends-Indep,Build-Depends-Arch

tmpdir=$(mktemp --directory)
tmpdir=
# we use stable to avoid unsatisfiable build depends
curl "http://snapshot.debian.org/archive/debian/$timestamp/dists/stable/main/binary-$arch/Packages.xz" | unxz --decompress --to-stdout > "$tmpdir/Packages_${timestamp}_$arch"
curl "http://snapshot.debian.org/archive/debian/$timestamp/dists/stable/main/source/Sources.xz" | unxz --decompress --to-stdout > "$tmpdir/Sources_${timestamp}"

grep-dctrl --exact-match -F Package dpkg Sources_${timestamp} > "$tmpdir/dpkg"
botch-create-graph --optgraph --verbose --progress --deb-drop-b-d-indep --deb-native-arch=$arch --bg "$tmpdir/Sources_${timestamp}" "$tmpdir/Packages_${timestamp}_$arch" "$tmpdir/dpkg" > "$tmpdir/repo.xml"
botch-buildgraph2packages "$tmpdir/repo.xml" "$tmpdir/Packages_${timestamp}_$arch" > "$tmpdir/Packages_${timestamp}_${arch}_min"
botch-bin2src --deb-native-arch=$arch "$tmpdir/Packages_${timestamp}_${arch}_min" "$tmpdir/Sources_${timestamp}" > "$tmpdir/Sources_${timestamp}_min"
rm "$tmpdir/repo.xml" "$tmpdir/dpkg" "$tmpdir/Packages_${timestamp}_$arch" "$tmpdir/Sources_${timestamp}"

dose-builddebcheck --explain --deb-drop-b-d-indep --failures --deb-native-arch=$arch "$tmpdir/Packages_${timestamp}_${arch}_min" "$tmpdir/Sources_${timestamp}_min"

botch-optuniv --verbose --deb-drop-b-d-indep --deb-native-arch=$arch "$tmpdir/Packages_${timestamp}_${arch}_min" "$tmpdir/Sources_${timestamp}_min" > "$tmpdir/Packages_${timestamp}_${arch}_opt"
# botch-optuniv removes all fields but those relevant to botch/dose3 but for
# apt we need the Filename field as well, we add it with botch-intersection
botch-packages-intersection "$tmpdir/Packages_${timestamp}_${arch}_min" "$tmpdir/Packages_${timestamp}_${arch}_opt" "-" \
	| grep-dctrl -s $binfields '' \
	| xz -9e --to-stdout > Packages_${timestamp}_${arch}.xz
botch-bin2src --deb-native-arch=$arch Packages_${timestamp}_${arch}.xz "$tmpdir/Sources_${timestamp}_min" \
	| grep-dctrl -s $srcfields '' \
	| xz -9e --to-stdout > Sources_${timestamp}.xz

rm "$tmpdir/Packages_${timestamp}_${arch}_min" "$tmpdir/Sources_${timestamp}_min"

dose-builddebcheck --explain --deb-drop-b-d-indep --failures --deb-native-arch=$arch Packages_${timestamp}_${arch}.xz Sources_${timestamp}.xz

dose-ceve -t deb Packages_${timestamp}_${arch}.xz -T cudf \
	| xz -9e --to-stdout > debian_${timestamp}.cudf.xz

mkdir -p "$tmpdir/var/lib/dpkg"
touch "$tmpdir/var/lib/dpkg/status"
mkdir -p "$tmpdir/etc/apt"
echo "deb [trusted=yes] file://$tmpdir/repo ./" > "$tmpdir/etc/apt/sources.list"
mkdir "$tmpdir/repo"
cp "$tmpdir/Packages_${timestamp}_${arch}_opt" "$tmpdir/repo/Packages"
env --chdir "$tmpdir/repo" apt-ftparchive release . > "$tmpdir/repo/Release"
cat << END > "$tmpdir/apt.conf"
Apt {
   Architecture "amd64";
   Architectures "amd64";
};

Dir "$tmpdir";
END
APT_CONFIG="$tmpdir/apt.conf" apt-get update
APT_CONFIG="$tmpdir/apt.conf" APT_EDSP_DUMP_FILENAME="$tmpdir/dump.edsp" apt-get install --solver=dump --simulate libmono-cil-dev || true
xz --to-stdout -9e "$tmpdir/dump.edsp" > debian_${timestamp}.edsp.xz
rm "$tmpdir/dump.edsp"
rm "$tmpdir/etc/apt/sources.list"
APT_CONFIG="$tmpdir/apt.conf" apt-get update
APT_CONFIG="$tmpdir/apt.conf" apt-get clean
rm "$tmpdir/apt.conf"
rm "$tmpdir/var/lib/dpkg/status"
rm "$tmpdir/repo/Release"
rm "$tmpdir/repo/Packages"
rm "$tmpdir/var/lib/apt/lists/lock"

rm "$tmpdir/Packages_${timestamp}_${arch}_opt"
# the rest is empty directories
find "$tmpdir" -depth -print0 | xargs -0 rmdir
