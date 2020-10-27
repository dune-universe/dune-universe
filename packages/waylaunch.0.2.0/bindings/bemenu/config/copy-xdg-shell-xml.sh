#!/bin/sh -e

file=$(pkg-config --variable=pkgdatadir wayland-protocols)/stable/xdg-shell/xdg-shell.xml

cp "$file" .
