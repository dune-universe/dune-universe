#!/bin/sh

cat example/org.recoil.dave.exampled.plist | sed "s|@BINARY@|$(pwd)/example.native|g" > $HOME/Library/LaunchAgents/org.recoil.dave.exampled.plist
launchctl load $HOME/Library/LaunchAgents/org.recoil.dave.exampled.plist
cat example/org.recoil.dave.anotherd.plist | sed "s|@BINARY@|$(pwd)/example.native|g" > $HOME/Library/LaunchAgents/org.recoil.dave.anotherd.plist
launchctl load $HOME/Library/LaunchAgents/org.recoil.dave.anotherd.plist
