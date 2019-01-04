#!/bin/sh

rm $HOME/Library/LaunchAgents/org.recoil.dave.exampled.plist
launchctl remove org.recoil.dave.exampled
rm $HOME/Library/LaunchAgents/org.recoil.dave.anotherd.plist
launchctl remove org.recoil.dave.anotherd
