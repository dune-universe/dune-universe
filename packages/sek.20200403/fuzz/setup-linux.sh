#!/bin/bash

# The following commands are required for afl to run properly under Linux.

echo "Disabling the crash reporter..."
echo core >/proc/sys/kernel/core_pattern
echo "Changing the CPU settings to maximize performance..."
(cd /sys/devices/system/cpu && (echo performance | tee cpu*/cpufreq/scaling_governor))
