#!/bin/bash

multiplier=10

cp dummy_file_encoded dummy_file_encoded.backup

for ((i = 1; i < $multiplier; i++)); do
  cat dummy_file_encoded.backup >> dummy_file_encoded
done

rm dummy_file_encoded.backup
