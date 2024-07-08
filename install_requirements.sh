#!/bin/bash

# Check if requirements.txt exists
if [ ! -f requirements.txt ]; then
  echo "requirements.txt not found!"
  exit 1
fi

# Read each line in requirements.txt
while IFS= read -r library; do
  # Skip empty lines and comments
  if [[ -z "$library" || "$library" =~ ^# ]]; then
    continue
  fi

  # Install the library using luarocks
  echo "Installing $library..."
  luarocks install "$library"

  # Check if the installation was successful
  if [ $? -ne 0 ]; then
    echo "Failed to install $library"
    exit 1
  fi
done < requirements.txt

echo "All libraries installed successfully."
