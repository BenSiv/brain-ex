#!/bin/bash
gprbuild -P brain_ex.gpr

echo "Build complete. Binary in bin/brain_ex"
echo "Suggested next step: cp bin/brain_ex /usr/local/bin/brex"

cat <<'EOF'
 ____            _             _____      
| __ ) _ __ __ _(_)_ __       | ____|_  __
|  _ \| '__/ _` | | '_ \ _____|  _| \ \/ /
| |_) | | | (_| | | | | |_____| |___ >  < 
|____/|_|  \__,_|_|_| |_|     |_____/_/\_\
EOF