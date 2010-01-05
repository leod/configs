find . -not -regex '.*/\.git/*.*' -type f -not -name README -exec diff -Naur '{}' ../'{}' \;
