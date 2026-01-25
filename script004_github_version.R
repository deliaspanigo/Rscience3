# 0 - TOdos lso archivos
git add .

# 1. Perform the final commit for the release version.
git commit -m "Data import better r."

# 2. Declare the annotated tag (using -a) to label the previous commit.
# (Make sure to change 'v1.0.0' and the message).
git tag -a 0.1.11 -m "Stable version 0.1.11 of the package."

# 3. Push the commits from the current branch (e.g., 'main' or 'master') to the remote repository.
git push

# 4. Push the tags to the remote repository. This is the crucial step for the versioning!
git push --tags
