Git commands

setting up ssh keys

ssh-keygen -t rsa -b 4096 -C "mlee09206@gmail.com"

git clone
https://github.com/minju-lee92/WSU_STATS419_FALL2020.git
-------------------------------------------------------------
# make directory (Folder)
mkdir 

# create enpty file
touch 

## make changes tot he folder ... 
git add .
git status
git commit  -m "Adding personality-raw"
git push -u origin master

## set remote url to know where we are pushing it to

git remote set-url origin
git@github.com:minju-lee92/WSU_STATS419_FALL2020.git

## Useful webpage when workin with gitbash
https://www.geeksforgeeks.org/working-on-git-bash/
  