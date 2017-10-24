# Jump_start script helps to switch between projects faster
#!/bin/sh

PROJECT=fillmem

WDIR=~/Sources/$PROJECT
BIN_DIR=$WDIR/bin
SRC_DIR=$WDIR/site

REPO=gsempe/gsempe.github.io.git
SHOULD_SSH_CLONE=true
BRANCH_SITE_SOURCE=source
BRANCH_PUBLIC=master

HUGO_BIN_ARCHIVE_URL=https://github.com/gohugoio/hugo/releases/download/v0.26/hugo_0.26_macOS-64bit.tar.gz
HUGO_BIN_ARCHIVE=hugo_0.26.tar.gz

echo "* Creating working folder $WDIR..."
echo "* Creating binary folder $BIN_DIR..."
mkdir -p $BIN_DIR
cd $WDIR

if [ ! -f $BIN_DIR/hugo ]; then
	echo "* Downloading Hugo archive $HUGO_BIN_ARCHIVE_URL..."
	curl -L -s -o $BIN_DIR/$HUGO_BIN_ARCHIVE $HUGO_BIN_ARCHIVE_URL
	tar xvzf $BIN_DIR/$HUGO_BIN_ARCHIVE -C $BIN_DIR
fi

if [ ! -d "$SRC_DIR" ]; then
	echo "* Downloading repository $REPO..."
	if [ "$SHOULD_SSH_CLONE" = true ] ; then
		git clone git@github.com:$REPO $SRC_DIR
	else
		git clone https://github.com/$REPO $SRC_DIR
	fi
else
	echo "* Skipping $REPO clone, folder $SRC_DIR exists..."
fi

cd $SRC_DIR
echo "* Downloading repository $REPO updates..."
git fetch --all
echo "* Switching to website source branch $BRANCH_SITE_SOURCE..."
git checkout $BRANCH_SITE_SOURCE
git pull --ff-only
git submodule init
git submodule update

if [ ! -d "public" ]; then
	echo "* Creating public branch $BRANCH_PUBLIC worktree in public folder..."
	git worktree add -B master public origin/master
else
	echo "* Skipping $BRANCH_PUBLIC worktree creation, folder public exists..."
fi

echo "* Changing to working folder $WDIR..."
cd $WDIR
