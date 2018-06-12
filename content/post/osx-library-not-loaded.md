---
description: "Use Brew and how to fix library not loaded error on OSX"
keywords:
  - brew switch
  - osx
  - library
categories:
  - Quick Tips
tags:
  - devops

title: "Fix \"library not loaded\" error with Brew"
slug: "library not loaded osx Brew"
date: 2018-06-12T23:47:27+01:00
draft: false
---

Ruby, PHP and a lot other languages rely on system libraries to provide some features in their own ecosystems. It times to times leads to this kind of error

```batch
Library not loaded: /usr/local/lib/libjpeg.8.dylib
```

A large majority of developers on OSX use [Brew package manager](https://brew.sh/) to install the libraries they need. On the above library loading error, the immediate reaction is so to brew install it

```batch
brew install libjpeg
Warning: jpeg 9c is already installed and up-to-date
To reinstall 9c, run `brew reinstall jpeg`
```

In most of the cases, you don't have the library missing installed and you can continue with you main task. In some cases, like mine, you already have it installed and you still have your problem.

The solution can be to `switch` the `libjpeg` version installed by `Brew`. First, retrieve the info of the formulae

```batch
brew info libjpeg
jpeg: stable 9c (bottled)
Image manipulation library
http://www.ijg.org
/usr/local/Cellar/jpeg/8d (19 files, 731.2KB)
  Poured from bottle on 2014-10-17 at 21:30:18
/usr/local/Cellar/jpeg/9b (20 files, 724KB)
  Poured from bottle on 2018-02-20 at 17:37:34
/usr/local/Cellar/jpeg/9c (21 files, 724.6KB) *
  Poured from bottle on 2018-06-12 at 14:24:01
From: https://github.com/Homebrew/homebrew-core/blob/master/Formula/jpeg.rb
```

And finally, switch to the version that you need. I was needed version 8, so I'm interested in `8d` version

```batch
brew switch libjpeg 8d
Cleaning /usr/local/Cellar/jpeg/8d
Cleaning /usr/local/Cellar/jpeg/9b
Cleaning /usr/local/Cellar/jpeg/9c
17 links created for /usr/local/Cellar/jpeg/8d
```

In many case, it makes it and this quick tip will help me to go faster next time.   
Hopefully it'll help you too.
