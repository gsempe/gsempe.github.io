---
description: "How to use pkgsite to generate your pkg godoc"
keywords:
  - godoc
  - pkg
  - modules
categories:
  - Quick Tips
tags:
  - go

title: "Generate godoc for pkg with generics"
date: 2022-04-10T14:39:36Z
draft: false
---

Go 1.18 is here with the addition of the generics. This is like the biggest change
to the language since version 1 and consequently it has many impacts to the tooling
around the code.

One of these impacts is the [godoc command deprecation](https://github.com/golang/go/issues/49212)
replaced by the command [pkgsite](https://go.googlesource.com/pkgsite).

When you create your own pkg, and you want to test your godoc locally you
have first to build pkgsite
```batch
git clone https://go.googlesource.com/pkgsite pkgsite
cd pkgsite
go install ./cmd/pkgsite
```

with the pkgsite command ready, you can cd in your pkg root directory, where your `go.mod` file is,
and launch the godoc http server
```batch
# Go to the root directory of your project
cd your/project/root/directory/
# Execute pkgsite without parameters
~/source/bin/pkgsite
Listening on addr http://localhost:8080
```

You can finally visualize your documentation. If the URL of your pkg is
`github.com/gsempe/xxxx`, you can see the documentation of your pkg at the
address [http://localhost:8080/github.com/gsempe/xxxx](http://localhost:8080/github.com/gsempe/xxxx)
