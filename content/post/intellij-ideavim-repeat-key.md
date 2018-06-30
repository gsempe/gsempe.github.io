---
description: "How to fix the IdeaVim key repeat is not working"
keywords:
  - jetbrains
  - intellij idea
  - vim
  - ide
  - key repeat
categories:
  - Quick Tips
tags:
  - vim
  - intellij

title: "IntelliJ IDEA and IdeaVim key does not repeat"
date: 2018-06-30T17:30:50+01:00
draft: false
---

When I started C language years ago, I became a Vim editor developer. Since then, for all my backend developments, I have been faithful to Vim.  

Few weeks ago, when I started to code in Elixir, I started as well to use [IntelliJ IDEA](https://www.jetbrains.com/idea/) with the [Elixir plugin](https://github.com/KronicDeth/intellij-elixir) and obviously the [Vim plugin](https://github.com/JetBrains/ideavim).  

On OSX, when you use the combination IntelliJ and IdeaVim it can happen that the keys you hold does not repeat themselves. For instance, when you are used to navigate in your code by holding the `b` or the `w` key to move in one line of code you feel quickly unproductive with this limitation.

To solve this issue IdeaVim team advises to change globally the `ApplePressAndHoldEnable` key value to false (cf. this [IdeaVim code](https://github.com/JetBrains/ideavim/blob/master/src/com/maddyhome/idea/vim/helper/MacKeyRepeat.java#L33))

```bash
defaults write -globalDomain ApplePressAndHoldEnable -bool false
```

This change fix the do not repeat key issue but impact your whole system. The problem can be fixed by changing the `ApplePressAndHoldEnable` key only in the Jetbrains IntelliJ domain

```bash
defaults write com.jetbrains.intellij ApplePressAndHoldEnabled -bool 0
```

To be sure that your command has been accepted, the defaults read should return `0`

```bash
defaults read com.jetbrains.intellij ApplePressAndHoldEnabled
0
```

You have to restart IntelliJ IDEA to let it gets this change.

If you have any doubts, you can verify that the JetBrains IntelliJ domain is still `com.jetbrains.intellij` 

```bash
defaults domains | tr ',' '\n' | grep jetbrains
```

