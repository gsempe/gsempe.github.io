---
categories:
  - Others
tags:
  - Premier tag
title: "First article"
date: 2017-08-29T06:47:57+01:00
slug: it's first article
draft: true
---

Hello
Default Pygments style are. A link to the page [showcase](https://help.farbox.com/pygments.html)

{{< highlight python "hl_lines=2 3" >}}
>>> from pygments.styles import get_all_styles
>>> styles = list(get_all_styles())
>>> print styles
['manni', 'igor', 'lovelace', 'xcode', 'vim', 'autumn', 'abap', 'vs', 'rrt', 'native', 'perldoc', 'borland', 'arduino', 'tango', 'emacs', 'friendly', 'monokai', 'paraiso-dark', 'colorful', 'murphy', 'bw', 'pastie', 'rainbow_dash', 'algol_nu', 'paraiso-light', 'trac', 'default', 'algol', 'fruity']
{{< /highlight >}}

{{< highlight html "hl_lines=2 3" >}}
<section id="main">
  <div>
    <h1 id="title">{{ .Title }}</h1>
    {{ range .Data.Pages }}
      {{ .Render "summary"}}
    {{ end }}
  </div>
</section>
{{< /highlight >}}

{{< highlight go >}}

package main

import "fmt"

func main() {
	fmt.Println("Hello, 世界")
	// Remaining things here
}
{{< /highlight >}}

{{< highlight erlang "hl_lines=8">}}
-module(talk).
-export([worker/0]).

worker() ->
    work(0).

%% Work to do
work(N) ->
    Msg = {self(),N},
    echo!Msg,
    io:format("~w sent.~n",[Msg]),
    receive
        _Reply ->
            timer:sleep(500),
            work(N+1)
    end.
{{< /highlight >}}

``` html
<section id="main">
  <div>
    <h1 id="title">{{ .Title }}</h1>
    {{ range .Data.Pages }}
      {{ .Render "summary"}}
    {{ end }}
  </div>
</section>

```
