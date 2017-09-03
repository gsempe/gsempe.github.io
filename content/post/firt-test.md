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

<pre data-line="1" class="language-batch"><code class="language-batch">mkdir test
cd dir
curl -L -s -o hugo_0.26.tar.gz https://github.com/gohugoio/hugo/releases/download/v0.26/hugo_0.26_macOS-64bit.tar.gz
</pre></code>

~~~batch
mkdir test
cd dir
curl -L -s -o hugo_0.26.tar.gz https://github.com/gohugoio/hugo/releases/download/v0.26/hugo_0.26_macOS-64bit.tar.gz
~~~
~~~python
>>> from pygments.styles import get_all_styles
>>> styles = list(get_all_styles())
>>> print styles
['manni', 'igor', 'lovelace', 'xcode', 'vim', 'autumn', 'abap', 'vs', 'rrt', 'native', 'perldoc', 'borland', 'arduino', 'tango', 'emacs', 'friendly', 'monokai', 'paraiso-dark', 'colorful', 'murphy', 'bw', 'pastie', 'rainbow_dash', 'algol_nu', 'paraiso-light', 'trac', 'default', 'algol', 'fruity']
~~~

~~~html
<section id="main">
  <div>
    <h1 id="title">{{ .Title }}</h1>
    {{ range .Data.Pages }}
      {{ .Render "summary"}}
    {{ end }}
  </div>
</section>
~~~

~~~go
package main

import "fmt"

func main() {
	fmt.Println("Hello, 世界")
	// Remaining things here
}
~~~

~~~erlang
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
~~~

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
