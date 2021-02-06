---
description: "A tiny helper function to create HTML elements in JavaScript"
keywords:
  - javascript
  - js
  - HTML element
categories:
  - Quick Tips
tags:
  - frontend
  - javascript

title: "Vanilla JavaScript: Create a HTML Element"
date: 2021-02-06T17:41:46Z
draft: false
---

In all the frontend projects I had been involved with, we ended up to need to add HTML
elements dynamically to a web page.

## Functions to create HTML elements

There are many implementations around the web of this small feature, but I find this one below
to be the clearer and easiest version to use. This version has also the advantage to be vanilla js.

```js
/**
 * creates a HTML element ready to be append.
 * @param {string} [tag=div] HTML tag of the element
 * @param {Object} [props={}] properties to add to the element
 * @param {HTMLElement[]} [children=[]] Children tot add to the element
 */
function $e(tag='div',props={},children=[]){
  let el = document.createElement(tag);
  Object.assign(el, props);
  el.append(...children);
  return el;
}

/**
 * creates a function callable from anywhere to create a text node.
 *
 * cf. bind documentation https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/bind
 */
let $t = document.createTextNode.bind(document);
```

## Examples

### Create a form HTML element

**JavaScript code**

```js
const form = $e('form', {className:'js-foo-form', action: '/foo', method: 'post'}, [
    $e('button', {className: 'bg-primary hover:bg-primary-darker'}, [
  	    $t('Ok'),
    ]),
]);

document.body.append(form);
```

**HTML output**

```html
<form class="js-foo-form" action="/foo" method="post">
    <button class="bg-primary hover:bg-primary-darker">Ok</button>
</form>
```