---
title: Welcome
layout: page
---

The `wave-machine` project is a way for me to learn more about sound and the Haskell programming language. As I learn and experiment, I will share my findings in this blog. Here are my blog entries in chronological order:

<nav>
{% for post in site.posts reversed %}
* [{{ post.title }}]({{ site.url }}{{ post.url }}){% endfor %}
</nav>


