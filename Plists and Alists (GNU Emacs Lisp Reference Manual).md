<!DOCTYPE html>
<!-- saved from url=(0080)https://www.gnu.org/software/emacs/manual/html_node/elisp/Plists-and-Alists.html -->
<html><!-- Created by GNU Texinfo 7.0.3, https://www.gnu.org/software/texinfo/ --><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8">

<title>Plists and Alists (GNU Emacs Lisp Reference Manual)</title>

<meta name="description" content="Plists and Alists (GNU Emacs Lisp Reference Manual)">
<meta name="keywords" content="Plists and Alists (GNU Emacs Lisp Reference Manual)">
<meta name="resource-type" content="document">
<meta name="distribution" content="global">
<meta name="Generator" content="makeinfo">
<meta name="viewport" content="width=device-width,initial-scale=1">

<link rev="made" href="mailto:bug-gnu-emacs@gnu.org">
<link rel="icon" type="image/png" href="https://www.gnu.org/graphics/gnu-head-mini.png">
<meta name="ICBM" content="42.256233,-71.006581">
<meta name="DC.title" content="gnu.org">
<style type="text/css">
@import url('/software/emacs/manual.css');
</style>
</head>

<body lang="en">
<div class="subsection-level-extent" id="Plists-and-Alists">
<div class="nav-panel">
<p>
Next: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html" accesskey="n" rel="next">Property Lists Outside Symbols</a>, Up: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html" accesskey="u" rel="up">Property Lists</a> &nbsp; [<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents" title="Table of contents" rel="contents">Contents</a>][<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html" title="Index" rel="index">Index</a>]</p>
</div>
<hr>
<h4 class="subsection" id="Property-Lists-and-Association-Lists">5.9.1 Property Lists and Association Lists</h4>
<a class="index-entry-id" id="index-plist-vs_002e-alist"></a>
<a class="index-entry-id" id="index-alist-vs_002e-plist"></a>

<a class="index-entry-id" id="index-property-lists-vs-association-lists"></a>
<p>Association lists (see <a class="pxref" href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html">Association Lists</a>) are very similar to
property lists.  In contrast to association lists, the order of the
pairs in the property list is not significant, since the property
names must be distinct.
</p>
<p>Property lists are better than association lists for attaching
information to various Lisp function names or variables.  If your
program keeps all such information in one association list, it will
typically need to search that entire list each time it checks for an
association for a particular Lisp function name or variable, which
could be slow.  By contrast, if you keep the same information in the
property lists of the function names or variables themselves, each
search will scan only the length of one property list, which is
usually short.  This is why the documentation for a variable is
recorded in a property named <code class="code">variable-documentation</code>.  The byte
compiler likewise uses properties to record those functions needing
special treatment.
</p>
<p>However, association lists have their own advantages.  Depending on
your application, it may be faster to add an association to the front of
an association list than to update a property.  All properties for a
symbol are stored in the same property list, so there is a possibility
of a conflict between different uses of a property name.  (For this
reason, it is a good idea to choose property names that are probably
unique, such as by beginning the property name with the program’s usual
name-prefix for variables and functions.)  An association list may be
used like a stack where associations are pushed on the front of the list
and later discarded; this is not possible with a property list.
</p>
</div>
<hr>
<div class="nav-panel">
<p>
Next: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html">Property Lists Outside Symbols</a>, Up: <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html">Property Lists</a> &nbsp; [<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents" title="Table of contents" rel="contents">Contents</a>][<a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html" title="Index" rel="index">Index</a>]</p>
</div>





</body></html>