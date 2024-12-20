{include resources/header.md}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
{remark  * [Documentation][5]}
  * [News][6]
{remark  * [Test results][tr]}
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ "documentation link"
   [6]: #news
   [7]: changelog.html
   [tr]: test-report.html
   
</div>
<div class="system-description">

### What it is

Class-mixins are a great idea but sometimes they lead to a
proliferation of subclasses with names like
`printable-touchable-composing-colored-shape-square-mixin`
and that's just ugly. Common-Lisp is a dynamic-programming
language; Dynamic-Classes brings dynamism to class
definition!

You define the mixins and associate them with parameters.
Then you

As an example, here is some code from [cl-containers][]
iterators that hooks parameters to iterator mixins:

    (add-parameter->dynamic-class
     :iterator :transform 'transforming-iterator-mixin)

    (add-parameter->dynamic-class
     :iterator :filter 'filtered-iterator-mixin)

    (add-parameter->dynamic-class
     :iterator :unique 'unique-value-iterator-mixin)

    (add-parameter->dynamic-class
     :iterator :circular 'circular-iterator-mixin)

and here it is in action:

    > (let ((i (make-iterator '(1 2 3))))
    	    (loop repeat 5 when (move-forward-p i) do
    		 (print (next-element i))))
    1 
    2 
    3 
    nil 
    
    > (let ((i (make-iterator '(1 2 3) :circular t)))
    	    (loop repeat 5 when (move-forward-p i) do
    		 (print (next-element i))))
    1 
    2 
    3 
    1 
    2 

    > (let ((i (make-iterator '(1 2 3) :circular t :transform #'sqrt)))
    	    (loop repeat 5 when (move-forward-p i) do
    		 (print (next-element i))))
    1.0 
    1.4142135 
    1.7320508 
    1.0 
    1.4142135 

The `make-iterator` function calls `determine-iterator-class`
to handle the work of figuring how (and possibly creating)
the right class given the parameters.

    (defmethod make-iterator
        (iteratee &rest args &key (iterator-class nil) &allow-other-keys)
      (apply #'make-instance 
    	 (apply #'determine-iterator-class iteratee iterator-class args)
             :container iteratee
             args))

Dynamic-Classes can make prototyping a breeze (and it's lots
of fun too)!

{anchor mailing-lists}

### Mailing Lists

  * [dynamic-classes-devel][devel-list]: A list for announcements, questions, patches, bug reports, and so on; It's for everything _but_ the sinking kitchen.

{anchor downloads}

### Where is it

metabang.com is switching from [darcs][] to [git][]
for source control; the current dynamic-classes repository is on
[github][github-dynamic-classes] and you can clone it using:

    git clone git://github.com/gwkkwg/dynamic-classes

Dynamic-Classes is also [ASDF installable][]. Its CLiki home
is right [where][cliki-home] you'd expect.

There's also a handy [gzipped tar file][tarball].


{anchor news}

### What is happening

2010 Dec 21 
moved to github.

2008-May-26
We've split off from metatilities and are living large and on
our own! More tests and documentation coming ... soon (we
hope).

</div>
</div>

{include resources/footer.md}

