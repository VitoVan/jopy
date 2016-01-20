# It's painful to use Common Lisp to build web spiders.

* Drakma does not support some character encoding such as: GBK, this makes Chinese user crazy.
* Some Common Lisp implements can't handle Threads very well (and SBCL's ```with-timeout``` won't work normally when ```burgled-batteries``` is loaded).
* You'll always have to handle some special characters in the web page, which makes your HTML Parser stop working.
* ......

# I love lisp so much, but now, the love hurts me.

* Common Lisp is awesome, no doubt, but...
* The community is small, you can't always find the stable packages you need.
* Even you found some packages you need, moust of these projects are not active.
* It's a wonderful place to build wheels, still.
* Don't use it now, unless you own everything you need to touch (such as a inner logical system).

* If you wana use Common Lisp, use it as a brain, not hands or legs.

# Clojure, oh clojure

* Clojure is ugly.
* It has the same problem as Common Lisp, lack of packages.
* They say Clojure can use any Java package, I feel sick to write code like this: ```(javax.swing.JOptionPane/showMessageDialog nil "sick")```
* Enlive is tooooooo heavy, and clj-tagsoup is lack of many things.
* It's still a place to build wheels, wonderful or not, I can't say.
* I won't use it, for now.

# Then what?

* Use Python for scripting, it's your suit armor.
* Use Node.js for HTML / JSON generating, it's your nice dress.
* Use Common Lisp for logical handling, if it is possible, it should be your brain.

http://ynniv.com/blog/2006/11/how-python-is-killing-lisp.html
