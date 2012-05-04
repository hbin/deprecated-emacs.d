## Emacs for hbin

This a personal Emacs configuration files write/accumulate by hbin.

### Getting Emacs 24

If you are an Ubuntu/Debian user like me, you can get and install Emacs 24
from here:

[Emacs 24 Debian packages](http://emacs.naquadah.org)

### Installation

#### 1. Dotfile

```bash
$ git clone git://github.com/hbin/dotfiles-for-emacs.git ~/emacs-hbin
$ git submodule sync
$ git submodule update --init --recursive
$ ln -ns ~/emacs-hbin ~/.emacs.d
```

#### 2. Rsense(optional)

This package have integrated with RSense which used to program ruby
language. Click [this](http://cx4a.org/software/rsense/) for more
detail about RSense.
<br>
Make sure you have install
[JDK](http://hbin.iteye.com/admin/blogs/1148147) and
[Ruby](http://beginrescueend.com/rvm/install/#explained)
property. And then test if the Rsense set up:

```bash
$ cd ~/emacs-hbin/tools/rsense
$ chmod +x bin/rsense
$ bin/rsense version
```

If you could see "rsense 0.x", that means Rsense is fine! Otherwise,
please click
[this](http://cx4a.org/software/rsense/manual.html#Trouble_Shooting)
for a trouble check list.

#### 3. No Rsense

If you don't like Rsense. You can comment following lines in hbin/ruby-config.el

```lisp
(setq rsense-home (expand-file-name (concat utils-dir "rsense")))
(add-to-list 'load-path (concat (concat utils-dir "rsense") "/etc"))
(require 'rsense)
```

and conmment following snippets in hbin/misc-ac.el

```lisp
ac-source-rsense
```

### Bugs & Improvements

Thank you very much for helping me at the [known issue list](https://github.com/hbin/dotfiles-for-emacs/issues?sort=created&direction=desc&state=open).
<br>
If you have any questions, feel free to contact me.
Bug reports always welcome.<br/>

### About Me

Hi, my name is
[Huang Bin](http://www.google.com.hk/#hl=en&newwindow=1&safe=strict&q=i+want+an+aston+martin&oq=I+want+an+aston+martin&aq=0&aqi=g1&aql=&gs_sm=1&gs_upl=17485l20618l17l22771l3l3l0l0l0l0l106l315l0.3l3l0&gs_l=serp.1.0.0.17485l20618l17l22771l3l3l0l0l0l0l106l315l0j3l3l0.frgbld.&bav=on.2,or.r_gc.r_pw.,cf.osb&fp=a292937e2003130a&biw=1310&bih=682). I
am a programmer lives in Shanghai China. I am a big fans of emacs and
I am very interesting in Ruby on Rails and FP. Nevertheless, I am a
Java programmer...<br> If you interesting in hire me,
[tell me about it](mailto:embrace.hbin@gmail.com).

### At Last
Done, cheer up!
