# Emacs for hbin

This is a personal Emacs configuration repo written/accumulated by [hbin](http://hbin.me). Only Emacs 24 supported.

## Installation

### 1. Install the dotfile and its submodules

```bash
$ git clone https://github.com/hbin/emacs.d.git ~/.emacs.d
$ git submodule sync
$ git submodule update --init --recursive
```

### 2. Ack!

[Ack](http://betterthangrep.com/) is a tool like grep, optimized for
programmers. Click [here](http://betterthangrep.com/install/) for the
wizard.

### 3. Ctags

[ctags](http://ctags.sourceforge.net/) help you to quickly and easily
locate method, constant while programming.
```bash
sudo aptitude install ctags
```

This package has been optimized the etags select feature, it's so awesome.

### 4. Markdown

For preview the markdown doc(markdown-preview), you may need this.

```bash
sudo aptitude install markdown
```

### 5. Rsense

[Rsense](http://cx4a.org/software/rsense/manual.html) is a very smart
code-completing plugin for coding ruby programs.

- Install JRE: Both Oracle JDK and OpenJDK are supported.
- Install Rsense: Download and extract the package to some where you want.
- Setting for Emacs: Add RSENSE_HOME environment variable to your system.

### Running in daemon mode

To use emacs in daemon mode, you may need following snippet in your
.zshrc or .bashrc.
```bash
export ALTERNATE_EDITOR=""
alias e="emacsclient -c"
alias t="emacsclient -t"
alias ec="emacsclient -e '(kill-emacs)'"
```

## Bugs & Improvements

Thank you very much for helping me at the [known issue list](https://github.com/hbin/emacs.d/issues?sort=created&direction=desc&state=open).
<br>
If you have any questions, feel free to contact me.
Bug reports always welcome.<br/>

## About Me

Hi, my name is
[Huang Bin](http://www.google.com.hk/#hl=en&newwindow=1&safe=strict&q=i+want+an+aston+martin&oq=I+want+an+aston+martin&aq=0&aqi=g1&aql=&gs_sm=1&gs_upl=17485l20618l17l22771l3l3l0l0l0l0l106l315l0.3l3l0&gs_l=serp.1.0.0.17485l20618l17l22771l3l3l0l0l0l0l106l315l0j3l3l0.frgbld.&bav=on.2,or.r_gc.r_pw.,cf.osb&fp=a292937e2003130a&biw=1310&bih=682). I
am a programmer lives in Beijin China. I am a big fans of Emacs and
I am very interesting in Ruby on Rails and FP.<br> If you're interesting in hire me,
[tell me about it](mailto:embrace.hbin@gmail.com).

## At Last
Refactor Done, cheer up!
