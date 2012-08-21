## Snipptes for hbin (Yasnippets-hbin) closed beta ##

### Why I create this repo? ###
I use both auto-complete and yasnippet. As we know, AC (auto-complete) can use
yasnippets as a source to ac-expand. So I decide to create my own
snippets. If you use these two extensions both, and encounter the same
scenario like  me, you can get this for you too.

### What's the difference from others' snippet ###

* Compatibility
  It's compatible with Auto-Complete, which is an awesome extention too.

* Rules of the snippets
  While I use other's or the default snippets shipped with Yasnippet,
  the convention of the snippets make me and AC confuse.

### How to use ###

#### Install ####
After you have installed yasnippet and auto-complete work well.

```bash
$ cd ~/.emacs.d
$ git clone git://github.com/hbin/yasnippets-hbin.git
```
And then, add this
```
(yas/load-directory "yasnippets-hbin")
```
into your load path.

#### Compatible with default snippets ####
Yasnippet has shipped snippets for different modes. To prevent from
ambiguity, it's better to add follow code to your load-path.
```
(setq yas/ignore-filenames-as-triggers t)
(setq-default yas/snippet-dirs "yasnippets-hbin")
(yas/load-directory yas/root-directory)
```
### Languages/frameworks support ###
Ruby
Rails

### At Last ###
This repo cannot cover all program languages, I am very happy for your help.
Click [here](https://github.com/hbin/yasnippets-hbin/wiki/Rules-Of-Snippets) for more information about the rules.
<br><br>
Bin Huang
