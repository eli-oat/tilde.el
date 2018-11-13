# User Manual 

This started as a bare-bones setup for noodling on [tilde.town](https://tilde.town), but has since become my go-to setup for all things emacs. Evil, sort of like a really tiny [spacemacs](http://spacemacs.org/).

- `init.el` handles initialization of some important stuff, but should be left more or less as is. 
- `tilde.el` is where the action happens. This is where all packages and settings are configured
- `tree-fort.el` is an optional custom config. If the file is present it will be initialized after `tilde.el`. If you wish to expand or modify this configuration this is where I recommend that you do that 

## Key Chords of Note
### General:
```
SPC /     : ripgrep
SPC TAB   : previous buffer
SPC SPC   : Open M-x
```
### Files:
```
SPC p f   : find files
```
### Buffers:
```
SPC b b   : buffers list
```
### Window:
```
SPC w l   : move right
SPC w h   : move left
SPC w j   : move down
SPC w k   : move up
SPC w /   : split right
SPC w -   : split bottom
SPC w x   : close window
```
### Other:
```
SPC a t   : open terminal in current buffer
```

## How to Install

If you have an existing `.emacs.d` directory back it up. 

```bash
$ cp -r .emacs.d ~/.emacs.d.backup
```

Then delete it! 

```bash
$ rm -rf .emacs.d
```

Now clone this repo (making sure to save it to a new `.emacs.d` directory)! 

```bash
$ git clone git@github.com:eli-oat/tilde.el.git .emacs.d
```

Launch emacs! SUCCESS!

## Credit where credit is due 

The vast majority of this config was shamelessly taken from [*Emacs from Scratch*](https://huytd.github.io/emacs-from-scratch.html), with a bit of guidance from [hjertnes'](https://github.com/hjertnes/emacs.d) emacs.d.
