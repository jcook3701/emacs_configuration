---
layout: default
---

# Emacs 29.4 - Linux - Ubuntu 22

Configuration Flags
```bash
$ ./configure --with-tree-sitter --with-native-compilation --with-json --with-mailutils --with-jpeg --with-png --with-rsvg --with-tiff --with-gif --with-xft --with-xml2 --without-ns --with-gnutls --with-imagemagick --with-xwidgets --with-x --with-modules --with-harfbuzz 
```

__Notes:__ [Getting Started With Tree-Sitter](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter)
__NOTES:__ [Helpful Build discussion](https://www.reddit.com/r/emacs/comments/qf9jjx/what_compilation_flags_do_you_use_for_your_emacs/)

# Emacs 25.3 Built for MacOS High Sierra - 2018
Version: Mac OS 10.13.6
## Production: Emacs 25.3
### Requirements

- Before you start, you need a few dependencies:
  - Xcode (free in the Mac App Store).
  - Autoconf and Automake. The easiest way to use Homebrew via brew install autoconf automake.
  - makeinfo (part of the Texinfo suite). Apple ships makeinfo, but at some point the system version fell below the minimum version Emacs needs to build.

#### Homebrew

makeinfo can also be installed from Homebrew:
```bash
brew install texinfo
```

But before building Emacs, you need to get it into your $PATH ahead of /usr/bin/makeinfo
The following exports should be added to your bashrc or bash_profile.
```bash
export PATH=/usr/local/opt/texinfo/bin:$PATH
export LDFLAGS=-L/usr/local/opt/texinfo/lib
```

This package is needed if you want the --with-gnutls flag to work correctly when you build emacs.  
Gnutls allows for the use of the package-list-packages function in emacs.
```bash
brew install gnutls
```

### Building Emacs 

I Chose to store my build folder in my local Applications folder.  Feel free to store where ever is convenient for you.  
```
/Users/jared3701/Applications/emacs/
```

Once you have the prerequisites squared away, the build is the same as it’s been for a while. Get the source:
```bash
cd /Users/jared3701/Applications/
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
```

Checkout the emacs-25 branch (master is the development branch):  
```bash
git checkout emacs-25
```

__Configuration Settings:__  
These are the configuration settings that I used for this project.  
- with-ns
   - This determines that it will be build in the nextstep folder.
- with-gnutls
   - This was needed to get the emacs package manager working.
- with-imagemagick

Configure and compile (make install build the application bundle, it doesn’t actually install anything):
```bash
make configure
./configure  --with-ns --with-gnutls --with-imagemagick
make install
```

### Deployed Emacs 25.3

__Deployment:__
```bash
cd /Users/jared3701/Applications/
mkdir emacs-25.3
cd emacs-25.3
rsync -avP /Users/jared3701/Applications/emacs/nextstep/Emacs.app /Users/jared3701/Applications/emacs-25.3/
```

### Helpful Links
https://stuff-things.net/2018/01/30/building-emacs-25-on-macos-high-sierra/


----------------------------


## Bleeding Edge: Emacs 25.3 with xwidget
### Why?

The purpose of building Emacs with xwidget is to make it possible to use the command "xwidget-webkit-browse-url".  This command allows users to open web-pages inside of Emacs without ever having to leave Emacs.  I have included configuration settings for navigating web-pages in my init.el file included in this Github repository.

### Requirements

Before starting it is very important to make sure that both your Homebrew and MacPort installations are fully updated.  As they will be needed for the installation of the following packages: autoconf, automake, gtk3, & webkitgtk.  

#### Homebrew
makeinfo can also be installed from Homebrew:
```bash
brew install texinfo
```

But before building Emacs, you need to get it into your $PATH ahead of /usr/bin/makeinfo
The following exports should be added to your bashrc or bash_profile.
```bash
export PATH=/usr/local/opt/texinfo/bin:$PATH
export LDFLAGS=-L/usr/local/opt/texinfo/lib
```

This package is needed if you want the --with-gnutls flag to work correctly when you build emacs.  
Gnutls allows for the use of the package-list-packages function in emacs.
```bash
brew install gnutls
```

This package is needed for Emacs to build with the flag "--with-xwidget".
```bash
brew install gtk+3
```

#### MacPorts
This package is also needed for Emacs to build with the flag "--with-xwidget".
```bash
port install webkit-gtk3
``` 

### Building Emacs

I Chose to store my build path local Applications folder.  Feel free to store where ever is convenient for you.  
```
/Users/jared3701/Applications/emacs/
```

Once you have the prerequisites squared away, the build is the same as it’s been for a while. Get the source:  
```bash
cd /Users/jared3701/Applications/
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
```

Checkout the emacs-25 branch (master is the development branch):  
```bash
git checkout emacs-25
```

__Configuration Settings:__  
These are the configuration settings that I used for this project.  
```bash
./autogen.sh all
./configure --with-xwidgets --without-ns --with-gnutls --with-imagemagick --without-dbus --with-x
make install
```

Unless you specify a path emacs will be installed in the following location when using "make install" with the following configuration.  
```
/usr/local/bin/
```

### XQuartz

If running emacs from /usr/local/bin/ starts in no window mode then you must start XQuartz and point it to emacs.  
```bash
XQuartz
```

Inside of XQuartz run the 'Add Item' command.  
```
Applicaion -> Customize. -> Add Item
```
![alt text](./images/XQuartz_Applications_Customize.png "Applicaion -> Customize")

The new item requires a Name and Command to be associated with it.  
```
Name: emacs
Command: /usr/local/bin/emacs
```
![alt text](./images/XQuartz_Application_emacs_setup.png "Applicaion -> Customize -> emacs")

I was experiencing errors with C-space for the Mark set and found that it could be fixed from:
```
XQuartz -> Preferences -> Input
------------------------------------------
Uncheck -> Enable key equivalents under X11
Check   -> Follow system keyboard layout
```
![alt text](./images/XQuartz_preferences.png "Applicaion -> Customize -> emacs")
![alt text](./images/XQuartz_input_preferences.png "Applicaion -> Customize -> emacs")

If Built with the flag "--with-dbus" or don't use the flag "--without-dbus" you might get the following error. Never less in my experiences a dbus error can be solved with the below commands.  
```bash
port notes dbus
```

This is the output of the command "port notes dbus".  
```bash
Jareds-MacBook-Pro-2:bin jared3701$ port notes dbus
dbus has the following notes:
  Startup items (named 'dbus-system, dbus-session') have been generated that will aid in starting dbus with launchd. They are disabled
  by default. Execute the following command to start them, and to cause them to launch at startup:

      sudo port load dbus
```

Running sudo port load dbus starts the dbus and fixes the error.  
```
sudo port load dbus
```

This is the output of the command "sudo port load dbus".
```bash
Jareds-MacBook-Pro-2:bin jared3701$ sudo port load dbus
Password:
--->  Loading startupitem 'dbus-system' for dbus
--->  Loading startupitem 'dbus-session' for dbus
```

### Demonstration
![alt text](./images/emacs_as_browser.png "Emacs as a browser")

### Helpful Links
#### Building Emacs
https://stackoverflow.com/questions/24213842/webkit-not-found-on-osx
https://emacs.stackexchange.com/questions/25037/compile-emacs-with-xwidget-under-osx
https://jiegec.me/programming/2016/02/18/building-emacs-git-version-with-xwidgets-and-modules-in-archlinux/
https://github.com/veshboo/emacs
#### Using xwidget
https://www.reddit.com/r/emacs/comments/4srze9/watching_youtube_inside_emacs_25/
https://www.youtube.com/watch?v=J2YdjpWJJHs
#### XQuartz
https://stackoverflow.com/questions/37826094/xt-error-cant-open-display-if-using-default-display
https://emacs.stackexchange.com/questions/21285/set-mark-command-c-spc-not-recognised-broken


[back](./)
