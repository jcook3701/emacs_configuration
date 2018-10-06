# Emacs 25.3 Built for MacOS High Sierra

## Emacs 25.3 --with-ns
### Requirements

- Before you start, you need a few dependencies:
  - Xcode (free in the Mac App Store).
  - Autoconf and Automake. The easiest way to use Homebrew via brew install autoconf automake.
  - makeinfo (part of the Texinfo suite). Apple ships makeinfo, but at some point the system version fell below the minimum version Emacs needs to build.


#### Homebrew

makeinfo can also be installed from Homebrew:
```
brew install texinfo
```

But before building Emacs, you need to get it into your $PATH ahead of /usr/bin/makeinfo
The following exports should be added to your bashrc or bash_profile.
```
export PATH=/usr/local/opt/texinfo/bin:$PATH
export LDFLAGS=-L/usr/local/opt/texinfo/lib
```

This package is needed if you want the --with-gnutls flag to work correctly when you build emacs.  
Gnutls allows for the use of the package-list-packages function in emacs.
```
brew install gnutls
```


#### Building Emacs 

I Chose to store my build folder in my local Applications folder.  Feel free to store where ever is convenient for you.  
```
/Users/jared3701/Applications/emacs/
```

Once you have the prerequisites squared away, the build is the same as it’s been for a while. Get the source:
```
cd /Users/jared3701/Applications/
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
```

Checkout the emacs-25 branch (master is the development branch):  
```
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
```
make configure
./configure  --with-ns --with-gnutls --with-imagemagick
make install
```

------------------------

#### Deployed Emacs 25.3

__Deployed Path:__
```
/Users/jared3701/Applications/emacs-25.3/
```

#### Helpful Links
https://stuff-things.net/2018/01/30/building-emacs-25-on-macos-high-sierra/


#### Important Paths


## Emacs 25.3 --with-xwidgets 
#### Requirements

Before starting it is very important to make sure that both your Homebrew and MacPort installations are fully updated.  
needed packages include: autoconf automake gtk3 webkitgtk.  

##### Homebrew
```
brew install texinfo
brew install gnutls
brew install gtk+3
```

##### MacPorts
```
port install webkit-gtk3
``` 

#### Build

I Chose to store my build path local Applications folder.  Feel free to store where ever is convenient for you.  
```
/Users/jared3701/Applications/emacs/
```

Once you have the prerequisites squared away, the build is the same as it’s been for a while. Get the source:  
```
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
```

Checkout the emacs-25 branch (master is the development branch):  
```
git checkout emacs-25
```

Build emacs with the following commands.  
__First Try:__
```
./autogen.sh all
./configure --with-xwidgets --without-ns --with-gnutls --with-imagemagick --with-x-toolkit=gtk3 --without-x
make install
```

__Second Try:__
```
./configure --with-xwidgets --without-ns --with-gnutls --with-imagemagick
```

__Third Try:__ I am currently actively using this. 
```
./configure --with-xwidgets --without-ns --with-gnutls --with-imagemagick --without-dbus --with-x
```

__Flags I should test:__
```

```

Unless you specify a path emacs will be installed in the following location.  
```
/usr/local/bin/
```

#### XQuartz

If running emacs from /usr/local/bin/ starts in no window mode then you must start XQuartz and point it to emacs.  
```
XQuartz
```

Inside of XQuartz run the 'Add Item' command.  
```
Applicaion -> Customize. -> Add Item
```

The new item requires a Name and Command to be associated with it.  
```
Name: emacs
Command: /usr/local/bin/emacs
```

I was experiencing errors with C-space for the Mark set and found that it could be fixed from:
```
XQuartz -> Preferences
------------------------------------------
Uncheck -> Enable key equivalents under X11
Check   -> Follow system keyboard layout
```

If you get a dbus error you can run the following commands to fix it.   
```
port notes dbus
```

This is the output of 'port notes dbus.'  
```
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

This is the output of 'sudo port load dbus'
```
Jareds-MacBook-Pro-2:bin jared3701$ sudo port load dbus
Password:
--->  Loading startupitem 'dbus-system' for dbus
--->  Loading startupitem 'dbus-session' for dbus
```

#### Helpful Links
https://jiegec.me/programming/2016/02/18/building-emacs-git-version-with-xwidgets-and-modules-in-archlinux/
