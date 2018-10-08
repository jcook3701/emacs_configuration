## Production: Emacs 25.3
### Requirements
> - Before you start, you need a few dependencies:
>   - Xcode (free in the Mac App Store).
>   - Autoconf and Automake. The easiest way to use Homebrew via brew install autoconf automake.
>   - makeinfo (part of the Texinfo suite). Apple ships makeinfo, but at some point the system version fell below the minimum version Emacs needs to build.
>
> #### Homebrew
>> makeinfo can also be installed from Homebrew:
>> ```bash
>> brew install texinfo
>> ```
>>
>> But before building Emacs, you need to get it into your $PATH ahead of /usr/bin/makeinfo
>> The following exports should be added to your bashrc or bash_profile.
>> ```bash
>> export PATH=/usr/local/opt/texinfo/bin:$PATH
>> export LDFLAGS=-L/usr/local/opt/texinfo/lib
>> ```
>>
>> This package is needed if you want the --with-gnutls flag to work correctly when you build emacs.  
>> Gnutls allows for the use of the package-list-packages function in emacs.
>> ```bash
>> brew install gnutls
>> ```

### Building Emacs 
> I Chose to store my build folder in my local Applications folder.  Feel free to store where ever is convenient for you.  
> ```
> /Users/jared3701/Applications/emacs/
> ```
>
> Once you have the prerequisites squared away, the build is the same as it’s been for a while. Get the source:
> ```bash
> cd /Users/jared3701/Applications/
> git clone git://git.savannah.gnu.org/emacs.git
> cd emacs
> ```
>
> Checkout the emacs-25 branch (master is the development branch):  
> ```bash
> git checkout emacs-25
> ```
>
> __Configuration Settings:__  
> These are the configuration settings that I used for this project.  
> - with-ns
>    - This determines that it will be build in the nextstep folder.
> - with-gnutls
>    - This was needed to get the emacs package manager working.
> - with-imagemagick
>
> Configure and compile (make install build the application bundle, it doesn’t actually install anything):
> ```bash
> make configure
> ./configure  --with-ns --with-gnutls --with-imagemagick
> make install
> ```

### Deployed Emacs 25.3
> __Deployment:__
> ```bash
> cd /Users/jared3701/Applications/
> mkdir emacs-25.3
> cd emacs-25.3
> rsync -avP /Users/jared3701/Applications/emacs/nextstep/Emacs.app /Users/jared3701/Applications/emacs-25.3/
> ```

### Helpful Links
> [https://stuff-things.net/2018/01/30/building-emacs-25-on-macos-high-sierra/](https://stuff-things.net/2018/01/30/building-emacs-25-on-macos-high-sierra/)

[back](.././)
