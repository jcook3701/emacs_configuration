---
layout: default
{ site.data.navigation}
---

# Emacs 29.4 - Linux - Ubuntu 22

Configuration Flags:  
```bash
$ ./configure --with-tree-sitter --with-native-compilation --with-json --with-mailutils --with-jpeg --with-png --with-rsvg --with-tiff --with-gif --with-xft --with-xml2 --without-ns --with-gnutls --with-imagemagick --with-xwidgets --with-x --with-modules --with-harfbuzz 
```

## After First Run (Within Emacs)
1. Install fonts for emacs package all-the-icons.  
```
$ M-x all-the-icons-install-fonts
```
2. Install LSP servers:  
```
$ M-x lsp-install-server
```
	1.  yamlls  
	2.  xmlls  
	2.  html-ls  
	3.  metals  
	4.  marksman  
	4.  magik  
	5.  json-ls  
	6.  eslint  
	7.  dockerfile-ls  
	8.  css-ls  
	9.  cmakels  
	10. gopls  <!-- Best when installed with system package manager -->
	11. bash-ls  
	12. ansible-ls  
	13. pyright  

3. Install all Treesitter Packages:  
```
$ M-x treesit-install-language-grammar
```
__NOTES:__ [Getting Started With Tree-Sitter](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter)
__NOTES:__ [Helpful Build discussion](https://www.reddit.com/r/emacs/comments/qf9jjx/what_compilation_flags_do_you_use_for_your_emacs/)

[back](./)
