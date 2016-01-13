# RFC (Request for Comments)
This file is a work in progress. At the moment it serves as a place for piling up notes and ideas for R coding style in the financial sector, (not just for this particular repository). Github was chosen for this purpose because one can edit / contribute using only a web browser.

## Notes On Contributing

Guidelines for making the file **'CONTRIBUTING(.md)'**  
* https://help.github.com/articles/setting-guidelines-for-repository-contributors/
* https://github.com/blog/1184-contributing-guidelines
* https://help.github.com/articles/setting-guidelines-for-repository-contributors/

Examples:
* [Atom](https://github.com/atom/atom/blob/master/CONTRIBUTING.md)
* [Puppet](https://raw.githubusercontent.com/puppetlabs/puppet/master/CONTRIBUTING.md)

## Code Style

Resources:
* [Google style](https://google.github.io/styleguide/Rguide.xml)  
* [Hadley Wickham's guide derived from Google style](http://adv-r.had.co.nz/Style.html)  
* [a guide by csgillespie](https://csgillespie.wordpress.com/2010/11/23/r-style-guide/)
* [r-bloggers' guide #1](http://www.r-bloggers.com/google-r-style-guide/)
* [r-blogger's naming conventions](http://www.r-bloggers.com/consistent-naming-conventions-in-r/)
* [by Derek Jones](http://www.r-bloggers.com/the-most-worthwhile-r-coding-guidelines-i-know/)

### Variable names

### Function names

#### Type/State/Value testing functions
* value testing functions should obey the same convention as isNULL(), isNA, etc.
* type testing functions shall use 'dot.sep': is.*(), where * stands for a type/class, usually tested by 'inherits' within the body of the function.

----------------------------------------------------------------------
## Miscellaneous notes: to be sorted among 'semantic blocks' in due time
* Dot separation — “xyz.class” — shall be used only to demonstrate the class of an object (variable / function, etc.)  
* Dot underscore — “._xyz” — shall be used for internal to a function variables never to be used outside of the function  
* Dot underscore (times N, N>1) — “.__xyz”,  “.___xyz” — shall be used in infrastructure code (akin to C reserved name conventions)  
* Names of constants shall be in CAPITAL_CASE with underscore(s).  
* Elsewhere “lowerCamelCase” shall be used (to be decided)  
* The dot sep (“xyz.nop.lovely.name”) might be popular because it is easy to type. However, it might only be restricted for field names in output tables: dot consumes less space compared to an underscore, so using a dot might make save screen space.

## Useful Resources on R
* [on a need for language specification](http://www.r-bloggers.com/an-academic-programming-language-paper-about-r/)  
* ..

## Markdown Cheatsheets & Style Guides
* [reference by adam-p](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)  
* [reference by John Gruber](https://daringfireball.net/projects/markdown/syntax)  
* [md style guide](http://www.cirosantilli.com/markdown-style-guide/)

## Notes On Roadmaps
Examples:  
* [IPython](https://github.com/ipython/ipython/wiki/Roadmap:-IPython)  
 
