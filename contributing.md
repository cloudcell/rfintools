this file is a work in progress

## Notes On Contributing

Guidelines for making the file 'contributing' 
* https://github.com/blog/1184-contributing-guidelines
* https://help.github.com/articles/setting-guidelines-for-repository-contributors/

Examples:
* [Atom](https://github.com/atom/atom/blob/master/CONTRIBUTING.md)
* [Puppet](https://raw.githubusercontent.com/puppetlabs/puppet/master/CONTRIBUTING.md)

## Code Style

### Variable names

### Function names


----------------
RFC (request for comments)

Dot separation — “xyz.class” — shall be used only to demonstrate the class of an object (variable / function, etc.)

Dot underscore — “._xyz” — shall be used for internal to a function variables never to be used outside of the function

Dot underscore (times N, N>1) — “.__xyz”,  “.___xyz” — shall be used in infrastructure code (akin to C reserved name conventions)

Names of constants shall be in CAPITAL_CASE with underscore(s).

Elsewhere “lowerCamelCase” shall be used (to be decided)

The dot sep (“xyz.nop.lovely.name”) might be popular because it is easy to type.






