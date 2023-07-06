# Change structure of messages/errors/warnings:
 
## Change the structure of the messages using the more portable C like format: `base::gettextf`:

```
stop(gettextf("You gave %d arguments but %d are needed.", n, m))
```

 - [ ] change in `sanitizing.R`
 - [ ] change throughout the package for odd ones (`grep` "message", "warning", "stop", "cat")

## Check pipeline through `potools`

[potools](https://cran.r-project.org/web//packages//potools/potools.pdf)






# NEWS

dispRity v1.7.13 (2023-07-06)
=========================

### NEW FEATURES

 * Converted all internal messages and documentations towards a portable friendly format (for future translation!)

dispRity v1.8 *more accessible*

 * Package fully translated in french.
 * Automatic translation in any language.
 * New vignette for helping community translation in any other language.


