# OSGI background information

Some (rather incomplete) information about bundles, bundle dependencies and errors in that context.

## METAINF.MF
### Require-Bundle

- specifies the explicit bundle (and optionally version) to use. If a required bundle needs to be refactored and a package moved elsewhere, then dependents will need changes to their MANIFEST.MF
- gives you access to ALL exports of the bundle, regardless of what they are, and regardless of whether you need them. If the parts you don't need have their own dependencies you will need those to
- bundles can be re-exported
- although discouraged, allows the use of split packages, e.g.: a package that is spread across multiple bundles
- can be used for non-code dependencies, e.g.: resources, Help etc.

### Import-Package (preferred)

- looser coupling, only the package (and optionally version) is specified and the run-time finds the required bundle
- actual implementations can be swapped out
- dependent packages can be moved to different bundles by the package owner
- requires more meta data to be maintained (i.e: each package name) at lower levels of granularity

[see stackoverflow](http://stackoverflow.com/questions/1865819/when-should-i-use-import-package-and-when-should-i-use-require-bundle)

## Solving dependency problems

[see this blog entry](http://njbartlett.name/2011/09/02/uses-constraints.html)

Solving dependency problems in an rcp application can be supported by using the Host OSGi-console view (luckily available within the test-edtor).
To get an overview of available commands simply type ```help```.

(Very incomplete) List of commands:
- packages PACKAGE-NAME : list of bundles importing the given PACKAGE, from which other bundle this is imported, and what version is actually used
- lb : list bundles, list all loaded bundles, their id and their current state
- bundle BUNDLE-ID-OR-NAME : list extended information regarding this bundle, including the imported packages (version and from which bundle they were actually imported), exported pacakges ...

### Common errors 
- uses constraint violations (bundle can load dependency via two different dependency paths)
  A package can be loaded from different bundles (possibly in different versions)

