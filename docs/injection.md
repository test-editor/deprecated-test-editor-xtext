# Injection


## OSGI background information

* Uses constraint violations (bundle can load dependency via two different dependency paths)

[see this blog entry](http://njbartlett.name/2011/09/02/uses-constraints.html)

* METAINF.MF
** Require-Bundle

- specifies the explicit bundle (and optionally version) to use. If a required bundle needs to be refactored and a package moved elsewhere, then dependents will need changes to their MANIFEST.MF
- gives you accesss to ALL exports of the bundle, regardless of what they are, and regardless of whether you need them. If the parts you don't need have their own dependencies you will need those to
- bundles can be re-exported
- although discouraged, allows the use of split packages, ie: a package that is spread across multiple bundles
- can be used for non-code dependencies, eg: resources, Help etc.

** Import-Package (preferred)

- looser coupling, only the package (and optionally version) is specified and the run-time finds the required bundle
- actual implementations can be swaped out
- dependent packages can be moved to different bundles by the package owner
- requires more metadata to be maintained (i.e: each package name) at lower levels of granularity

[see stackoverflow](http://stackoverflow.com/questions/1865819/when-should-i-use-import-package-and-when-should-i-use-require-bundle)

## E4 Injection


## Xtext Guice Injection
