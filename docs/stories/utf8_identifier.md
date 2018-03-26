# UTF-8 Characters within Identifiers

The different Test-Editor languages (tcl, tml, config, aml) make use of identifiers that are (transitively) compiled into Java identifiers. In order to parse those accurately, the corresponding parsing rules need to be adjusted to the UTF-8 characters that should be usable. Be sure to allow only characters that satisfy `Character.isUnicodeIdentifierPart`, otherwise Java compiler errors might occur.

locations:
- tcl/org.testeditor.tcl.dsl/src/org/testeditor/tcl/dsl/Tcl.xtext, terminal rule 'ID' (currently at line 261)
- aml/org.testeditor.aml.dsl/src/org/testeditor/aml/dsl/Aml.xtext, terminal rule 'ID' (currently at line 148)
- aml/org.testeditor.aml.dsl/src/org/testeditor/aml/dsl/validation/AmlValidator.xtend, ID_REGEX definition (currently at line 207)
