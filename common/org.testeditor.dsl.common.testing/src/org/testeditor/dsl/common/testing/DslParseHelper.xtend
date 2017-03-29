package org.testeditor.dsl.common.testing

import com.google.inject.Provider
import java.io.StringReader
import java.util.UUID
import com.google.inject.Inject
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.ParserRule
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.generator.InMemoryFileSystemAccess
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.parser.IParseResult
import org.eclipse.xtext.parser.IParser
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.serializer.ISerializer
import org.testeditor.aml.AmlModel
import org.testeditor.aml.ModelElement
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.dsl.common.testing.xtext.XtextAssertionHelper
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.TclStandaloneSetup
import org.testeditor.tsl.TslModel
import org.testeditor.tsl.dsl.TslStandaloneSetup

class DslParseHelper {
	// the publics are used by several abstract parser test classes (should be read only)
	public Provider<XtextResourceSet> resourceSetProvider
	public XtextResourceSet resourceSet
	public ISerializer tclSerializer
	@Inject public InMemoryFileSystemAccess fsa

	@Inject extension XtextAssertionHelper

	@Inject protected IParser iparser

	protected ParseHelper<AmlModel> amlParseHelper
	protected ParseHelper<TclModel> tclParseHelper
	protected ParseHelper<TslModel> tslParseHelper

	@Inject
	new(Provider<XtextResourceSet> resourceSetProvider) {
		this.resourceSetProvider = resourceSetProvider
		this.resourceSet = resourceSetProvider.get

		// make sure all classes in this classpath are accessible for the resource set (e.g. DummyFixture ...)
		this.resourceSet.classpathURIContext = this

		// register all languages
		val amlInjector = (new AmlStandaloneSetup).createInjectorAndDoEMFRegistration
		amlParseHelper = amlInjector.getInstance(ParseHelper)
		val tclInjector = (new TclStandaloneSetup).createInjectorAndDoEMFRegistration
		tclParseHelper = tclInjector.getInstance(ParseHelper)
		val tslInjector = (new TslStandaloneSetup).createInjectorAndDoEMFRegistration
		tslParseHelper = tslInjector.getInstance(ParseHelper)
		
		// inject serializer(s)
		tclSerializer = tclInjector.getInstance(ISerializer)
	}

	def AmlModel parseAml(CharSequence input) {
		return amlParseHelper.parse(input, resourceSet)
	}

	def AmlModel parseAml(CharSequence input, ResourceSet alternateResourceSet) {
		return amlParseHelper.parse(input, alternateResourceSet)
	}

	/**
	 * Creates a sample model by adding a package definition to the passed input, i.e.
	 * <pre>
	 * 	package com.example
	 * 	«input»
	 * </pre>
	 * and returns the first element of the passed type within the model's eAllContents.
	 * 
	 * @param the partial model to parse
	 * @param elementClass the expected type
	 * @return {@code model.eAllContents.filter(elementClass).head}
	 */
	def <T extends ModelElement> T parseAmlWithStdPackage(CharSequence input, Class<T> elementClass) {
		return parseAmlWithPackage(input, 'com.example', elementClass)
	}

	def <T extends ModelElement> T parseAmlWithUniquePackage(CharSequence input, Class<T> elementClass) {
		val uuid = UUID.randomUUID.toString.replace('-', '')
		return parseAmlWithPackage(input, '''com.example«uuid»''', elementClass)
	}

	def <T extends ModelElement> T parseAmlWithPackage(CharSequence input, String thePackage, Class<T> elementClass) {
		val newInput = '''
			package «thePackage»
			
			«input»
		'''
		val model = amlParseHelper.parse(newInput, resourceSet)
		return model.eAllContents.filter(elementClass).head
	}

	def <T extends EObject> T addToResourceSet(T model) {
		switch (model) {
			TclModel: {
				if (model.macroCollection !== null && model.macroCollection.macros.size > 0) {
					return model.addToResourceSet(model.tclModelName + ".tml")
				} else if (model.test !== null) {
					return model.addToResourceSet(model.tclModelName + ".tcl")
				} else if (model.config !== null) {
					return model.addToResourceSet(model.tclModelName + ".config")
				} else {
					throw new RuntimeException('''tcl model is neither a macroCollection nor a test nor a config''')
				}
			}
			AmlModel:
				return model.addToResourceSet("Dummy.aml")
			TslModel:
				return model.addToResourceSet("Dummy.tsl")
			default:
				throw new RuntimeException('''unknown model='«model.class.name»'.''')
		}
	}

	def <T extends EObject> T addToResourceSet(T model, String fileName) {
		val uri = URI.createURI(fileName)

		val newResource = resourceSet.createResource(uri)
		newResource.contents.add(model)
		return model
	}

	def TclModel parseTcl(String tcl) {
		return tclParseHelper.parse(tcl, resourceSet)
	}

	def TclModel parseTcl(String tcl, String fileName) {
		return tclParseHelper.parse(tcl, URI.createURI(fileName), resourceSet)
	}

	def TslModel parseTsl(String tsl) {
		return tslParseHelper.parse(tsl, resourceSet)
	}

	def TslModel parseTsl(String tsl, String fileName) {
		return tslParseHelper.parse(tsl, URI.createURI(fileName), resourceSet)
	}

	def TclModel parseTcl(String tcl, String fileName, ResourceSet alternateResourceSet) {
		return tclParseHelper.parse(tcl, URI.createURI(fileName), alternateResourceSet)
	}

	def Object getJavaFile(TclModel model) {
		val ^package = model.package
		val name = model.tclModelName
		val key = '''«IFileSystemAccess.DEFAULT_OUTPUT»«package.replaceAll('\\.', '/')»/«name».java'''
		return fsa.allFiles.get(key)
	}

	def <T> T parse(CharSequence input, ParserRule rule, Class<T> ruleClass) {
		return iparser.parse(rule, new StringReader(input.toString)).assertNoSyntaxErrors.getParsedRule(ruleClass)
	}

	def IParseResult partialParse(ParserRule rule, CharSequence input) {
		iparser.parse(rule, new StringReader(input.toString))
	}

	def <T> T getParsedRule(IParseResult parseResult, Class<T> rule) {
		return rule.cast(parseResult.rootASTElement)
	}

	// has duplicate code in TclModelUtil, but since TclModelUtil has references to xtypes and I don't 
	// want this dependency here, this duplication is accepted 
	private def <T extends EObject> String tclModelName(TclModel model) {
		model.macroCollection?.name ?: model.test?.name ?: model.config?.name
	}

}
