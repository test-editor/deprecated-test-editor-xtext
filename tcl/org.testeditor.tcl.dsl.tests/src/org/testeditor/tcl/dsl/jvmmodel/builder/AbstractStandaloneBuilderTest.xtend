package org.testeditor.tcl.dsl.jvmmodel.builder

import com.google.common.io.Files
import com.google.inject.Module
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.List
import com.google.inject.Inject
import org.eclipse.xtext.ISetup
import org.eclipse.xtext.builder.standalone.IIssueHandler
import org.eclipse.xtext.builder.standalone.IIssueHandler.DefaultIssueHandler
import org.eclipse.xtext.builder.standalone.ILanguageConfiguration
import org.eclipse.xtext.builder.standalone.LanguageAccessFactory
import org.eclipse.xtext.builder.standalone.StandaloneBuilder
import org.eclipse.xtext.builder.standalone.compiler.EclipseJavaCompiler
import org.eclipse.xtext.builder.standalone.compiler.IJavaCompiler
import org.eclipse.xtext.generator.AbstractFileSystemAccess
import org.eclipse.xtext.generator.OutputConfigurationProvider
import org.eclipse.xtext.junit4.TemporaryFolder
import org.eclipse.xtext.xbase.compiler.RegisteringFileSystemAccess
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.testeditor.aml.dsl.AmlStandaloneSetup
import org.testeditor.tcl.dsl.TclStandaloneSetup
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tsl.dsl.TslStandaloneSetup

/**
 * Super class for standalone builder tests. 
 */
abstract class AbstractStandaloneBuilderTest extends AbstractTclTest {

	@Rule public TemporaryFolder folder = new TemporaryFolder()

	@Inject protected StandaloneBuilder builder
	@Inject LanguageAccessFactory languageAccessFactory
	@Inject OutputConfigurationProvider configurationProvider

	protected File srcFolder
	protected List<String> classPathEntries

	override protected collectModules(List<Module> modules) {
		super.collectModules(modules)
		modules += [ binder |
			binder.bind(AbstractFileSystemAccess).to(RegisteringFileSystemAccess).asEagerSingleton
			binder.bind(IJavaCompiler).to(EclipseJavaCompiler)
			binder.bind(IIssueHandler).to(DefaultIssueHandler)
		]
	}

	@Before
	def void setup() {
		// configure languages
		val languages = languageAccessFactory.createLanguageAccess(#[
			createLanguageConfiguration(AmlStandaloneSetup),
			createLanguageConfiguration(TclStandaloneSetup),
			createLanguageConfiguration(TslStandaloneSetup)
		], class.classLoader)
		builder.languages = languages

		// configure source folders
		srcFolder = folder.newFolder("src")
		builder.baseDir = folder.root.toString
		builder.sourceDirs = #[srcFolder.absolutePath]
		
		// handle class path entries
		classPathEntries = newLinkedList
		classPathEntries += Test.classPathEntry
		builder.classPathEntries = classPathEntries
	}
	
	protected def String getClassPathEntry(Class<?> clazz) {
		return clazz.protectionDomain.codeSource.location.toURI.path
	}

	private def ILanguageConfiguration createLanguageConfiguration(Class<? extends ISetup> setupClass) {
		return new ILanguageConfiguration() {

			override getOutputConfigurations() {
				configurationProvider.getOutputConfigurations()
			}

			override getSetup() {
				return setupClass.name
			}

			override isJavaSupport() {
				return setupClass == TclStandaloneSetup
			}

		}
	}

	protected def File getFile(String fileName) {
		val parts = fileName.split("/")
		val path = Paths.get(folder.root.absolutePath, parts)
		return path.toFile
	}

	protected def File writeFile(String filename, CharSequence input) {
		val file = getFile(filename)
		Files.createParentDirs(file)
		file.createNewFile
		Files.write(input, file, StandardCharsets.UTF_8)
		return file
	}

	protected def String readFile(String filename) {
		val file = getFile(filename)
		assertTrue(file.exists, '''File with filename='«filename»' does not exist. Full path='«file»'.''')
		return Files.toString(file, StandardCharsets.UTF_8)
	}

	protected def String removeJavaDoc(String contents) {
		return contents.replaceAll('''\/\*\*(.|\r|\n)*?\*\/(\r?\n)?''', '')
	}

}
