package org.testeditor.tcl.dsl.ide

import com.google.inject.Module
import java.util.HashSet
import java.util.List
import java.util.Set
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import javax.inject.Inject
import javax.inject.Provider
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistEntry
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor
import org.eclipse.xtext.ide.editor.contentassist.antlr.ContentAssistContextFactory
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.testing.util.ParseHelper
import org.eclipse.xtext.util.ITextRegion
import org.eclipse.xtext.util.TextRegion
import org.junit.AfterClass
import org.junit.Assert
import org.junit.BeforeClass
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

/**
 * base class for content assist tests
 */
class AbstractContentAssistTest extends AbstractParserTest {

	static val CARET_CHAR = '|'

	static ExecutorService contextFactoryExecutorService

	@Inject Provider<ContentAssistContextFactory> contextFactoryProvider
	@Inject TclContentProposalProvider proposalProvider
	@Inject protected ParseHelper<TclModel> tclParseHelper // make sure to use this parse helper, not the one that is provided by DslParseHelper

	@BeforeClass
	static def void initContextFactoryExecutorService() {
		contextFactoryExecutorService = Executors.newCachedThreadPool
	}

	@AfterClass
	static def void disposeContextFactoryExecutorService() {
		contextFactoryExecutorService.shutdown()
	}

	override protected collectModules(List<Module> modules) {
		super.collectModules(modules)
		modules += new TclIdeModule as Module
	}

	/**
	 * Provides proposals for the given input.
	 * The cursor location shall be identified by the {@code CARET_CHAR}.
	 */
	def Set<Pair<Integer, ContentAssistEntry>> getProposalsWithPriority(String text, String caretChar) {
		val cursor = text.indexOf(caretChar)
		if (cursor === -1) {
			throw new IllegalArgumentException('''No cursor position set, specfiy it by marking it with caret-char='«CARET_CHAR»'.''')
		}
		val selection = new TextRegion(cursor, 0)

		val textToParse = text.replaceAll('\\' + caretChar, '')
		// make sure to use the injected parse helper instead of the one provided by DslParseHelper
		// using the the DslParseHelper here will result in Rules injected by different injectors which fails upon building the contexts
		var TclModel model = tclParseHelper.parse(textToParse, resourceSet)

		val resource = model.eResource as XtextResource

		val contexts = resource.getContexts(selection, cursor)

		// prepare result list and acceptor
		val proposals = new HashSet<Pair<Integer, ContentAssistEntry>>
		val acceptor = new IIdeContentProposalAcceptor {

			override accept(ContentAssistEntry entry, int priority) {
				proposals.add(priority -> entry)
			}

			override canAcceptMoreProposals() {
				true
			}

		}

		proposalProvider.createProposals(contexts, acceptor)
		return proposals
	}

	def List<ContentAssistEntry> getProposals(String text) {
		return getProposals(text, CARET_CHAR)
	}

	def List<ContentAssistEntry> getProposals(String text, String caretChar) {
		return getProposalsWithPriority(text, caretChar).map[value].toList
	}

	def void reject(Iterable<ContentAssistEntry> proposals, String unexpectedProposal) {
		val entry = proposals.filterNull.findFirst[proposal == unexpectedProposal]
		if (entry !== null) {
			Assert.fail('''
				Expected NO proposal with text="«unexpectedProposal»", but got:
					«entry»
			''')
		}
	}

	def void expectOnly(Iterable<ContentAssistEntry> proposals, Iterable<String> expectedProposals) {
		val unfoundExpectedProposals = expectedProposals.filter[expectedProposal|proposals.filterNull.forall[proposal != expectedProposal]].filterNull
		val unexpectedProposals = proposals.filter[proposal|expectedProposals.forall[proposal?.proposal != it]].filterNull
		if (!unfoundExpectedProposals.isEmpty || !unexpectedProposals.isEmpty) {
			Assert.fail('''
				«IF !unfoundExpectedProposals.isEmpty»Unfound expected proposals with texts=«unfoundExpectedProposals.map['''"«it»"'''].join(', ')»«System.lineSeparator»«ENDIF»Got unexpected:
					«unexpectedProposals.join(System.lineSeparator)»
			''')
		}
	}

	def ContentAssistEntry expect(Iterable<ContentAssistEntry> proposals, String expectedProposal) {
		val entry = proposals.filterNull.findFirst[proposal == expectedProposal]
		if (entry === null) {
			Assert.fail('''
				Expected proposal with text="«expectedProposal»", but got:
					«proposals.join(System.lineSeparator)»
			''')
		}
		return entry
	}

	private def ContentAssistContext[] getContexts(XtextResource resource, ITextRegion selection, int caretOffset) {
		val text = resource.parseResult.rootNode.text
		if (caretOffset > text.length) {
			return #[]
		}
		val contextFactory = contextFactoryProvider.get() => [pool = contextFactoryExecutorService]
		return contextFactory.create(text, selection, caretOffset, resource)
	}

}
