package org.testeditor.tcl.dsl.ide;

import com.google.common.base.Objects;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.inject.Inject;
import javax.inject.Provider;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistEntry;
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor;
import org.eclipse.xtext.ide.editor.contentassist.antlr.ContentAssistContextFactory;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.util.ITextRegion;
import org.eclipse.xtext.util.TextRegion;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.testeditor.tcl.TclModel;
import org.testeditor.tcl.dsl.ide.TclContentProposalProvider;
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest;

/**
 * base class for content assist tests
 */
@SuppressWarnings("all")
public class AbstractContentAssistTest extends AbstractParserTest {
  private final static String CARET_CHAR = "$";
  
  private static ExecutorService executorService;
  
  @Inject
  private Provider<ContentAssistContextFactory> contextFactoryProvider;
  
  @Inject
  private TclContentProposalProvider proposalProvider;
  
  @BeforeClass
  public static void initExecutorService() {
    AbstractContentAssistTest.executorService = Executors.newCachedThreadPool();
  }
  
  @AfterClass
  public static void disposeExecutorService() {
    AbstractContentAssistTest.executorService.shutdown();
  }
  
  /**
   * Provides proposals for the given input.
   * The cursor location shall be identified by the {@code CARET_CHAR}.
   */
  public HashSet<Pair<Integer, ContentAssistEntry>> getProposalsWithPriority(final String text) {
    final int cursor = text.indexOf(AbstractContentAssistTest.CARET_CHAR);
    if ((cursor == (-1))) {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("No cursor position set, specfiy it by marking it with caret-char=\'");
      _builder.append(AbstractContentAssistTest.CARET_CHAR);
      _builder.append("\'.");
      throw new IllegalArgumentException(_builder.toString());
    }
    final TextRegion selection = new TextRegion(cursor, 0);
    final String textToParse = text.replaceAll(("\\" + AbstractContentAssistTest.CARET_CHAR), "");
    final TclModel model = this.parserHelper.parseTcl(textToParse);
    Resource _eResource = model.eResource();
    final XtextResource resource = ((XtextResource) _eResource);
    final ContentAssistContext[] contexts = this.getContexts(resource, selection, cursor);
    final HashSet<Pair<Integer, ContentAssistEntry>> proposals = new HashSet<Pair<Integer, ContentAssistEntry>>();
    final IIdeContentProposalAcceptor acceptor = new IIdeContentProposalAcceptor() {
      @Override
      public void accept(final ContentAssistEntry entry, final int priority) {
        Pair<Integer, ContentAssistEntry> _mappedTo = Pair.<Integer, ContentAssistEntry>of(Integer.valueOf(priority), entry);
        proposals.add(_mappedTo);
      }
      
      @Override
      public boolean canAcceptMoreProposals() {
        return true;
      }
    };
    this.proposalProvider.createProposals(((Collection<ContentAssistContext>)Conversions.doWrapArray(contexts)), acceptor);
    return proposals;
  }
  
  public List<ContentAssistEntry> getProposals(final String text) {
    final Function1<Pair<Integer, ContentAssistEntry>, ContentAssistEntry> _function = (Pair<Integer, ContentAssistEntry> it) -> {
      return it.getValue();
    };
    return IterableExtensions.<ContentAssistEntry>toList(IterableExtensions.<Pair<Integer, ContentAssistEntry>, ContentAssistEntry>map(this.getProposalsWithPriority(text), _function));
  }
  
  public ContentAssistEntry expect(final List<ContentAssistEntry> proposals, final String expectedProposal) {
    final Function1<ContentAssistEntry, Boolean> _function = (ContentAssistEntry it) -> {
      String _proposal = it.getProposal();
      return Boolean.valueOf(Objects.equal(_proposal, expectedProposal));
    };
    final ContentAssistEntry entry = IterableExtensions.<ContentAssistEntry>findFirst(proposals, _function);
    if ((entry == null)) {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("Expected proposal with text=\"");
      _builder.append(expectedProposal);
      _builder.append("\", but got:");
      _builder.newLineIfNotEmpty();
      _builder.append("\t");
      String _join = IterableExtensions.join(proposals, System.lineSeparator());
      _builder.append(_join, "\t");
      _builder.newLineIfNotEmpty();
      Assert.fail(_builder.toString());
    }
    return entry;
  }
  
  private ContentAssistContext[] getContexts(final XtextResource resource, final ITextRegion selection, final int caretOffset) {
    final String text = resource.getParseResult().getRootNode().getText();
    int _length = text.length();
    boolean _greaterThan = (caretOffset > _length);
    if (_greaterThan) {
      return new ContentAssistContext[] {};
    }
    ContentAssistContextFactory _get = this.contextFactoryProvider.get();
    final Procedure1<ContentAssistContextFactory> _function = (ContentAssistContextFactory it) -> {
      it.setPool(AbstractContentAssistTest.executorService);
    };
    final ContentAssistContextFactory contextFactory = ObjectExtensions.<ContentAssistContextFactory>operator_doubleArrow(_get, _function);
    return contextFactory.create(text, selection, caretOffset, resource);
  }
}
