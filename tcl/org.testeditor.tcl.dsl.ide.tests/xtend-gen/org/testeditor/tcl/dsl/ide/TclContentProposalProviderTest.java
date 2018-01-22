package org.testeditor.tcl.dsl.ide;

import java.util.HashSet;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistEntry;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Before;
import org.junit.Test;
import org.testeditor.dsl.common.testing.DummyFixture;
import org.testeditor.tcl.dsl.ide.AbstractContentAssistTest;

@SuppressWarnings("all")
public class TclContentProposalProviderTest extends AbstractContentAssistTest {
  @Before
  public void parseAmlModel() {
    this.parserHelper.parseAml(DummyFixture.getAmlModel());
  }
  
  @Test
  public void testTemplateProposal() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("# SomeTest");
    _builder.newLine();
    _builder.newLine();
    _builder.append("* Do something");
    _builder.newLine();
    _builder.append("  ");
    _builder.append("Component: GreetingApplication");
    _builder.newLine();
    _builder.append("  ");
    _builder.append("- $");
    _builder.newLine();
    final String tclSnippet = _builder.toString();
    final HashSet<Pair<Integer, ContentAssistEntry>> proposals = this.getProposalsWithPriority(tclSnippet);
    Iterable<Pair<Integer, ContentAssistEntry>> _assertSize = this._assertionHelper.<Pair<Integer, ContentAssistEntry>>assertSize(proposals, 3);
    final Procedure1<Iterable<Pair<Integer, ContentAssistEntry>>> _function = (Iterable<Pair<Integer, ContentAssistEntry>> it) -> {
      ContentAssistEntry _value = ((Pair<Integer, ContentAssistEntry>[])Conversions.unwrapArray(it, Pair.class))[0].getValue();
      final Procedure1<ContentAssistEntry> _function_1 = (ContentAssistEntry it_1) -> {
        this._assertionHelper.assertEquals(it_1.getProposal(), "");
      };
      ObjectExtensions.<ContentAssistEntry>operator_doubleArrow(_value, _function_1);
    };
    ObjectExtensions.<Iterable<Pair<Integer, ContentAssistEntry>>>operator_doubleArrow(_assertSize, _function);
  }
}
