package org.testeditor.rcp4.views.tests.tree;

import com.google.common.collect.Iterables;
import java.util.Collections;
import javax.inject.Inject;
import org.eclipse.emf.common.util.EList;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.hamcrest.CoreMatchers;
import org.hamcrest.Matcher;
import org.junit.Assert;
import org.junit.Test;
import org.testeditor.aml.AmlFactory;
import org.testeditor.aml.AmlModel;
import org.testeditor.aml.Component;
import org.testeditor.aml.ComponentElement;
import org.testeditor.aml.InteractionType;
import org.testeditor.aml.Template;
import org.testeditor.aml.TemplateContent;
import org.testeditor.aml.TemplateText;
import org.testeditor.aml.TemplateVariable;
import org.testeditor.rcp4.views.AmlModelLabelProvider;
import org.testeditor.rcp4.views.tests.tree.AbstractParserTest;

@SuppressWarnings("all")
public class AmlModelLabelProviderTest extends AbstractParserTest {
  @Inject
  private AmlModelLabelProvider amlModelLabelProvider;
  
  private AmlFactory factory = AmlFactory.eINSTANCE;
  
  @Test
  public void labelForModelIsPackage() {
    final AmlModel amlModel = this.factory.createAmlModel();
    amlModel.setPackage("test");
    final String result = this.amlModelLabelProvider.getText(amlModel);
    Matcher<String> _is = CoreMatchers.<String>is("test");
    Assert.<String>assertThat(result, _is);
  }
  
  @Test
  public void labelForComponentIsName() {
    final Component component = this.factory.createComponent();
    component.setName("test");
    final String result = this.amlModelLabelProvider.getText(component);
    Matcher<String> _is = CoreMatchers.<String>is("test");
    Assert.<String>assertThat(result, _is);
  }
  
  @Test
  public void labelForComponentElementIsName() {
    final ComponentElement componentElement = this.factory.createComponentElement();
    componentElement.setName("test");
    final String result = this.amlModelLabelProvider.getText(componentElement);
    Matcher<String> _is = CoreMatchers.<String>is("test");
    Assert.<String>assertThat(result, _is);
  }
  
  @Test
  public void labelForInteractionTypesIsTheTemplateString() {
    final InteractionType interactionType = this.factory.createInteractionType();
    Template _createTemplate = this.factory.createTemplate();
    interactionType.setTemplate(_createTemplate);
    Template _template = interactionType.getTemplate();
    EList<TemplateContent> _contents = _template.getContents();
    final Procedure1<TemplateText> _function = (TemplateText it) -> {
      it.setValue("Insert Text");
    };
    TemplateText _newTemplateText = this.newTemplateText(_function);
    final Procedure1<TemplateVariable> _function_1 = (TemplateVariable it) -> {
      it.setName("text");
    };
    TemplateVariable _newTemplateVariable = this.newTemplateVariable(_function_1);
    final Procedure1<TemplateText> _function_2 = (TemplateText it) -> {
      it.setValue("into");
    };
    TemplateText _newTemplateText_1 = this.newTemplateText(_function_2);
    final Procedure1<TemplateVariable> _function_3 = (TemplateVariable it) -> {
      it.setName("element");
    };
    TemplateVariable _newTemplateVariable_1 = this.newTemplateVariable(_function_3);
    Iterables.<TemplateContent>addAll(_contents, Collections.<TemplateContent>unmodifiableList(CollectionLiterals.<TemplateContent>newArrayList(_newTemplateText, _newTemplateVariable, _newTemplateText_1, _newTemplateVariable_1)));
    final String result = this.amlModelLabelProvider.getText(interactionType);
    Matcher<String> _is = CoreMatchers.<String>is("Insert Text \"text\" into <element>");
    Assert.<String>assertThat(result, _is);
  }
  
  private TemplateText newTemplateText(final Procedure1<TemplateText> init) {
    final TemplateText result = this.factory.createTemplateText();
    init.apply(result);
    return result;
  }
  
  private TemplateVariable newTemplateVariable(final Procedure1<TemplateVariable> init) {
    final TemplateVariable result = this.factory.createTemplateVariable();
    init.apply(result);
    return result;
  }
}
