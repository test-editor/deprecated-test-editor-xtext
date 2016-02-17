package org.testeditor.rcp4.views.tests.tree;

import com.google.common.base.Objects;
import com.google.common.collect.Iterables;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import javax.inject.Inject;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.hamcrest.CoreMatchers;
import org.hamcrest.Matcher;
import org.junit.Assert;
import org.junit.Test;
import org.testeditor.aml.AmlModel;
import org.testeditor.aml.Component;
import org.testeditor.aml.ComponentElement;
import org.testeditor.aml.InteractionType;
import org.testeditor.aml.ModelElement;
import org.testeditor.rcp4.views.AmlModelTreeAdapter;
import org.testeditor.rcp4.views.tests.tree.AbstractParserTest;

@SuppressWarnings("all")
public class AmlModelTreeAdapterTest extends AbstractParserTest {
  @Inject
  private AmlModelTreeAdapter amlModelTreeAdapter;
  
  @Test
  public void amlModelHasComponentsInTree() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("package pa");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("component MyApp { }");
      _builder.newLine();
      _builder.append("component MyApp2 { }");
      _builder.newLine();
      final String input = _builder.toString();
      final AmlModel model = this.parser.parse(input);
      List<EObject> _children = this.amlModelTreeAdapter.children(model);
      final Set<EObject> children = IterableExtensions.<EObject>toSet(_children);
      EList<Component> _components = model.getComponents();
      Set<Component> _set = IterableExtensions.<Component>toSet(_components);
      Matcher<Set<? extends EObject>> _is = CoreMatchers.<Set<? extends EObject>>is(_set);
      Assert.<Set<EObject>>assertThat(children, _is);
      int _size = children.size();
      Matcher<Integer> _is_1 = CoreMatchers.<Integer>is(Integer.valueOf(2));
      Assert.<Integer>assertThat(Integer.valueOf(_size), _is_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void componentElementHasInteractionsInTree() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("package pa");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("component MyApp {");
      _builder.newLine();
      _builder.append(" \t");
      _builder.append("element MyButton is Button { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("element type Button {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("interactions = push, release");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("interaction type push { }");
      _builder.newLine();
      _builder.newLine();
      _builder.append("interaction type release { }");
      _builder.newLine();
      final String input = _builder.toString();
      final AmlModel model = this.parser.parse(input);
      EList<Component> _components = model.getComponents();
      final Function1<Component, Boolean> _function = (Component it) -> {
        String _name = it.getName();
        return Boolean.valueOf(Objects.equal(_name, "MyApp"));
      };
      final Component app = IterableExtensions.<Component>findFirst(_components, _function);
      EList<ComponentElement> _elements = app.getElements();
      final Function1<ComponentElement, Boolean> _function_1 = (ComponentElement it) -> {
        String _name = it.getName();
        return Boolean.valueOf(Objects.equal(_name, "MyButton"));
      };
      ComponentElement _findFirst = IterableExtensions.<ComponentElement>findFirst(_elements, _function_1);
      List<EObject> _children = this.amlModelTreeAdapter.children(_findFirst);
      final Set<EObject> children = IterableExtensions.<EObject>toSet(_children);
      EList<InteractionType> _interactionTypes = model.getInteractionTypes();
      Set<InteractionType> _set = IterableExtensions.<InteractionType>toSet(_interactionTypes);
      Matcher<Set<? extends EObject>> _is = CoreMatchers.<Set<? extends EObject>>is(_set);
      Assert.<Set<EObject>>assertThat(children, _is);
      int _size = children.size();
      Matcher<Integer> _is_1 = CoreMatchers.<Integer>is(Integer.valueOf(2));
      Assert.<Integer>assertThat(Integer.valueOf(_size), _is_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void componentHasOwnAndInheritedInheritedInteractionsInTree() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("package pa");
      _builder.newLine();
      _builder.newLine();
      _builder.append("component other is KillApplication { ");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("element MyInheritedButton is Button { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("component MyApp is Application includes other { ");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("element MyButton is Button { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("component type Application {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("interactions = open, close");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("component type KillApplication {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("interactions = kill");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("element type Button {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("interactions = push, release");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("interaction type open { }");
      _builder.newLine();
      _builder.newLine();
      _builder.append("interaction type close { }");
      _builder.newLine();
      _builder.newLine();
      _builder.append("interaction type kill {\t}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("interaction type push { }");
      _builder.newLine();
      _builder.newLine();
      _builder.append("interaction type release { }");
      _builder.newLine();
      final String input = _builder.toString();
      final AmlModel model = this.parser.parse(input);
      EList<Component> _components = model.getComponents();
      final Function1<Component, Boolean> _function = (Component it) -> {
        String _name = it.getName();
        return Boolean.valueOf(Objects.equal(_name, "MyApp"));
      };
      Component _findFirst = IterableExtensions.<Component>findFirst(_components, _function);
      List<EObject> _children = this.amlModelTreeAdapter.children(_findFirst);
      final Set<EObject> children = IterableExtensions.<EObject>toSet(_children);
      EList<InteractionType> _interactionTypes = model.getInteractionTypes();
      final Function1<InteractionType, Boolean> _function_1 = (InteractionType it) -> {
        String _name = it.getName();
        return Boolean.valueOf(Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("open", "close", "kill")).contains(_name));
      };
      final Iterable<InteractionType> expectedInteractions = IterableExtensions.<InteractionType>filter(_interactionTypes, _function_1);
      EList<Component> _components_1 = model.getComponents();
      final Function1<Component, EList<ComponentElement>> _function_2 = (Component it) -> {
        return it.getElements();
      };
      List<EList<ComponentElement>> _map = ListExtensions.<Component, EList<ComponentElement>>map(_components_1, _function_2);
      final Iterable<ComponentElement> expectedElements = Iterables.<ComponentElement>concat(_map);
      Iterable<ModelElement> _plus = Iterables.<ModelElement>concat(expectedInteractions, expectedElements);
      Set<ModelElement> _set = IterableExtensions.<ModelElement>toSet(_plus);
      Matcher<Set<? extends EObject>> _is = CoreMatchers.<Set<? extends EObject>>is(_set);
      Assert.<Set<EObject>>assertThat(children, _is);
      int _size = children.size();
      Matcher<Integer> _is_1 = CoreMatchers.<Integer>is(Integer.valueOf(5));
      Assert.<Integer>assertThat(Integer.valueOf(_size), _is_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
