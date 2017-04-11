# Injection

## E4 Injection

E4 introduces a set of annotations that control injection. Generally speaking injected objects are fetched from the eclipse context. The
eclipse context is actually a hierarchical structure of contexts which are traversed bottom up in the search for a matching object that will
then be used to construct an object (via c-tor) which in turn will be injected [see here](http://eclipsesource.com/blogs/tutorials/eclipse-4-e4-tutorial-part-4-dependency-injection-basics/).

Available (incomplete) list of annotations:
- @Inject : mark unit that uses injection (constructor, field, method)
- @Named : restrict injected object to specific type
- @Optional : no exception is thrown if no matching object is found for injection
- @Active : inject the currently active (UI) element

Injection order is:
1. constructor + constructor parameters injection
2. field injection
3. methods are injected when called by the framework

## Xtext Guice Injection

[see also](https://eclipse.org/Xtext/documentation/302_configuration.html#dependency-injection)


## Combining E4 and Xtext injection

Both types of injection make use of the same @Inject annotation (javax.inject).

Usually it's a good idea to separate these two injection mechanisms into separate classes. Sometimes this is not possible. In these (rare)
cases injection is done manually. Being within an object that was created by E4 injection, an injector for the dsl must be available.

This injector is created by providing a *creatable* class that create this injector e.g:

```java
	import com.google.inject.Injector
	import org.eclipse.e4.core.di.annotations.Creatable
	import ...ui.internal.DslActivator

	@Creatable
	class MyDslInjectorProvider {
		def Injector get() {
			return DslActivator.instance.getInjector(DslActivator.MY_DSL)
		}
	}
```

```MyDslInjector``` can now be injected using the E4 injection mechanism and can in turn be used to create objects using the xtext injection e.g.

```java
	@Inject MyDslInjectorProvider dslInjectorProvider // e4 injection at work

	@PostConstruct // automatically executed after construction (and initialization) of this component, during c-tor execution fields are not injected yet
	def void postConstruct(Composite parent) {
		val dslInjector = dslInjectorProvider.get
		someInstanceUsingDslInjection = dslInjector.getInstance(SomeClassNeedingDslInjection) // xtext injection at work
	}
```

<!--  LocalWords:  refactored
 -->
