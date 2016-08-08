/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.fixture.swt;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;
import static org.junit.Assert.assertNotNull;

import java.lang.reflect.Method;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.ContextMenuHelper;
import org.eclipse.swtbot.swt.finder.utils.SWTBotPreferences;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotCheckBox;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotList;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.testeditor.fixture.core.interaction.FixtureMethod;

/**
 * Fixture to control SWT elements in an RCP Application.
 *
 */
public class SWTFixture {

	private Logger logger = LogManager.getLogger(SWTFixture.class);
	private SWTWorkbenchBot bot = new SWTWorkbenchBot();

	/**
	 * Method for debugging the AUT. A first implementation of an widget
	 * scanner. all widget s on current SWT-App will be printed with their id's
	 * in the swtbot.log with the trace-level. This Method is in work and can be
	 * extended and modified step by step.
	 * 
	 */
	@FixtureMethod
	public void reportWidgets() {
		logger.info("analyzeWidgets start");
		logger.info("---------------------------------------------");

		getDisplay().syncExec(new Runnable() {
			@Override
			public void run() {

				List<? extends Widget> widgets = bot.widgets(widgetOfType(Widget.class));

				StringBuilder sb = new StringBuilder();

				for (Widget widget : widgets) {

					if (widget instanceof Table) {
						sb.append("\n >>> Table gefunden mit " + ((Table) widget).getItems().length + " Zeilen !");
					}

					sb.append("widgetId: " + widget.getData("org.eclipse.swtbot.widget.key"));
					sb.append(" widgetClass: " + widget.getClass().getSimpleName());
					try {
						Method[] methods = widget.getClass().getMethods();
						boolean foundGetText = false;
						for (Method method : methods) {
							if (method.getName().equals("getText")) {
								foundGetText = true;
							}
						}
						if (foundGetText) {

							Method method = widget.getClass().getMethod("getText", new Class[] {});
							sb.append("\n >>> text value: " + method.invoke(widget, new Object[] {}));
						}
					} catch (Exception e) {
						logger.error("No text", e);
					}
					sb.append(" widget: " + widget).append("\n");
				}

				logger.info(sb.toString());

			}
		});
		logger.info("analyzeWidgets end");
		logger.info("---------------------------------------------");
	}

	/**
	 * 
	 * @return a SWT Display object.
	 */
	protected Display getDisplay() {
		return Display.getDefault();
	}

	/**
	 * Wait Step to inteerupt the test execution for a defined time.
	 * 
	 * @param seconds
	 *            to wait until continue test.
	 * @throws InterruptedException
	 *             on test abort.
	 */
	@FixtureMethod
	public void wait(int seconds) throws InterruptedException {
		logger.info("Waiting for {} seconds.", seconds);
		Thread.sleep(seconds * 1000);
	}

	/**
	 * Waits for a dialog with a given title to show up.
	 * 
	 * @param title
	 *            the title of the dialog
	 */
	@FixtureMethod
	public void waitForDialogClosing(String title) {
		long swtBotDefaultInMilliSeconds = SWTBotPreferences.TIMEOUT;
		waitForDialogClosingWithTimeout(title, swtBotDefaultInMilliSeconds / 1000);
	}

	/**
	 * Waits for a dialog with a given title to show up.
	 * 
	 * @param title
	 *            the title of the dialog
	 * @param timeout
	 *            the time to wait
	 */
	@FixtureMethod
	public void waitForDialogClosingWithTimeout(String title, long timeout) {
		logger.info("Waiting for dialog with title='{}' to open, timeout='{}' seconds.", title, timeout);
		try {
			SWTBotShell shell = bot.shell(title);
			if (shell.isActive()) {
				bot.waitUntil(Conditions.shellCloses(shell), timeout * 1000);
			}
		} catch (WidgetNotFoundException e) {
			logger.info("Widget not found. No reason to wait.");
		}
		logger.info("Finished waiting.");
	}

	/**
	 * Checks that a view in the RCP is active.
	 * 
	 * @param locator
	 *            to locator of the view to be looked up.
	 * @param locatorStrategy
	 *            the strategy that shall be applied
	 * @return true if the vie is visible
	 */
	@FixtureMethod
	public boolean isViewVisible(String locator, SWTBotViewLocatorStrategy locatorStrategy) {
		SWTBotView view = getView(locator, locatorStrategy);
		return view.isActive();
	}

	private SWTBotView getView(String locator, SWTBotViewLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case VIEW_ID:
			logger.debug("Searching for view with id='{}'.", locator);
			return bot.viewById(locator);
		case VIEW_PARTNAME:
			logger.debug("Searching for view with partName='{}'.", locator);
			return bot.viewByPartName(locator);
		case VIEW_TITLE:
			logger.debug("Searching for view with title='{}'.", locator);
			return bot.viewByTitle(locator);
		default:
			throw new IllegalArgumentException("Unknown locator strategy: " + locatorStrategy);
		}
	}

	/**
	 * Selects an element in a tree. It takes a path string end tries to expand
	 * the tree along the path. The node of the path is selectd.
	 * 
	 * @param locator
	 *            of the tree.
	 * @param itemName
	 *            path to the item to be selectd. Example:
	 *            "Root/SubNode/FinalNode"
	 */
	@FixtureMethod
	public void selectElementInTreeView(String locator, String itemName) {
		logger.trace("search for view with title: {}", locator);
		SWTBotTree tree = null;
		if (locator.startsWith("[Single]")) {
			tree = bot.tree();
		} else {
			SWTBotView view = bot.viewByTitle(locator);
			tree = view.bot().tree();
		}
		assertNotNull(tree);
		logger.trace("Open item with path: {}", itemName);
		SWTBotTreeItem expandNode = tree.expandNode(itemName.split("/"));
		assertNotNull(expandNode);
		expandNode.select();
	}

	@FixtureMethod
	public void selectElementInList(String locator, String itemName) {
		logger.trace("search for list with {}", locator);
		if (locator.startsWith("[ID]")) {
			SWTBotList list = bot.listWithId(getLocatorFragmentFrom(locator));
			list.select(itemName);
		}
	}

	/**
	 * Opens the context Menu of a widget and executes the menuitem described as
	 * a path.
	 * 
	 * @param viewName
	 *            id of the view / widget with the context menu
	 * @param menuItem
	 *            poath to the menuitem Example: "New/Project"
	 * @throws Exception
	 *             on failure to break the test.
	 */
	@FixtureMethod
	public void executeContextMenuEntry(String viewName, String menuItem) throws Exception {
		logger.trace("search for view with title: {}", viewName);
		SWTBotView view = bot.viewByTitle(viewName);
		assertNotNull(view);
		SWTBotTree tree = view.bot().tree();
		assertNotNull(tree);
		MenuItem item = ContextMenuHelper.contextMenu(tree, menuItem.split("/"));
		assertNotNull(item);
		new SWTBotMenu(item).click();
	}

	/**
	 * Looking for a widget in that is typeable and is identifiable by the
	 * locator.
	 * 
	 * @param locator
	 *            of the widget.
	 * @param value
	 *            set to the widget.
	 */
	@FixtureMethod
	public void typeInto(String locator, String value) {
		logger.trace("search for text with title: {}", locator);
		SWTBotText text = null;
		if (locator.startsWith("[Single]")) {
			text = bot.text();
		}
		text.setText(value);
	}

	/**
	 * Clicks on a button.
	 * 
	 * @param locator
	 *            to identify the button.
	 */
	@FixtureMethod
	public void clickOn(String locator) {
		logger.trace("search for button with title: {}", locator);
		SWTBotButton button = null;
		if (locator.startsWith("[Label]")) {
			String locatorFragment = getLocatorFragmentFrom(locator);
			button = bot.button(locatorFragment);
		}
		if (locator.startsWith("[ID]")) {
			String locatorFragment = getLocatorFragmentFrom(locator);
			try {
				button = bot.buttonWithId(locatorFragment);
			} catch (WidgetNotFoundException e) {
				logger.info(e.getLocalizedMessage(), e);
				this.reportWidgets();
			}
		}
		assertNotNull(button);
		button.click();
	}

	/**
	 * Checks an SWT checkbox.
	 * 
	 * @param locator
	 *            to identify the checkbox
	 * @param locatorStrategy
	 *            the strategy to use
	 */
	@FixtureMethod
	public void check(String locator, SWTLocatorStrategy locatorStrategy) {
		SWTBotCheckBox checkBox = getCheckBox(locator, locatorStrategy);
		checkBox.select();
	}

	/**
	 * Unchecks an SWT checkbox.
	 * 
	 * @param locator
	 *            to identify the checkbox
	 * @param locatorStrategy
	 *            the strategy to use
	 */
	@FixtureMethod
	public void uncheck(String locator, SWTLocatorStrategy locatorStrategy) {
		SWTBotCheckBox checkBox = getCheckBox(locator, locatorStrategy);
		checkBox.deselect();
	}

	// TODO would be nicer to use the generic withId(...) but we need Harmcrest on the classpath for that
	public SWTBotCheckBox getCheckBox(String locator, SWTLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case ID:
			return bot.checkBoxWithId(locator);
		case LABEL: 
			return bot.checkBoxWithLabel(locator);
		}
		throw new IllegalArgumentException("Unkown locatorStrategy: " + locatorStrategy);
	}

	/**
	 * Extracts the locator fragment from a locator string type.
	 * 
	 * @param locator
	 *            string with type.
	 * @return locator fragment without type.
	 */
	private String getLocatorFragmentFrom(String locator) {
		return locator.substring(locator.indexOf(']') + 1);
	}

}
