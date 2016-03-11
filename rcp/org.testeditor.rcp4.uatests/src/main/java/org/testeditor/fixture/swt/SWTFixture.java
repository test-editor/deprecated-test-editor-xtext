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
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.finders.ContextMenuHelper;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.testeditor.fixture.core.interaction.FixtureMethod;

/**
 * Fixture to control SWT elements in an RCP Application.
 *
 */
public class SWTFixture {

	Logger logger = LogManager.getLogger(SWTFixture.class);
	SWTWorkbenchBot bot = new SWTWorkbenchBot();

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
	 * @throws NumberFormatException
	 *             on not parse able time.
	 * @throws InterruptedException
	 *             on test abort.
	 */
	@FixtureMethod
	public void wait(String seconds) throws NumberFormatException, InterruptedException {
		logger.info("wait for:  {}", seconds);
		Thread.sleep(Long.parseLong(seconds) * 1000);
	}

	/**
	 * Checks that a view in the rcp is active.
	 * 
	 * @param viewName
	 *            to bee looked up.
	 * @return true if the vie is visible
	 */
	@FixtureMethod
	public boolean isViewVisible(String viewName) {
		logger.trace("search for view with titel: {}", viewName);
		SWTBotView view = bot.viewByTitle(viewName);
		return view.isActive();
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
		logger.trace("search for view with titel: {}", locator);
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
		logger.trace("search for view with titel: {}", viewName);
		SWTBotView view = bot.viewByTitle(viewName);
		assertNotNull(view);
		SWTBotTree tree = view.bot().tree();
		assertNotNull(tree);
		MenuItem item = ContextMenuHelper.contextMenu(tree, menuItem.split("/"));
		new SWTBotMenu(item).click();
		logger.trace("Yeaa renne weiter");
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
		logger.trace("search for text with titel: {}", locator);
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
		logger.trace("search for button with titel: {}", locator);
		SWTBotButton button = null;
		if (locator.startsWith("[Label]")) {
			String locatorFragment = getLocatorFragmentFrom(locator);
			logger.trace("Fragment>>>>>>>>>>>>>>>>>> {}", locatorFragment);
			this.reportWidgets();
			button = bot.button(locatorFragment);
		}
		assertNotNull(button);
		button.click();
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
