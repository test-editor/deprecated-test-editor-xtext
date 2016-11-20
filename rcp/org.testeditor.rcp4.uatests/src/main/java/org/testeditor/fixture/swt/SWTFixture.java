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
import static org.junit.Assert.fail;

import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.waits.Conditions;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEclipseEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotEditor;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.ContextMenuHelper;
import org.eclipse.swtbot.swt.finder.utils.SWTBotPreferences;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotCheckBox;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotCombo;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotList;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotMenu;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.eclipse.swtbot.swt.finder.widgets.TimeoutException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testeditor.fixture.core.TestRunListener;
import org.testeditor.fixture.core.TestRunReportable;
import org.testeditor.fixture.core.TestRunReporter;
import org.testeditor.fixture.core.TestRunReporter.Action;
import org.testeditor.fixture.core.TestRunReporter.SemanticUnit;
import org.testeditor.fixture.core.interaction.FixtureMethod;

/**
 * Fixture to control SWT elements in an RCP Application.
 *
 */
public class SWTFixture implements TestRunListener, TestRunReportable {

	private static final Logger logger = LoggerFactory.getLogger(SWTFixture.class);
	private static final int SCREENSHOT_FILENAME_MAXLEN = 128;

	private SWTWorkbenchBot bot = new SWTWorkbenchBot();
	private String runningTest = null;

	public SWTFixture() {
		System.setProperty("org.eclipse.swtbot.search.timeout", "10000");
		System.setProperty("org.eclipse.swtbot.playback.poll.delay", "10000");
	}
	
	public void initWithReporter(TestRunReporter reporter) {
		reporter.addListener(this);
	}

	@Override
	public void reported(SemanticUnit unit, Action action, String msg) {
		if (unit == SemanticUnit.TEST && action == Action.ENTER) {
			runningTest = msg;
		}
		if (screenshotShouldBeMade(unit, action, msg)) {
			screenshot(msg + '.' + action.name());
		}
	}

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
		waitForPopupDialogClosingWithTimeout(title, swtBotDefaultInMilliSeconds / 2000,
				swtBotDefaultInMilliSeconds / 1000);
	}

	private String getCurrentTestCase() {
		return runningTest != null ? runningTest : "UNKNOWN_TEST";
	}

	private String getScreenshotPath() {
		// configurable through maven build?
		return "screenshots";
	}

	private boolean screenshotShouldBeMade(SemanticUnit unit, Action action, String msg) {
		// configurable through maven build?
		return (action == Action.ENTER) || unit == SemanticUnit.TEST;
	}
	
	private String reduceToMaxLen(String base, int maxLen){
		if (base.length() < maxLen) {
			return base;
		} else {
			return base.substring(0, maxLen);
		}
	}

	private String constructScreenshotFilename(String filenameBase, String testcase) {
		String additionalGraphicType = ".png";
		String escapedBaseName = filenameBase.replaceAll("[^a-zA-Z0-9.-]", "_").replaceAll("_+", "_")
				.replaceAll("_+\\.", ".").replaceAll("\\._+", ".");
		String timeStr = new SimpleDateFormat("HHmmss.SSS").format(new Date());
		StringBuffer finalFilenameBuffer = new StringBuffer();
		int lenOfFixedElements = timeStr.length() + additionalGraphicType.length() + 1/* hyphen */;
		finalFilenameBuffer //
				.append(getScreenshotPath()) //
				.append('/').append(reduceToMaxLen(testcase, SCREENSHOT_FILENAME_MAXLEN))//
				.append('/').append(timeStr).append('-')
				.append(reduceToMaxLen(escapedBaseName, SCREENSHOT_FILENAME_MAXLEN - lenOfFixedElements))//
				.append(additionalGraphicType);
		return finalFilenameBuffer.toString();
	}

	/**
	 * Write a screenshot of the current ui into a file, based on the basic
	 * filenameBase provided. The final filename is constructed using the
	 * testcase a hash of the fixture itself and a shortened timestamp.
	 * 
	 * @param filenameBase
	 *            user definable part of the final filename
	 */
	@FixtureMethod
	public void screenshot(String filenameBase) {
		String testcase = getCurrentTestCase();
		String finalFilename = constructScreenshotFilename(filenameBase, testcase);
		new SWTWorkbenchBot().captureScreenshot(finalFilename);
		logger.info("Wrote screenshot to file='{}'.", finalFilename);
	}

	/**
	 * Waits for a dialog with a given title to show up.
	 * 
	 * @param title
	 *            the title of the dialog
	 * @param openingTimeout
	 *            the time to wait for the window to open
	 * @param closingTimeout
	 *            the time to wait for the window to close, once it opened up
	 */
	@FixtureMethod
	public void waitForPopupDialogClosingWithTimeout(String title, long openingTimeout, long closingTimeout) {
		logger.info("Waiting for dialog with title='{}' to open, timeout='{}' seconds.", title, closingTimeout);
		try {
			try {
				bot.waitUntil(Conditions.shellIsActive(title), openingTimeout * 1000);
				SWTBotShell shell = bot.shell(title);
				bot.waitUntil(Conditions.shellCloses(shell), closingTimeout * 1000);
			} catch (TimeoutException e) {
				logger.info("Dialog with title {} was not found. Test continuous.", title);
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
	public boolean isViewVisible(String locator, ViewLocatorStrategy locatorStrategy) {
		SWTBotView view = getView(locator, locatorStrategy);
		return view.isActive();
	}

	private SWTBotView getView(String locator, ViewLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case ID:
			logger.debug("Searching for view with id='{}'.", locator);
			return bot.viewById(locator);
		case PARTNAME:
			logger.debug("Searching for view with partName='{}'.", locator);
			return bot.viewByPartName(locator);
		case TITLE:
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
	public void selectElementInTreeView(String itemName, String locator, SWTLocatorStrategy locatorStrategy) {
		logger.trace("search for view with title: {} to get the default tree", locator);
		SWTBotTree tree = getTree(locator, locatorStrategy);
		logger.trace("Open item with path: {}", itemName);
		try {
			SWTBotTreeItem expandNode = tree.expandNode(itemName.split("/"));
			expandNode.select();
		} catch (WidgetNotFoundException e) {
			logger.error("Widget not found", e);
			printTreeItems(tree, locator);
			throw e;
		}
	}

	private SWTBotTree getTree(String locator, SWTLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case ID:
			return bot.viewById(locator).bot().tree();
		case LABEL:
			return bot.viewByTitle(locator).bot().tree();
		case SINGLE:
			return bot.tree();
		}
		throw new IllegalArgumentException("Unkown locatorStrategy: " + locatorStrategy);
	}

	private void printTreeItems(SWTBotTree tree, String locator) {
		logger.info("Printing all items of tree with locator='{}'.", locator);
		printTreeItems(tree.getAllItems(), 0);
	}

	private void printTreeItems(SWTBotTreeItem[] items, int level) {
		String spaces = StringUtils.repeat(" ", 4 * level);
		for (SWTBotTreeItem item : items) {
			logger.info("{}|-- {}", spaces, item.getText());
			if (!item.isExpanded()) {
				item.expand();
			}
			printTreeItems(item.getItems(), level + 1);
		}
	}

	@FixtureMethod
	public void selectElementInList(String itemName, String locator, SWTLocatorStrategy locatorStrategy) {
		logger.trace("search for list with {}", locator);
		SWTBotList list = getList(locator, locatorStrategy);
		list.select(itemName);
	}

	private SWTBotList getList(String locator, SWTLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case ID:
			return bot.listWithId(locator);
		case LABEL:
			return bot.listWithLabel(locator);
		case SINGLE:
			return bot.list();
		}
		throw new IllegalArgumentException("Unkown locatorStrategy: " + locatorStrategy);
	}

	@FixtureMethod
	public void selectElementInCombobox(String value, String locator, SWTLocatorStrategy locatorStrategy) {
		logger.trace("search for dropdown with {}", locator);
		SWTBotCombo comboBox = getComboBox(locator, locatorStrategy);
		comboBox.setText(value);
	}

	private SWTBotCombo getComboBox(String locator, SWTLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case ID:
			return bot.comboBoxWithId(locator);
		case LABEL:
			return bot.comboBoxWithLabel(locator);
		case SINGLE:
			return bot.comboBox();
		}
		throw new IllegalArgumentException("Unkown locatorStrategy: " + locatorStrategy);
	}

	/**
	 * Opens the context Menu of a widget and executes the menuitem described as
	 * a path.
	 * 
	 * @param viewName
	 *            id of the view / widget with the context menu
	 * @param menuItem
	 *            path to the menu item Example: "New/Project"
	 * @throws Exception
	 *             on failure to break the test.
	 */
	@FixtureMethod
	public void executeContextMenuEntry(String menuItem, String viewName, SWTLocatorStrategy locatorStrategy)
			throws Exception {
		logger.trace("Search for view with title: {}", viewName);
		SWTBotTree tree = getTree(viewName, locatorStrategy);
		assertNotNull(tree);
		MenuItem item = null;
		try {
			item = ContextMenuHelper.contextMenu(tree, menuItem.split("/"));
		} catch (WidgetNotFoundException e) {
			Thread.sleep(100);
			logger.trace("Search menu entry a second time: {}", menuItem);
			item = ContextMenuHelper.contextMenu(tree, menuItem.split("/"));
		}
		assertNotNull(item);
		logger.trace("Click on menu item: {}", menuItem);
		new SWTBotMenu(item).click();
	}

	/**
	 * Looking for a widget in that is type able and is identifiable by the
	 * locator.
	 * 
	 * @param locator
	 *            of the widget.
	 * @param value
	 *            set to the widget.
	 * @param locatorStrategy
	 *            strategy to lookup the widget.
	 */
	@FixtureMethod
	public void typeInto(String value, String locator, SWTLocatorStrategy locatorStrategy) {
		logger.trace("search for text with title: {}", locator);
		SWTBotText text = getText(locator, locatorStrategy);
		text.setText(value);
	}

	private SWTBotText getText(String locator, SWTLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case ID:
			return bot.textWithId(locator);
		case LABEL:
			return bot.textWithLabel(locator);
		case SINGLE:
			return bot.text();
		}
		throw new IllegalArgumentException("Unkown locatorStrategy: " + locatorStrategy);
	}

	/**
	 * Clicks on a button.
	 * 
	 * @param locator
	 *            to identify the button.
	 */
	@FixtureMethod
	public void clickOn(String locator, SWTLocatorStrategy locatorStrategy) {
		logger.trace("search for button with: {}", locator);
		try {
			SWTBotButton button = getButton(locator, locatorStrategy);
			button.click();
		} catch (WidgetNotFoundException e) {
			logger.info(e.getLocalizedMessage(), e);
			this.reportWidgets();
			fail();
		}
	}

	private SWTBotButton getButton(String locator, SWTLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case ID:
			return bot.buttonWithId(locator);
		case LABEL:
			return bot.button(locator);
		case SINGLE:
			return bot.button();
		}
		throw new IllegalArgumentException("Unkown locatorStrategy: " + locatorStrategy);
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

	// TODO would be nicer to use the generic withId(...) but we need Harmcrest
	// on the classpath for that
	public SWTBotCheckBox getCheckBox(String locator, SWTLocatorStrategy locatorStrategy) {
		switch (locatorStrategy) {
		case ID:
			return bot.checkBoxWithId(locator);
		case LABEL:
			return bot.checkBoxWithLabel(locator);
		case SINGLE:
			return bot.checkBox(locator);
		}
		throw new IllegalArgumentException("Unkown locatorStrategy: " + locatorStrategy);
	}

	private String condenseForLogging(String largeString) {
		String trimmedLargeString = largeString.trim();
		int length = trimmedLargeString.length();
		if (length < 50) {
			return trimmedLargeString;
		}
		return trimmedLargeString.substring(0, 20) + "..." + trimmedLargeString.substring(length - 21, length - 1);
	}

	@FixtureMethod
	public boolean containsActiveTextEditorContent(String searchString) {
		SWTBotEditor activeEditor = bot.activeEditor();
		logger.info("Check if the current active editor '{}' contains '{}'", activeEditor.getTitle(), searchString);
		String textEditorContents = activeEditor.toTextEditor().getText();
		String condensedContents = condenseForLogging(textEditorContents);
		boolean result = textEditorContents.contains(searchString);
		logger.debug("Editor contains '{}' is: {}.", condensedContents, result);
		return result;
	}

	@FixtureMethod
	public void removeLineFromEditor(int lineNumber) {
		SWTBotEditor activeEditor = bot.activeEditor();
		logger.info("Removing line '{}' from  editor '{}'.", lineNumber, activeEditor.getTitle());
		SWTBotEclipseEditor textEditor = activeEditor.toTextEditor();
		String contents = textEditor.getText();
		logger.debug("Editor contains '{}' before.", condenseForLogging(contents));
		String newContent = removeLineFromString(contents, lineNumber);
		textEditor.setText(newContent);
		String condensedContentsAfter = condenseForLogging(textEditor.getText());
		logger.debug("Editor contains '{}' after.", condensedContentsAfter);
	}

	private String removeLineFromString(String string, int lineNumber) {
		String[] contentLines = string.replaceAll("\r", "").split("\n");
		logger.debug("Content contains '{}' lines, now removing line '{}'.", contentLines.length, lineNumber);
		if (lineNumber < 1 || lineNumber > contentLines.length) {
			throw new IllegalArgumentException(
					"Linenumber='" + lineNumber + "' outside available lines='" + contentLines.length + "'.");
		}
		StringBuffer newContent = new StringBuffer();
		for (int i = 0; i < contentLines.length; i++) {
			if (i != lineNumber - 1) {
				if (newContent.length() > 0) {
					newContent.append(System.lineSeparator());
				}
				newContent.append(contentLines[i]);
			}
		}
		return newContent.toString();
	}

	@FixtureMethod
	public void saveActiveEditor() {
		SWTBotEditor activeEditor = bot.activeEditor();
		String textEditorContents = activeEditor.toTextEditor().getText();
		String condensedContents = condenseForLogging(textEditorContents);
		logger.debug("Editor contains '{}'.", condensedContents);
		logger.info("Save editor {}", activeEditor.getTitle());
		activeEditor.toTextEditor().save();
	}

	@FixtureMethod
	public void typeTextIntoActiveEditor(String text) {
		SWTBotEditor activeEditor = bot.activeEditor();
		logger.info("Type text: {} into editor {}", text, activeEditor.getTitle());
		activeEditor.toTextEditor().typeText(text);
	}

	@FixtureMethod
	public void goToLineInActiveEditor(int lineNumber) {
		SWTBotEditor activeEditor = bot.activeEditor();
		logger.info("Go to line: {} into editor {}", lineNumber, activeEditor.getTitle());
		activeEditor.toTextEditor().navigateTo(lineNumber, 0);
	}

	@FixtureMethod
	public void activateAutocompletion(String text, String selectedProposal) {
		SWTBotEditor activeEditor = bot.activeEditor();
		logger.info("Activate autocompletion on editor {}", activeEditor.getTitle());
		activeEditor.toTextEditor().autoCompleteProposal(text, selectedProposal);
	}

}
