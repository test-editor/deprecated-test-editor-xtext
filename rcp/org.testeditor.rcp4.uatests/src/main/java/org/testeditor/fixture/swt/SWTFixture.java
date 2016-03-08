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

import java.lang.reflect.Method;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
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
		logger.trace("search for view with name: {}", viewName);
		SWTBotView view = bot.viewByPartName(viewName);
		return view.isActive();
	}

}
