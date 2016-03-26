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
package org.testeditor.dsl.common.ui.wizards

import org.eclipse.jface.wizard.WizardPage
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.List
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.events.SelectionEvent

class TestProjectConfigurationWizardPage extends WizardPage {

	Combo buildSystem

	List availableFixtures

	List selectedFixtures

	Button demoCode

	protected new(String pageName) {
		super(pageName)
		description = "Specify initial configuration of the project (build-system, fixture type and templates)"
		title = "Project configuration"
	}

	override createControl(Composite parent) {
		val cmp = new Composite(parent, SWT.NORMAL)
		cmp.setLayout(new GridLayout(3, false))
		val cmpBs = new Composite(cmp, SWT.NORMAL);
		cmpBs.layout = new GridLayout(2, false)
		val gd = new GridData(GridData.FILL_HORIZONTAL)
		gd.horizontalSpan = 3
		cmpBs.layoutData = gd
		new Label(cmpBs, SWT.NORMAL).text = "Build-system:"
		buildSystem = new Combo(cmpBs, SWT.None);
		buildSystem.add("Maven");
		buildSystem.add("Gradle");
		buildSystem.text = "Maven"
		new Label(cmp, SWT.NORMAL).text = "Available Fixtures:"
		new Composite(cmp, SWT.NONE)
		new Label(cmp, SWT.NORMAL).text = "Selected Fixtures:"
		availableFixtures = new List(cmp, SWT.BORDER)
		availableFixtures.layoutData = new GridData(GridData.FILL_BOTH)
		availableFixtures.add("Web Fixture");
		val buttonArea = new Composite(cmp, SWT.NORMAL)
		buttonArea.layout = new GridLayout(1, false)
		val addBtn = new Button(buttonArea, SWT.BORDER)
		addBtn.text = ">"
		addBtn.layoutData = new GridData(GridData.FILL_HORIZONTAL)
		val delBtn = new Button(buttonArea, SWT.BORDER)
		delBtn.text = "<"
		delBtn.layoutData = new GridData(GridData.FILL_HORIZONTAL)
		selectedFixtures = new List(cmp, SWT.BORDER)
		selectedFixtures.layoutData = new GridData(GridData.FILL_BOTH)
		addBtn.addSelectionListener(getMoveListener(availableFixtures, selectedFixtures))
		delBtn.addSelectionListener(getMoveListener(selectedFixtures, availableFixtures))
		demoCode = new Button(cmp, SWT.CHECK)
		demoCode.text = "Generate with examples"
		setControl(cmp);
	}

	def String[] getSelectedFixtures() {
		return selectedFixtures.items
	}

	def String getBuildSystemName() {
		return buildSystem.text
	}

	def boolean withDemoCode() {
		return demoCode.selection
	}

	def getMoveListener(List sourceList, List destList) {
		new SelectionListener() {

			override widgetDefaultSelected(SelectionEvent e) {
				val sel = sourceList.selection
				for (fixtureName : sel) {
					destList.add(fixtureName)
					sourceList.remove(fixtureName)
				}
			}

			override widgetSelected(SelectionEvent e) {
				widgetDefaultSelected(e)
			}

		}
	}

}
