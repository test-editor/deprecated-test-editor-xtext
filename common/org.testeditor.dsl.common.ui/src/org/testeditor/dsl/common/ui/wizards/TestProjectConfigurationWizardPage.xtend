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
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.List
import org.testeditor.dsl.common.ui.utils.Constants
import org.eclipse.swt.events.SelectionAdapter

class TestProjectConfigurationWizardPage extends WizardPage {

	Combo buildSystem

	List availableFixtures

	List selectedFixtures

	java.util.List<String> availableBuildSystems;

	java.util.List<String> availableFixtureNames;

	Button demoCode

	new(String pageName) {
		super(pageName)
		description = "Specify initial configuration of the project (build-system, fixture type and templates)"
		title = "Project configuration"
	}

	override void createControl(Composite superParent) {
		val parent = new Composite(superParent, SWT.NONE)
		parent.layout = new GridLayout(3, false)
		createBuildSystemSelectionArea(parent)
		createFixtureSelectionArea(parent)
		createDemoSelectionArea(parent)
		setControl(buildSystem);
	}
	
	private def createDemoSelectionArea(Composite parent) {
		demoCode = new Button(parent, SWT.CHECK)
		demoCode.text = "Generate with examples"
	}
	
	private def createFixtureSelectionArea(Composite parent) {
		new Label(parent, SWT.NONE).text = "Available Fixtures:"
		new Composite(parent, SWT.NONE)
		new Label(parent, SWT.NONE).text = "Selected Fixtures:"
		availableFixtures = new List(parent, SWT.BORDER)
		availableFixtures.setData(Constants.SWT_BOT_ID_KEY, Constants.NEW_DIALOG_AVAILABLE_FIXTURE_LIST)
		availableFixtures.layoutData = new GridData(GridData.FILL_BOTH)
		for (fixtureName : availableFixtureNames) {
			availableFixtures.add(fixtureName)
		}
		val buttonArea = new Composite(parent, SWT.NONE)
		buttonArea.layout = new GridLayout(1, false)
		val addBtn = new Button(buttonArea, SWT.BORDER)
		addBtn.setData(Constants.SWT_BOT_ID_KEY, Constants.NEW_DIALOG_ADD_SELECTED_FIXTURE)
		addBtn.text = ">"
		addBtn.layoutData = new GridData(GridData.FILL_HORIZONTAL)
		val delBtn = new Button(buttonArea, SWT.BORDER)
		delBtn.text = "<"
		delBtn.layoutData = new GridData(GridData.FILL_HORIZONTAL)
		selectedFixtures = new List(parent, SWT.BORDER)
		selectedFixtures.layoutData = new GridData(GridData.FILL_BOTH)
		addBtn.addSelectionListener(createMoveListener(availableFixtures, selectedFixtures))
		delBtn.addSelectionListener(createMoveListener(selectedFixtures, availableFixtures))
	}
	
	private def createBuildSystemSelectionArea(Composite superParent) {
		val parent = new Composite(superParent, SWT.NONE);
		parent.layout = new GridLayout(2, false)
		val gd = new GridData(GridData.FILL_HORIZONTAL)
		gd.horizontalSpan = 3
		parent.layoutData = gd
		new Label(parent, SWT.NORMAL).text = "Build-system:"
		buildSystem = new Combo(parent, SWT.None)
		for (systems : availableBuildSystems) {
			buildSystem.add(systems)
		}
		if (availableBuildSystems.size > 0) {
			buildSystem.text = availableBuildSystems.get(0)
		}
	}

	def void setAvailableFixtureNames(java.util.List<String> names) {
		availableFixtureNames = names
	}

	def void setAvailableBuildSystems(java.util.List<String> buildSystems) {
		availableBuildSystems = buildSystems
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

	def SelectionListener createMoveListener(List sourceList, List destList) {
		new SelectionAdapter() {

			override widgetSelected(SelectionEvent e) {
				val sel = sourceList.selection
				for (fixtureName : sel) {
					destList.add(fixtureName)
					sourceList.remove(fixtureName)
				}
			}

		}
	}

}
