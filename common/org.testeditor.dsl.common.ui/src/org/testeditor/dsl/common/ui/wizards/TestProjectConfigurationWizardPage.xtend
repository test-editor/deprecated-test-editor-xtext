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
import org.eclipse.swt.events.MouseAdapter
import org.eclipse.swt.events.MouseEvent
import org.eclipse.swt.events.MouseListener
import org.eclipse.swt.events.SelectionAdapter
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

class TestProjectConfigurationWizardPage extends WizardPage {

	Combo buildSystem

	List availableFixturesList

	List selectedFixturesList

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
		demoCode = new Button(parent, SWT.CHECK) => [
			text = "Generate with examples"
			setData(Constants.SWT_BOT_ID_KEY, Constants.NEW_DIALOG_GENERATE_WITH_EXAMPLES)
		]
	}

	private def createFixtureSelectionArea(Composite parent) {
		new Label(parent, SWT.NONE).text = "Available Fixtures:"
		new Composite(parent, SWT.NONE)
		new Label(parent, SWT.NONE).text = "Selected Fixtures:"
		availableFixturesList = new List(parent, SWT.BORDER) => [
			setData(Constants.SWT_BOT_ID_KEY, Constants.NEW_DIALOG_AVAILABLE_FIXTURE_LIST)
			layoutData = new GridData(GridData.FILL_BOTH)
		]
		availableFixtureNames.forEach[availableFixturesList.add(it)]
		val buttonArea = new Composite(parent, SWT.NONE)
		buttonArea.layout = new GridLayout(1, false)
		val rbutton = new Button(buttonArea, SWT.BORDER) => [
			setData(Constants.SWT_BOT_ID_KEY, Constants.NEW_DIALOG_ADD_SELECTED_FIXTURE)
			text = ">"
			layoutData = new GridData(GridData.FILL_HORIZONTAL)
		]
		val lbutton = new Button(buttonArea, SWT.BORDER) => [
			text = "<"
			layoutData = new GridData(GridData.FILL_HORIZONTAL)
		]
		selectedFixturesList = new List(parent, SWT.BORDER) => [
			layoutData = new GridData(GridData.FILL_BOTH)
		]

		selectedFixturesList.addMouseListener(createDoubleClickListener(selectedFixturesList, availableFixturesList))
		availableFixturesList.addMouseListener(createDoubleClickListener(availableFixturesList, selectedFixturesList))
		rbutton.addSelectionListener(createMoveListener(availableFixturesList, selectedFixturesList))
		lbutton.addSelectionListener(createMoveListener(selectedFixturesList, availableFixturesList))
	}

	private def createBuildSystemSelectionArea(Composite superParent) {
		val parent = new Composite(superParent, SWT.NONE);
		parent.layout = new GridLayout(2, false)
		val gd = new GridData(GridData.FILL_HORIZONTAL)
		gd.horizontalSpan = 3
		parent.layoutData = gd
		new Label(parent, SWT.NORMAL).text = "Build-system:"
		buildSystem = new Combo(parent, SWT.None)
		availableBuildSystems.forEach[buildSystem.add(it)]
		buildSystem.text = availableBuildSystems.head
	}

	def void setAvailableFixtureNames(java.util.List<String> names) {
		availableFixtureNames = names
	}

	def void setAvailableBuildSystems(java.util.List<String> buildSystems) {
		availableBuildSystems = buildSystems
	}

	def String[] getSelectedFixtures() {
		return selectedFixturesList.items
	}

	def String getBuildSystemName() {
		return buildSystem.text
	}

	def boolean withDemoCode() {
		return demoCode.selection
	}

	override getNextPage() {
		return null
	}

	def void moveSelection(List from, List to) {
		from.selection.forEach [
			to.add(it)
			from.remove(it)
		]
	}

	private def MouseListener createDoubleClickListener(List from, List to) {
		return new MouseAdapter() {

			override mouseDoubleClick(MouseEvent e) {
				moveSelection(from, to)
				container.updateButtons
			}

		}
	}

	private def SelectionListener createMoveListener(List sourceList, List destList) {
		return new SelectionAdapter() {

			override widgetSelected(SelectionEvent e) {
				moveSelection(sourceList, destList)
				container.updateButtons
			}

		}
	}

}
