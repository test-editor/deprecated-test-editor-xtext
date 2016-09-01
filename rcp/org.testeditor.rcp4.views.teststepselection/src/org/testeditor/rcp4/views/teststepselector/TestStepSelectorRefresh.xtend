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
 *******************************************************************************/package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.eclipse.e4.core.di.annotations.Execute
import org.eclipse.e4.core.services.events.IEventBroker

/** e4 command implementation for refreshing the tree view (e.g. aml files/model changed) */
class TestStepSelectorRefresh {
	@Inject
	var IEventBroker broker

	@Execute
	def void execute() {
		broker.post(TestStepSelector.SELECTOR_TOPIC_UPDATE, null)
	}
}
