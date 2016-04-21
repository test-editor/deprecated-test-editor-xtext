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
package org.testeditor.dsl.common.ide.util

import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.util.ITextRegion
import org.eclipse.xtext.util.TextRegion

/**
 * Helper class to create a {@link ITextRegion} for a list of nodes.
 */
class NodeRegionUtil {

	def ITextRegion findNodesRegionForFeature(EObject object, EStructuralFeature feature) {
		val nodes = NodeModelUtils.findNodesForFeature(object, feature)
		if (!nodes.empty) {
			return nodes.region
		} else {
			return null
		}
	}

	def ITextRegion getRegion(List<INode> nodes) {
		val first = nodes.head
		val last = nodes.last
		val length = last.offset - first.offset + last.length
		new TextRegion(first.offset, length)
	}

}