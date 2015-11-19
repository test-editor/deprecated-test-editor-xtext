package org.testeditor.tcl.dsl.ui.util

import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.jface.text.Region
import org.eclipse.xtext.nodemodel.INode
import org.eclipse.xtext.nodemodel.util.NodeModelUtils

/**
 * Helper class to create a {@link Region} for a list of nodes.
 */
class NodeRegionUtil {

	static def Region findNodesRegionForFeature(EObject object, EStructuralFeature feature) {
		val nodes = NodeModelUtils.findNodesForFeature(object, feature)
		if (!nodes.empty) {
			return nodes.region
		} else {
			return null
		}
	}

	static def Region getRegion(List<INode> nodes) {
		val first = nodes.head
		val last = nodes.last
		val length = last.offset - first.offset + last.length
		new Region(first.offset, length)
	}

}