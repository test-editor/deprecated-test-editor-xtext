package org.testeditor.aml.dsl.generator

import org.testeditor.aml.model.AmlModel

class AllActionGroupsGenerator {
	
	def CharSequence generateAllActionGroups(AmlModel model) '''
		<?xml version="1.0" encoding="UTF-8"?>
		<ActionGroups xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://testeditor.org/xsd_schema/v_1_1/AllActionGroups.xsd" schemaVersion="1.1">
		</ActionGroups>
	'''
	
}