package org.testeditor.rcp4.views.tcltestrun.model

import com.google.common.io.Files
import java.io.File
import java.nio.charset.StandardCharsets
import org.junit.Before
import org.junit.Rule
import org.junit.rules.TemporaryFolder
import org.mockito.InjectMocks
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest
import org.testeditor.rcp4.views.tcltestrun.LogLocationHelper

import static org.mockito.Mockito.*

abstract class AbstractTestExecutionManagerTest extends AbstractTest {

	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder

	@InjectMocks protected TestExecutionManager testExecutionManager
	@Mock protected LogLocationHelper logLocationHelper

	@Before
	def void initMocks() {
		when(logLocationHelper.logLocation).thenReturn(tempFolder.root)
	}

	protected def File createTestRunSummary(String folderName) {
		val folder = tempFolder.newFolder(folderName)
		val file = new File(folder, "testSummary.xml")
		Files.write(testSummaryContent, file, StandardCharsets.UTF_8)
		return file
	}

	protected def String getTestSummaryContent() '''
		<?xml version="1.0" encoding="UTF-8" standalone="no"?>
			<testrun errors="0" failures="0" ignored="0" name="java" project="org.testeditor.rcp4.uatests" started="1" tests="1">
		</testrun>
	'''

}
