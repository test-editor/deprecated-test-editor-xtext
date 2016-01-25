package org.testeditor.rcp4.views

import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.SWT
import javax.inject.Inject
import javax.annotation.PostConstruct
import javax.annotation.PreDestroy

class MaskStepSelector {
	
	@Inject
 	new() {
		System.out.println("created")		
	}
	
	@PostConstruct
	def void postConstruct(Composite parent) {		
      val label = new Label(parent, SWT.NONE);
      label.setText("Hello World!");
	}
	
	@PreDestroy
	def void preDestroy(){
		System.out.println("destroyed")
	}
	
}