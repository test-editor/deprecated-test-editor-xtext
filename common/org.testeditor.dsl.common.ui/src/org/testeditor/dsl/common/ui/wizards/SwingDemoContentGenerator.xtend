package org.testeditor.dsl.common.ui.wizards

// TODO instead of generating this code we should either check it out from Git or extract a pre-bundled zip
class SwingDemoContentGenerator {
	
	def String getAmlContents() '''
		component GreetingApplication is Application {
			
			element Input is Text {
				locator = "text.input"
			}
			
			element Output is Text {
				locator = "text.output"
			}
			
			element GreetButton is Button {
				locator = "button.greet"
			}
			
		}
	'''

	def String getSwingAmlContents(String packageName) '''
		package «packageName»
		
		import org.testeditor.fixture.swing.SwingFixture
		
		/**
		 * Defines a Swing application.
		 */
		component type Application {
			interactions = start, stop, wait
		}
		
		interaction type start {
			template = "Start application" ${path}
			method = SwingFixture.startApplication(path)
		}
		
		interaction type stop {
			template = "Stop application"
			method = SwingFixture.stopApplication
		}
		
		interaction type wait {
			template = "Wait" ${miliSeconds} "ms"
			method = SwingFixture.waitMilliSeconds(miliSeconds)
		}
		
		/**
		 * Defines a Swing button.
		 */
		element type Button {
			interactions = clickButton
		}
		
		interaction type clickButton {
			template = "Click on" ${element}
			method = SwingFixture.clickButton(element)
		}
		
		/**
		 * Defines a Swing JTextField.
		 */
		element type Text {
			interactions = insertIntoTextField, getTextFromTextField
		}
		
		interaction type insertIntoTextField {
			template = "Insert" ${text} "into field" ${element}
			method = SwingFixture.insertIntoTextField(element, text)
		}
		
		interaction type getTextFromTextField {
			template = "Read text from" ${element}
			method = SwingFixture.getTextFromTextField(element)
		}
	'''
	
	def String getTestCase(String packageName) '''
		# GreetingTest
		 
		* Start the famous greetings application. 
		 
		    Mask: GreetingApplication
		    - Start application "«packageName».GreetingApplication"
		 
		* Send greetings "Hello World" to the world.
		 
		    Mask: GreetingApplication
		    - Insert "Hello World" into field <Input> 
		    - Click on <GreetButton>
		    - Wait "2000" ms
		    - foo = Read text from <Output>
		    - assert foo == "Hello World"
		 
		* Stop the famous greeting application.
		
			Mask: GreetingApplication
			- Stop application
	'''

	def String getSwingApplicationCode(String packageName) '''
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
		package «packageName»;
		
		import java.awt.GridLayout;
		import java.awt.event.ActionEvent;
		import java.awt.event.ActionListener;
		
		import javax.swing.JButton;
		import javax.swing.JFrame;
		import javax.swing.JLabel;
		import javax.swing.JPanel;
		import javax.swing.JTextField;
		import javax.swing.SwingConstants;
		
		public class GreetingApplication extends JFrame {
		
		    private static final long serialVersionUID = 1L;
		
		    public GreetingApplication() {
		        JPanel panel = new JPanel(new GridLayout(2, 1));
		        getContentPane().add(panel);
		
		        // Create input area
		        JPanel inputArea = new JPanel(new GridLayout(1, 3));
		        panel.add(inputArea);
		        inputArea.add(new JLabel("Your greeting:"));
		        final JTextField input = new JTextField("");
		        input.setName("text.input");
		        inputArea.add(input);
		        JButton button = new JButton("Greet");
		        button.setName("button.greet");
		        inputArea.add(button);
		
		        // Create output area
		        JPanel outputArea = new JPanel(new GridLayout(1, 1));
		        panel.add(outputArea);
		        final JTextField greeting = new JTextField("Greet me!");
		        greeting.setName("text.output");
		        greeting.setEnabled(false);
		        greeting.setHorizontalAlignment(SwingConstants.CENTER);
		        outputArea.add(greeting);
		
		        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		        setSize(500, 200);
		        setVisible(true);
		
		        button.addActionListener(new ActionListener() {
		            @Override
		            public void actionPerformed(ActionEvent e) {
		                greeting.setText(input.getText());
		            }
		        });
		    }
		    
		    public static void main(String[] args) {
		        new GreetingApplication();
		    }
		    
		    public String sayHello() {
		    	return "Hello Service";
		    }
		
		}
	'''
	
}